#!/usr/local/bin/R
#rm(list=ls(all=T)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 26/11/2019
# This script removes outliers.
# GitHub repo: https://github.com/atsukotominaga/teaching-v2.0/tree/master/script/R 

####################################
#  Requirements
####################################
# set working directory to file source location
# install and load required packages
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}

# setting
# ggplots
theme_set(theme_classic())

# create necessary folders if not exist
# filtered - all of the outputs will be stored in this folder
if (!file.exists("trimmed")){
  dir.create("trimmed")
}

# read a text file for ideal performance
dt_ideal <- fread("./ideal.txt")
colnames(dt_ideal) <- "Pitch"
dt_ideal$RowNr <- c(1:nrow(dt_ideal))
dt_ideal <- dt_ideal[c(2, 1)]

# read txt files
dt_onset_all <- fread(file = "./filtered/dt_correct_onset.txt", header = T)
dt_offset_all <- fread(file= "./filtered/dt_correct_offset.txt", header = T)

# assign RowNr
dt_onset_all$RowNr <- rep(1:72, nrow(dt_onset_all)/72)
dt_offset_all$RowNr <- rep(1:72, nrow(dt_offset_all)/72)

# check whether each trial has both onset/offset datasets
onset_trials <- dt_onset_all[, .(N = .N), by = .(SubNr, BlockNr, TrialNr)]
offset_trials <- dt_offset_all[, .(N = .N), by = .(SubNr, BlockNr, TrialNr)]
valid_trials <- rbind(onset_trials, offset_trials)
valid_trials$Duplicate <- duplicated(valid_trials)
valid_trials_included <- valid_trials[Duplicate == TRUE]
# 1 trial was excluded
write(sprintf("KOT: %i trial was excluded because it lacks either onset or offset dataset", nrow(onset_trials)-nrow(valid_trials_included)), file = "./trimmed/outlier.txt", append = T)
print(sprintf("KOT: %i trial was excluded because it lacks either onset or offset dataset", nrow(onset_trials)-nrow(valid_trials_included)))

dt_kot_onset_all <- data.table()
for (row in 1:nrow(valid_trials_included)){
  current <- dt_onset_all[SubNr == valid_trials_included$SubNr[row] & BlockNr == valid_trials_included$BlockNr[row] & TrialNr == valid_trials_included$TrialNr[row]]
  dt_kot_onset_all <- rbind(dt_kot_onset_all, current)
}

dt_kot_offset_all <- data.table()
for (row in 1:nrow(valid_trials_included)){
  current <- dt_offset_all[SubNr == valid_trials_included$SubNr[row] & BlockNr == valid_trials_included$BlockNr[row] & TrialNr == valid_trials_included$TrialNr[row]]
  dt_kot_offset_all <- rbind(dt_kot_offset_all, current)
}

# sort by SubNr, BlockNr, TrialNr and NoteNr
dt_onset <- dt_kot_onset_all[order(SubNr, BlockNr, TrialNr, NoteNr)]
dt_offset <- dt_kot_offset_all[order(SubNr, BlockNr, TrialNr, NoteNr)]

####################################
# Define Subcomponents
####################################
# for intervals
ls_legato <- list(c(1:4), c(9:16), c(21:24), c(40:46))
ls_staccato <- list(c(6:7), c(18:19), c(29:31), c(36:38), c(48:50), c(53:55), c(58:64))
ls_forte <- list(c(1:4), c(9:16), c(21:24), c(40:46))
ls_piano <- list(c(6:7), c(18:19), c(29:31), c(36:38), c(48:50), c(53:55), c(58:64))

# for each note (velocity)
ls_legato2 <- list(c(1:5), c(9:17), c(21:26), c(40:47))
ls_staccato2 <- list(c(6:8), c(18:20), c(29:32), c(36:39), c(48:51), c(53:56), c(58:65))
ls_forte2 <- list(c(1:5), c(9:17), c(21:26), c(40:47))
ls_piano2 <- list(c(6:8), c(18:20), c(29:32), c(36:39), c(48:51), c(53:56), c(58:65))

# define Component Change (LtoS, FtoP)
change_1 <- c(5, 17, 47)
# define Component Change (StoL, PtoF)
change_2 <- c(8, 20, 39)

####################################
# Inter-Onset intervals
####################################
# run trimming_ioi.R first
# source("./trimming_ioi.R")

# or read csv
dt_ioi_1 <- fread(file = "./trimmed/data_ioi_1.txt", header = T) # remove outliers outside 3SD across the conditions
dt_ioi_3 <- fread(file = "./trimmed/data_ioi_3.txt", header = T) # remove outliers per Boundary

####################################
# Key Overlap Time - articulation
####################################
dt_kot <- dt_onset

# Offset 1 - Onset 2
dt_kot$KOT <- NA
for (row in 1:nrow(dt_kot)){
  if (row < nrow(dt_kot)){
    dt_kot$KOT[row+1] <- dt_offset$TimeStamp[row] - dt_onset$TimeStamp[row+1] # offset(n) - onset(n+1)
  }
}
# convert bpm to ms
dt_kot[Tempo == 120]$Tempo <- 250
dt_kot[Tempo == 110]$Tempo <- 273
dt_kot[Tempo == 100]$Tempo <- 300

# remove the first note
dt_kot <- dt_kot[RowNr != 1]

# assign a sequence number for each tone
dt_kot$Interval <- rep(1:71, nrow(dt_kot)/71)

# assign Subcomponents
dt_kot$Subcomponent <- "NA"
# Legato
for (phrase in 1:length(ls_legato)){
  for (note in 1:length(ls_legato[[phrase]])){
    dt_kot[Skill == "articulation" & Interval == ls_legato[[phrase]][note]]$Subcomponent <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato)){
  for (note in 1:length(ls_staccato[[phrase]])){
    dt_kot[Skill == "articulation" & Interval == ls_staccato[[phrase]][note]]$Subcomponent <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte)){
  for (note in 1:length(ls_forte[[phrase]])){
    dt_kot[Skill == "dynamics" & Interval == ls_forte[[phrase]][note]]$Subcomponent <- "Forte"
  }
}
# Piano
for (phrase in 1:length(ls_piano)){
  for (note in 1:length(ls_piano[[phrase]])){
    dt_kot[Skill == "dynamics" & Interval == ls_piano[[phrase]][note]]$Subcomponent <- "Piano"
  }
}

# assign Subcomponent Change
for (number in change_1){
  dt_kot[Skill == "articulation" & Interval == number]$Subcomponent <- "LtoS"
  dt_kot[Skill == "dynamics" & Interval == number]$Subcomponent <- "FtoP"
}
for (number in change_2){
  dt_kot[Skill == "articulation" & Interval == number]$Subcomponent <- "StoL"
  dt_kot[Skill == "dynamics" & Interval == number]$Subcomponent <- "PtoF"
}

# add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
dt_kot$Grouping <- "NA"
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    dt_kot[Condition == ls_grouping$Condition[cond] & Skill == ls_grouping$Skill[skill]]$Grouping <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

# compute KOR (Key-Overlap Ratio)
# mean IOI for each trial (dt_ioi_1)
ioi_1 <- dt_ioi_1[, .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = .(SubNr, BlockNr, TrialNr, Condition, Skill)]

# mean IOI for each trial (dt_ioi_3)
ioi_3 <- dt_ioi_3[, .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = .(SubNr, BlockNr, TrialNr, Condition, Skill)]

# Calculate KOR for each interval (ioi_1)
dt_kor_1<- data.frame()
for (subnr in unique(dt_kot$SubNr)){
  for (block in unique(dt_kot[SubNr == subnr]$BlockNr)){
    for (trial in unique(dt_kot[SubNr == subnr & BlockNr == block]$TrialNr)){
      dt_current <- dt_kot[SubNr == subnr & BlockNr == block & TrialNr == trial]
      dt_current$KOR <- dt_current$KOT/ioi_1[SubNr == subnr & BlockNr == block & TrialNr == trial]$Mean
      dt_kor_1 <- rbind(dt_kor_1, dt_current)
    }
  }
}

# Calculate KOR for each interval (ioi_3)
dt_kor_3 <- data.frame()
for (subnr in unique(dt_kot$SubNr)){
  for (block in unique(dt_kot[SubNr == subnr]$BlockNr)){
    for (trial in unique(dt_kot[SubNr == subnr & BlockNr == block]$TrialNr)){
      dt_current <- dt_kot[SubNr == subnr & BlockNr == block & TrialNr == trial]
      dt_current$KOR <- dt_current$KOT/ioi_3[SubNr == subnr & BlockNr == block & TrialNr == trial]$Mean
      dt_kor_3 <- rbind(dt_kor_3, dt_current)
    }
  }
}

####################################
# Remove outliers (KOR)
####################################
# dt_kor_1
# exclude irrelevant notes (Subcomponent == NA means not 8th notes / KOR == NA means a missing value)
dt_kor_subset <- dt_kor_1[Subcomponent != "NA" & !is.na(KOR)]

# draw histogram and boxplot
p_kor_hist <- ggplot(dt_kor_subset, aes(x = KOR, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .05) +
  theme_classic()
plot(p_kor_hist)

p_kor_box <- ggboxplot(dt_kor_subset, x = "Subcomponent", y = "KOR", color = "Condition")
p_kor_box <- ggpar(p_kor_box, ylab = "Key-Overlap Ratio (KOT/meanIOI)")
plot(p_kor_box)

# exclude kor > +- 3SD (per subcomponent)
kor_subcomponent <- dt_kor_subset[, .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = Subcomponent]
dt_kor_trim_sd <- data.table()
for (subcomponent in unique(dt_kor_subset$Subcomponent)){
  upper <- kor_subcomponent[Subcomponent == subcomponent]$Mean+3*kor_subcomponent[Subcomponent == subcomponent]$SD
  lower <- kor_subcomponent[Subcomponent == subcomponent]$Mean-3*kor_subcomponent[Subcomponent == subcomponent]$SD
  dt_current <- dt_kor_subset[Subcomponent == subcomponent & KOR < upper & KOR > lower]
  dt_kor_trim_sd <- rbind(dt_kor_trim_sd, dt_current)
}

removed_kor <- nrow(dt_kor_subset)-nrow(dt_kor_trim_sd)
proportion_kor <- round(removed_kor/nrow(dt_kor_subset), 5)
write(sprintf("KOR: Remove %i responses beyond +- 3SD / %f percent", removed_kor, proportion_kor*100), file = "./trimmed/outlier.txt", append = T)
print(sprintf("KOR: Remove %i responses beyond +- 3SD / %f percent", removed_kor, proportion_kor*100))

# draw histogram and boxplot
p_kor_hist_sd <- ggplot(dt_kor_trim_sd, aes(x = KOR, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .05) +
  theme_classic()
plot(p_kor_hist_sd)

p_kor_box_sd <- ggboxplot(dt_kor_trim_sd, x = "Subcomponent", y = "KOR", color = "Condition")
p_kor_box_sd <- ggpar(p_kor_box_sd, ylab = "Key-Overlap Ratio (KOT/meanIOI)")
plot(p_kor_box_sd)

# Save plots
# png files
ggsave("./trimmed/kor_hist.png", plot = p_kor_hist, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kor_hist_sd.png", plot = p_kor_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kor_box.png", plot = p_kor_box, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kor_box_sd.png", plot = p_kor_box_sd, dpi = 600, width = 5, height = 4)

# Export a txt file for dt_kor_trim_sd
fwrite(dt_kor_trim_sd, file = "./trimmed/data_kor.txt", row.names = F)

####################################
# Remove outliers (KOT)
####################################
# exclude irrelevant notes (Subcomponent == NA means not 8th notes / KOT == NA means a missing value)
dt_kot_subset <- dt_kot[Subcomponent != "NA" & !is.na(KOT)]

# draw histogram and boxplot
p_kot_hist <- ggplot(dt_kot_subset, aes(x = KOT, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()
plot(p_kot_hist)

p_kot_box <- ggboxplot(dt_kot_subset, x = "Subcomponent", y = "KOT", color = "Condition")
p_kot_box <- ggpar(p_kot_box, ylab = "Key-Overlap Time")
plot(p_kot_box)

# exclude kot > +- 3SD (per subcomponent)
kot_subcomponent <- dt_kot_subset[, .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = Subcomponent]
dt_kot_trim_sd <- data.frame()
for (subcomponent in unique(dt_kot_subset$Subcomponent)){
  upper <- kot_subcomponent[Subcomponent == subcomponent]$Mean+3*kot_subcomponent[Subcomponent == subcomponent]$SD
  lower <- kot_subcomponent[Subcomponent == subcomponent]$Mean-3*kot_subcomponent[Subcomponent == subcomponent]$SD
  dt_current <- dt_kot_subset[Subcomponent == subcomponent & KOT < upper & KOT > lower]
  dt_kot_trim_sd <- rbind(dt_kot_trim_sd, dt_current)
}

removed_kot <- nrow(dt_kot_subset)-nrow(dt_kot_trim_sd)
proportion_kot <- round(removed_kot/nrow(dt_kot_subset), 5)
write(sprintf("KOT: Remove %i responses beyond +- 3SD / %f percent", removed_kot, proportion_kot*100), file = "./trimmed/outlier.txt", append = T)
print(sprintf("KOT: Remove %i responses beyond +- 3SD / %f percent", removed_kot, proportion_kot*100))

# draw histogram and boxplot
p_kot_hist_sd <- ggplot(dt_kot_trim_sd, aes(x = KOT, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()
plot(p_kot_hist_sd)

p_kot_box_sd <- ggboxplot(dt_kot_trim_sd, x = "Subcomponent", y = "KOT", color = "Condition")
p_kot_box_sd <- ggpar(p_kot_box_sd, ylab = "Key-Overlap Time")
plot(p_kot_box_sd)

# Save plots
# png files
ggsave("./trimmed/kot_hist.png", plot = p_kot_hist, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kot_hist_sd.png", plot = p_kot_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kot_box.png", plot = p_kot_box, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kot_box_sd.png", plot = p_kot_box_sd, dpi = 600, width = 5, height = 4)

# Export a txt file for dt_kot_trim_sd
fwrite(dt_kot_trim_sd, file = "./trimmed/data_kot.txt", row.names = F)
