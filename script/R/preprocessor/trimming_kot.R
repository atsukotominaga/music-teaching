#!/usr/local/bin/R
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 25/09/2020
# This script removes outliers.
# GitHub repo (private): https://github.com/atsukotominaga/teaching-v1.0/tree/master/script/R/preprocessor

# install and load required packages
# data manipulation
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
# plot
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}

# setting
# ggplots
theme_set(theme_classic())

# create a folder if not exists
if (!file.exists("trimmed")){
  dir.create("trimmed")
}

# read txt files
dt_onset <- fread(file = "./filtered/dt_correct_onset.txt")
dt_offset <- fread(file = "./filtered/dt_correct_offset.txt")

# sort by SubNr, BlockNr, TrialNr, NoteNrm TimeStamp
dt_onset <- dt_onset[order(SubNr, BlockNr, TrialNr, NoteNr, TimeStamp)]
dt_offset <- dt_offset
dt_offset$TimeStamp <- as.integer(dt_offset$TimeStamp)

# conbine onset and offset
dt_all <- rbind(dt_onset, dt_offset)
dt_all <- dt_all[order(SubNr, BlockNr, TrialNr, NoteNr, TimeStamp)]

####################################
# Define Subcomponents
####################################
# For intervals
ls_legato <- list(c(1:7), c(17:23), c(42:48), c(58:64))
ls_staccato <- list(c(9:15), c(25:31), c(34:40), c(50:56))
ls_forte <- list(c(1:7), c(17:23), c(42:48), c(58:64))
ls_piano <- list(c(9:15), c(25:31), c(34:40), c(50:56))

# For each note (velocity only)
ls_legato_2 <- list(c(1:8), c(17:24), c(42:49), c(58:65))
ls_staccato_2 <- list(c(9:16), c(25:32), c(34:41), c(50:57))
ls_forte_2 <- list(c(1:8), c(17:24), c(42:49), c(58:65))
ls_piano_2 <- list(c(9:16), c(25:32), c(34:41), c(50:57))

# Define Skill Change (LtoS, FtoP)
change_1 <- c(8, 24, 49)
# Define Skill Change (StoL, PtoF)
change_2 <- c(16, 41, 57)

####################################
# Inter-Onset intervals
####################################
# run trimming_ioi.R first
# source("./trimming_ioi.R")

# or read txt files for IOIs
dt_ioi_1 <- fread(file.path("./trimmed/data_ioi_1.txt"), header = T) # remove outliers outside 3SD across the conditions
dt_ioi_3 <- fread(file.path("./trimmed/data_ioi_3.txt"), header = T) # remove outliers per Boundary

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

# remove the first note
dt_kot <- dt_kot[RowNr != 1]

# assign a sequence number for each tone
dt_kot$Interval <- rep(1:66, nrow(dt_kot)/66)

# assign Subcomponents
dt_kot$Subcomponent <- NA
# Legato
for (phrase in 1:length(ls_legato)){
  for (note in 1:length(ls_legato[[phrase]])){
    dt_kot$Subcomponent[dt_kot$Skill == "articulation" & dt_kot$Interval == ls_legato[[phrase]][note]] <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato)){
  for (note in 1:length(ls_staccato[[phrase]])){
    dt_kot$Subcomponent[dt_kot$Skill == "articulation" & dt_kot$Interval == ls_staccato[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte)){
  for (note in 1:length(ls_forte[[phrase]])){
    dt_kot$Subcomponent[dt_kot$Skill == "dynamics" & dt_kot$Interval == ls_forte[[phrase]][note]] <- "Forte"
  }
}
# Piano
for (phrase in 1:length(ls_piano)){
  for (note in 1:length(ls_piano[[phrase]])){
    dt_kot$Subcomponent[dt_kot$Skill == "dynamics" & dt_kot$Interval == ls_piano[[phrase]][note]] <- "Piano"
  }
}

# assign Subcomponent Change
for (number in change_1){
  dt_kot$Subcomponent[dt_kot$Skill == "articulation" & dt_kot$Interval == number] <- "LtoS"
  dt_kot$Subcomponent[dt_kot$Skill == "dynamics" & dt_kot$Interval == number] <- "FtoP"
}
for (number in change_2){
  dt_kot$Subcomponent[dt_kot$Skill == "articulation" & dt_kot$Interval == number] <- "StoL"
  dt_kot$Subcomponent[dt_kot$Skill == "dynamics" & dt_kot$Interval == number] <- "PtoF"
}

# add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    dt_kot$Grouping[dt_kot$Condition == ls_grouping$Condition[cond] & dt_kot$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

# # compute KOR (Key-Overlap Ratio)
# # mean IOI for each trial (dt_ioi_1)
# ioi_1 <- dt_ioi_1[, .(N = .N, mean = mean(IOI), sd = sd(IOI)), by = .(SubNr, BlockNr, TrialNr, Condition, Skill)]
# 
# # Change colnames
# colnames(ioi_1) <- c("SubNr", "BlockNr", "TrialNr", "Condition", "Skill", "N", "Mean", "SD")
# 
# # Calculate KOR for each interval (ioi_1)
# dt_kor_1<- data.frame()
# for (subnr in unique(dt_kot$SubNr)){
#   for (block in unique(dt_kot$BlockNr[dt_kot$SubNr == subnr])){
#     for (trial in unique(dt_kot$TrialNr[dt_kot$SubNr == subnr & dt_kot$BlockNr == block])){
#       dt_current <- dt_kot %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
#       dt_current$KOR <- dt_current$KOT/ioi_1$Mean[ioi_1$SubNr == subnr & ioi_1$BlockNr == block & ioi_1$TrialNr == trial]
#       dt_kor_1 <- rbind(dt_kor_1, dt_current)
#     }
#   }
# }

####################################
# Remove outliers (KOT)
####################################
# exclude irrelevant notes (Subcomponent == NA means not 16th notes / KOT == NA means a missing value)
dt_kot_subset <- subset(dt_kot, !is.na(dt_kot$Subcomponent) & !is.na(dt_kot$KOT))

# draw histogram and boxplot
p_kot_hist <- ggplot(dt_kot_subset, aes(x = KOT, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 10) +
  theme_classic()
plot(p_kot_hist)

p_kot_box <- ggboxplot(dt_kot_subset, x = "Subcomponent", y = "KOT", color = "Condition")
p_kot_box <- ggpar(p_kot_box, ylab = "Key-Overlap Time")
plot(p_kot_box)

# exclude kot > +- 3SD (per subcomponent)
kot_subcomponent <- dt_kot_subset[, .(N = .N, mean = mean(KOT), sd = sd(KOT), sem = sd(KOT)/sqrt(.N)), by = Subcomponent]
dt_kot_trim_sd <- data.table()
for (subcomponent in unique(dt_kot_subset$Subcomponent)){
  upper <- kot_subcomponent$mean[kot_subcomponent$Subcomponent == subcomponent]+3*kot_subcomponent$sd[kot_subcomponent$Subcomponent == subcomponent]
  lower <- kot_subcomponent$mean[kot_subcomponent$Subcomponent == subcomponent]-3*kot_subcomponent$sd[kot_subcomponent$Subcomponent == subcomponent]
  dt_current <- dt_kot_subset %>% dplyr::filter(Subcomponent == subcomponent & KOT < upper & KOT > lower)
  dt_kot_trim_sd <- rbind(dt_kot_trim_sd, dt_current)
}
removed_kot <- nrow(dt_kot_subset)-nrow(dt_kot_trim_sd)
proportion_kot <- round(removed_kot/nrow(dt_kot_subset), 5)
write(sprintf("KOT: Remove %i responses beyond +- 3SD / %f percent", removed_kot, proportion_kot*100), file = "./trimmed/outlier.txt", append = T)
print(sprintf("KOT: Remove %i responses beyond +- 3SD / %f percent", removed_kot, proportion_kot*100))

# draw histogram and boxplot
p_kot_hist_sd <- ggplot(dt_kot_trim_sd, aes(x = KOT, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 10) +
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

# Export a csv file for dt_kot_trim_sd
write.csv(dt_kot_trim_sd, file = "./trimmed/data_kot.csv", row.names = F)
