#!/usr/local/bin/R
#rm(list=ls(all=T)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 26/11/2019
# This script removes outliers.
# GitHub repo (private): https://github.com/atsukotominaga/teaching-v2.0/tree/master/script/R 

####################################
#  Requirements
####################################
# set working directory to file source location
# install and load required packages
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}
if (!require("wesanderson")) {install.packages("wesanderson"); require("wesanderson")}

# setting
# ggplots
theme_set(theme_classic())

# create necessary folders if not exist
# filtered - all of the outputs will be stored in this folder
if (!file.exists("trimmed")){
  dir.create("trimmed")
}

# read a text file for ideal performance
df_ideal <- read.table("./ideal.txt")
colnames(df_ideal) <- "Pitch"
df_ideal$RowNr <- c(1:nrow(df_ideal))
df_ideal <- df_ideal[c(2, 1)]

# read csv
df_onset <- read.csv(file.path("./filtered/data_correct_onset.csv"), header = T)
df_offset <- read.csv(file.path("./filtered/data_correct_offset.csv"), header = T)

# assign RowNr
df_onset$RowNr <- rep(1:72, nrow(df_onset)/72)
df_offset$RowNr <- rep(1:72, nrow(df_offset)/72)

# sort by SubNr, BlockNr, TrialNr and NoteNr
df_onset <- df_onset[order(df_onset$SubNr, df_onset$BlockNr, df_onset$TrialNr, df_onset$RowNr),]
df_offset <- df_offset[order(df_offset$SubNr, df_offset$BlockNr, df_offset$TrialNr, df_offset$RowNr),]

# read functions
source("./function.R")

# make sure there is no pitch errors - outputs should be only missing trials
print("----- Onset missing trials -----")
ls_removed_onset <- pitch_remover(df_onset, df_ideal)
print("----- Offset missing trials -----")
ls_removed_offset <- pitch_remover(df_offset, df_ideal)

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
df_ioi_1 <- read.csv(file.path("./trimmed/data_ioi_1.csv"), header = T) # remove outliers outside 3SD across the conditions
df_ioi_3 <- read.csv(file.path("./trimmed/data_ioi_3.csv"), header = T) # remove outliers per Boundary

####################################
# Key Overlap Time - articulation
####################################
df_kot <- df_onset

# Offset 1 - Onset 2
df_kot$KOT <- NA
for (row in 1:length(df_kot$NoteNr)){
  if (row < length(df_kot$NoteNr)){
    df_kot$KOT[row+1] <- df_offset$TimeStamp[row] - df_onset$TimeStamp[row+1] # offset(n) - onset(n+1)
  }
}
# convert bpm to ms
df_kot$Tempo[df_kot$Tempo == 120] <- 250
df_kot$Tempo[df_kot$Tempo == 110] <- 273
df_kot$Tempo[df_kot$Tempo == 100] <- 300

# remove the first note
df_kot <- df_kot %>% dplyr::filter(RowNr != 1)

# assign a sequence number for each tone
df_kot$Interval <- rep(1:71, nrow(df_kot)/71)

# assign Subcomponents
df_kot$Subcomponent <- NA
# Legato
for (phrase in 1:length(ls_legato)){
  for (note in 1:length(ls_legato[[phrase]])){
    df_kot$Subcomponent[df_kot$Skill == "articulation" & df_kot$Interval == ls_legato[[phrase]][note]] <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato)){
  for (note in 1:length(ls_staccato[[phrase]])){
    df_kot$Subcomponent[df_kot$Skill == "articulation" & df_kot$Interval == ls_staccato[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte)){
  for (note in 1:length(ls_forte[[phrase]])){
    df_kot$Subcomponent[df_kot$Skill == "dynamics" & df_kot$Interval == ls_forte[[phrase]][note]] <- "Forte"
  }
}
# Piano
for (phrase in 1:length(ls_piano)){
  for (note in 1:length(ls_piano[[phrase]])){
    df_kot$Subcomponent[df_kot$Skill == "dynamics" & df_kot$Interval == ls_piano[[phrase]][note]] <- "Piano"
  }
}

# Assign Subcomponent Change
for (number in change_1){
  df_kot$Subcomponent[df_kot$Skill == "articulation" & df_kot$Interval == number] <- "LtoS"
  df_kot$Subcomponent[df_kot$Skill == "dynamics" & df_kot$Interval == number] <- "FtoP"
}
for (number in change_2){
  df_kot$Subcomponent[df_kot$Skill == "articulation" & df_kot$Interval == number] <- "StoL"
  df_kot$Subcomponent[df_kot$Skill == "dynamics" & df_kot$Interval == number] <- "PtoF"
}

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    df_kot$Grouping[df_kot$Condition == ls_grouping$Condition[cond] & df_kot$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

# compute KOR (Key-Overlap Ratio)
# mean IOI for each trial (df_ioi_1)
ioi_1 <- aggregate(IOI~SubNr*BlockNr*TrialNr*Condition*Skill, data = df_ioi_1,
                   FUN = function(x){c(length(x), mean = mean(x), sd = sd(x))})
ioi_1 <- cbind(ioi_1[,1:5], as.data.frame(ioi_1[,6]))
# Change colnames
colnames(ioi_1) <- c("SubNr", "BlockNr", "TrialNr", "Condition", "Skill", "N", "Mean", "SD")

# mean IOI for each trial (df_ioi_1)
ioi_3 <- aggregate(IOI~SubNr*BlockNr*TrialNr*Condition*Skill, data = df_ioi_3,
                   FUN = function(x){c(length(x), mean = mean(x), sd = sd(x))})
ioi_3 <- cbind(ioi_3[,1:5], as.data.frame(ioi_3[,6]))
# Change colnames
colnames(ioi_3) <- c("SubNr", "BlockNr", "TrialNr", "Condition", "Skill", "N", "Mean", "SD")

# Calculate KOR for each interval (ioi_1)
df_kor_1<- data.frame()
for (subnr in unique(df_kot$SubNr)){
  for (block in unique(df_kot$BlockNr[df_kot$SubNr == subnr])){
    for (trial in unique(df_kot$TrialNr[df_kot$SubNr == subnr & df_kot$BlockNr == block])){
      df_current <- df_kot %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      df_current$KOR <- df_current$KOT/ioi_1$Mean[ioi_1$SubNr == subnr & ioi_1$BlockNr == block & ioi_1$TrialNr == trial]
      df_kor_1 <- rbind(df_kor_1, df_current)
    }
  }
}

# Calculate KOR for each interval (ioi_3)
df_kor_3<- data.frame()
for (subnr in unique(df_kot$SubNr)){
  for (block in unique(df_kot$BlockNr[df_kot$SubNr == subnr])){
    for (trial in unique(df_kot$TrialNr[df_kot$SubNr == subnr & df_kot$BlockNr == block])){
      df_current <- df_kot %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      df_current$KOR <- df_current$KOT/ioi_3$Mean[ioi_3$SubNr == subnr & ioi_3$BlockNr == block & ioi_3$TrialNr == trial]
      df_kor_3 <- rbind(df_kor_3, df_current)
    }
  }
}

####################################
# Remove outliers (KOR)
####################################
# df_kor_3
# exclude irrelevant notes (Subcomponent == NA means not 8th notes / KOR == NA means a missing value)
df_kor_subset <- subset(df_kor_3, !is.na(df_kor_3$Subcomponent) & !is.na(df_kor_3$KOR))

# draw histogram and boxplot
p_kor_hist <- ggplot(df_kor_subset, aes(x = KOR, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .1) +
  theme_classic()
plot(p_kor_hist)

p_kor_box <- ggboxplot(df_kor_subset, x = "Skill", y = "KOR", color = "Condition")
p_kor_box <- ggpar(p_kor_box, ylab = "Key-Overlap Ratio (KOT/meanIOI)")
plot(p_kor_box)

# exclude kor > +- 3SD (within a given condition)
kor_subcomponent <- aggregate(KOR~Subcomponent, data = df_kor_subset,
                              FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
kor_subcomponent <- cbind(kor_subcomponent, as.data.frame(kor_subcomponent[,2]))
df_kor_trim_sd <- data.frame()
for (subcomponent in unique(df_kor_subset$Subcomponent)){
  upper = kor_subcomponent$mean[kor_subcomponent$Subcomponent == subcomponent]+3*kor_subcomponent$sd[kor_subcomponent$Subcomponent == subcomponent]
  lower = kor_subcomponent$mean[kor_subcomponent$Subcomponent == subcomponent]-3*kor_subcomponent$sd[kor_subcomponent$Subcomponent == subcomponent]
  df_current <- df_kor_subset %>% dplyr::filter(Subcomponent == subcomponent & KOR < upper & KOR > lower)
  df_kor_trim_sd <- rbind(df_kor_trim_sd, df_current)
}
removed_kor <- nrow(df_kor_subset)-nrow(df_kor_trim_sd)
proportion_kor <- round(removed_kor/nrow(df_kor_subset), 5)
print(sprintf("KOR: Remove %i responses beyond +- 3SD / %f percent", removed_kor, proportion_kor*100))

# draw histogram and boxplot
p_kor_hist_sd <- ggplot(df_kor_trim_sd, aes(x = KOR, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .05) +
  theme_classic()
plot(p_kor_hist_sd)

p_kor_box_sd <- ggboxplot(df_kor_trim_sd, x = "Skill", y = "KOR", color = "Condition")
p_kor_box_sd <- ggpar(p_kor_box_sd, ylab = "Key-Overlap Ratio (KOT/meanIOI)")
plot(p_kor_box_sd)

# Save plots
# png files
ggsave("./trimmed/kor_hist.png", plot = p_kor_hist, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kor_hist_sd.png", plot = p_kor_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kor_box.png", plot = p_kor_box, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kor_box_sd.png", plot = p_kor_box_sd, dpi = 600, width = 5, height = 4)

# Export a csv file for df_kor_trim_sd
write.csv(df_kor_trim_sd, file = "./trimmed/data_kor.csv", row.names = F)

####################################
# Remove outliers (KOT)
####################################
# exclude irrelevant notes (Subcomponent == NA means not 8th notes / KOT == NA means a missing value)
df_kot_subset <- subset(df_kot, !is.na(df_kot$Subcomponent) & !is.na(df_kot$KOT))

# draw histogram and boxplot
p_kot_hist <- ggplot(df_kot_subset, aes(x = KOT, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 10) +
  theme_classic()
plot(p_kot_hist)

p_kot_box <- ggboxplot(df_kot_subset, x = "Skill", y = "KOT", color = "Condition")
p_kot_box <- ggpar(p_kotbox, ylab = "Key-Overlap Time")
plot(p_kot_box)

# exclude kot > +- 3SD (within a given condition)
kot_subcomponent <- aggregate(KOT~Subcomponent, data = df_kot_subset,
                              FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
kot_subcomponent <- cbind(kot_subcomponent, as.data.frame(kot_subcomponent[,2]))
df_kot_trim_sd <- data.frame()
for (subcomponent in unique(df_kot_subset$Subcomponent)){
  upper = kot_subcomponent$mean[kot_subcomponent$Subcomponent == subcomponent]+3*kot_subcomponent$sd[kot_subcomponent$Subcomponent == subcomponent]
  lower = kot_subcomponent$mean[kot_subcomponent$Subcomponent == subcomponent]-3*kot_subcomponent$sd[kot_subcomponent$Subcomponent == subcomponent]
  df_current <- df_kot_subset %>% dplyr::filter(Subcomponent == subcomponent & KOT < upper & KOT > lower)
  df_kot_trim_sd <- rbind(df_kot_trim_sd, df_current)
}
removed_kot <- nrow(df_kot_subset)-nrow(df_kot_trim_sd)
proportion_kot <- round(removed_kot/nrow(df_kot_subset), 5)
print(sprintf("KOT: Remove %i responses beyond +- 3SD / %f percent", removed_kot, proportion_kot*100))

# draw histogram and boxplot
p_kot_hist_sd <- ggplot(df_kot_trim_sd, aes(x = KOT, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 10) +
  theme_classic()
plot(p_kot_hist_sd)

p_kot_box_sd <- ggboxplot(df_kot_trim_sd, x = "Skill", y = "KOT", color = "Condition")
p_kot_box_sd <- ggpar(p_kot_box_sd, ylab = "Key-Overlap Time")
plot(p_kot_box_sd)

# Save plots
# png files
ggsave("./trimmed/kot_hist.png", plot = p_kot_hist, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kot_hist_sd.png", plot = p_kot_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kot_box.png", plot = p_kot_box, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kot_box_sd.png", plot = p_kot_box_sd, dpi = 600, width = 5, height = 4)

# Export a csv file for df_kot_trim_sd
write.csv(df_kot_trim_sd, file = "./trimmed/data_kot.csv", row.names = F)
