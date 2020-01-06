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
# calculate IOIs
df_ioi <- df_onset
df_ioi$IOI <- diff(c(0, df_ioi$TimeStamp))
# convert bpm to ms
df_ioi$Tempo[df_ioi$Tempo == 120] <- 250
df_ioi$Tempo[df_ioi$Tempo == 110] <- 273
df_ioi$Tempo[df_ioi$Tempo == 100] <- 300
# normalise IOIs
df_ioi$normIOI <- df_ioi$IOI/df_ioi$Tempo
df_ioi$NoteNr <- rep(1:72, nrow(df_ioi)/72)

# remove the first note
df_ioi <- df_ioi %>% dplyr::filter(NoteNr != 1)

# assign Interval
df_ioi$Interval <- rep(1:71, nrow(df_ioi)/71)

# assign Subcomponents
df_ioi$Subcomponent <- NA
# Legato
for (phrase in 1:length(ls_legato)){
  for (note in 1:length(ls_legato[[phrase]])){
    df_ioi$Subcomponent[df_ioi$Skill == "articulation" & df_ioi$Interval == ls_legato[[phrase]][note]] <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato)){
  for (note in 1:length(ls_staccato[[phrase]])){
    df_ioi$Subcomponent[df_ioi$Skill == "articulation" & df_ioi$Interval == ls_staccato[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte)){
  for (note in 1:length(ls_forte[[phrase]])){
    df_ioi$Subcomponent[df_ioi$Skill == "dynamics" & df_ioi$Interval == ls_forte[[phrase]][note]] <- "Forte"
  }
}
# Piano
for (phrase in 1:length(ls_piano)){
  for (note in 1:length(ls_piano[[phrase]])){
    df_ioi$Subcomponent[df_ioi$Skill == "dynamics" & df_ioi$Interval == ls_piano[[phrase]][note]] <- "Piano"
  }
}

# assign Subcomponent Change
for (number in change_1){
  df_ioi$Subcomponent[df_ioi$Skill == "articulation" & df_ioi$Interval == number] <- "LtoS"
  df_ioi$Subcomponent[df_ioi$Skill == "dynamics" & df_ioi$Interval == number] <- "FtoP"
}
for (number in change_2){
  df_ioi$Subcomponent[df_ioi$Skill == "articulation" & df_ioi$Interval == number] <- "StoL"
  df_ioi$Subcomponent[df_ioi$Skill == "dynamics" & df_ioi$Interval == number] <- "PtoF"
}

# add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    df_ioi$Grouping[df_ioi$Condition == ls_grouping$Condition[cond] & df_ioi$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

####################################
# Remove outliers (3 methods)
####################################
# exclude irrelevant notes (Subcomponent == NA means not 8th notes / normIOI == NA means a missing value)
df_ioi_subset <- subset(df_ioi, !is.na(df_ioi$Subcomponent) & !is.na(df_ioi$normIOI))

# draw histogram and
p_ioi_hist <- ggplot(df_ioi_subset, aes(x = normIOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .01)
plot(p_ioi_hist)

p_ioi_box <- ggboxplot(df_ioi_subset, x = "Skill", y = "normIOI", color = "Condition")
p_ioi_box <- ggpar(p_ioi_box, ylab = "Normalised IOI (IOI/Tempo)")
plot(p_ioi_box)

####################################
# 1. > +- 3SD across the conditions
####################################
# exclude ioi > +- 3SD (across the conditions)
upper_ioi_1 <- mean(df_ioi_subset$normIOI)+3*sd(df_ioi_subset$normIOI)
lower_ioi_1 <- mean(df_ioi_subset$normIOI)-3*sd(df_ioi_subset$normIOI)
df_ioi_trim_sd_1 <- df_ioi_subset %>% dplyr::filter(normIOI < upper_ioi_1 & normIOI > lower_ioi_1)
removed_ioi_1 <- nrow(df_ioi_subset)-nrow(df_ioi_trim_sd_1)
proportion_ioi_1 <- round(removed_ioi_1/nrow(df_ioi_subset), 5)
print(sprintf("(Method 1)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_1, proportion_ioi_1*100))

p_ioi_hist_sd_1 <- ggplot(df_ioi_trim_sd_1, aes(x = normIOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .01)
plot(p_ioi_hist_sd_1)

p_ioi_box_sd_1 <- ggboxplot(df_ioi_trim_sd_1, x = "Skill", y = "normIOI", color = "Condition")
p_ioi_box_sd_1 <- ggpar(p_ioi_box_sd_1, ylab = "Normalised IOI (IOI/Tempo)")
plot(p_ioi_box_sd_1)

# Save plots
# png files
ggsave("./trimmed/ioi_hist.png", plot = p_ioi_hist, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_hist_sd_1.png", plot = p_ioi_hist_sd_1, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_box.png", plot = p_ioi_box, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_box_sd_1.png", plot = p_ioi_box_sd_1, dpi = 600, width = 5, height = 4)

# Export a csv file for df_ioi_trim_sd
write.csv(df_ioi_trim_sd_1, file = "./trimmed/data_ioi_1.csv", row.names = F)

####################################
# 2. > +- 3SD per condition
####################################
# exclude ioi > +- 3SD (per condition)
ioi_skill <- aggregate(normIOI~Grouping, data = df_ioi_subset,
                              FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
ioi_skill <- cbind(ioi_skill, as.data.frame(ioi_skill[,2]))

df_ioi_trim_sd_2 <- data.frame()
for (grouping in unique(ioi_skill$Grouping)){
  upper_ioi_2 = ioi_skill$mean[ioi_skill$Grouping == grouping]+3*ioi_skill$sd[ioi_skill$Grouping == grouping]
  lower_ioi_2 = ioi_skill$mean[ioi_skill$Grouping == grouping]-3*ioi_skill$sd[ioi_skill$Grouping == grouping]
  df_current <- df_ioi_subset %>% dplyr::filter(Grouping == grouping & normIOI < upper_ioi_2 & normIOI > lower_ioi_2)
  df_ioi_trim_sd_2 <- rbind(df_ioi_trim_sd_2, df_current)
}

removed_ioi_2 <- nrow(df_ioi_subset)-nrow(df_ioi_trim_sd_2)
proportion_ioi_2 <- round(removed_ioi_2/nrow(df_ioi_subset), 5)
print(sprintf("(Method 2)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_2, proportion_ioi_2*100))

p_ioi_hist_sd_2 <- ggplot(df_ioi_trim_sd_2, aes(x = normIOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .01)
plot(p_ioi_hist_sd_2)

p_ioi_box_sd_2 <- ggboxplot(df_ioi_trim_sd_2, x = "Skill", y = "normIOI", color = "Condition")
p_ioi_box_sd_2 <- ggpar(p_ioi_box_sd_2, ylab = "Normalised IOI (IOI/Tempo)")
plot(p_ioi_box_sd_2)

# Save plots
# png files
ggsave("./trimmed/ioi_hist_sd_2.png", plot = p_ioi_hist_sd_2, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_box_sd_2.png", plot = p_ioi_box_sd_2, dpi = 600, width = 5, height = 4)

# Export a csv file for df_ioi_trim_sd
write.csv(df_ioi_trim_sd_2, file = "./trimmed/data_ioi_2.csv", row.names = F)

####################################
# 3. > +- 3SD actross the conditions
# separately for Subcomponent Change
# (YES or NO - as a factor)
####################################
# evaluate Boundary (i.e., LtoS, StoL, FtoP, PtoF) or not
df_ioi_subset$Boundary <- "No"
df_ioi_subset$Boundary[df_ioi_subset$Subcomponent == "LtoS" | df_ioi_subset$Subcomponent == "StoL" |
                         df_ioi_subset$Subcomponent == "FtoP" | df_ioi_subset$Subcomponent == "PtoF"] <- "Yes"

# exclude ioi > +- 3SD (separately for each Boundary)
ioi_boundary <- aggregate(normIOI~Boundary, data = df_ioi_subset,
                       FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
ioi_boundary <- cbind(ioi_boundary, as.data.frame(ioi_boundary[,2]))

df_ioi_trim_sd_3 <- data.frame()
for (boundary in unique(ioi_boundary$Boundary)){
  upper_ioi_3 = ioi_boundary$mean[ioi_boundary$Boundary == boundary]+3*ioi_boundary$sd[ioi_boundary$Boundary == boundary]
  lower_ioi_3 = ioi_boundary$mean[ioi_boundary$Boundary == boundary]-3*ioi_boundary$sd[ioi_boundary$Boundary == boundary]
  df_current <- df_ioi_subset %>% dplyr::filter(Boundary == boundary & normIOI < upper_ioi_3 & normIOI > lower_ioi_3)
  df_ioi_trim_sd_3 <- rbind(df_ioi_trim_sd_3, df_current)
}

removed_ioi_3 <- nrow(df_ioi_subset)-nrow(df_ioi_trim_sd_3)
proportion_ioi_3 <- round(removed_ioi_3/nrow(df_ioi_subset), 5)
print(sprintf("(Method 3)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_3, proportion_ioi_3*100))

p_ioi_hist_sd_3 <- ggplot(df_ioi_trim_sd_3, aes(x = normIOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .01)
plot(p_ioi_hist_sd_3)

p_ioi_box_sd_3 <- ggboxplot(df_ioi_trim_sd_3, x = "Skill", y = "normIOI", color = "Condition")
p_ioi_box_sd_3 <- ggpar(p_ioi_box_sd_3, ylab = "Normalised IOI (IOI/Tempo)")
plot(p_ioi_box_sd_3)

# Save plots
# png files
ggsave("./trimmed/ioi_hist_sd_3.png", plot = p_ioi_hist_sd_3, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_box_sd_3.png", plot = p_ioi_box_sd_3, dpi = 600, width = 5, height = 4)

# Export a csv file for df_ioi_trim_sd
write.csv(df_ioi_trim_sd_3, file = "./trimmed/data_ioi_3.csv", row.names = F)
