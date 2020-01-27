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
# Key Velocity - dynamics
####################################
# calculate Acc (acceleration - velocity difference between notes)
df_vel <- df_onset
df_vel$Diff <- diff(c(0, df_vel$Velocity))
# convert bpm to ms
df_vel$Tempo[df_vel$Tempo == 120] <- 250
df_vel$Tempo[df_vel$Tempo == 110] <- 273
df_vel$Tempo[df_vel$Tempo == 100] <- 300

# assign NoteNr
df_vel$NoteNr <- rep(1:72, nrow(df_vel)/72)

# remove the first note
df_vel_diff <- df_vel %>% dplyr::filter(NoteNr != 1)
df_vel$Diff <- NULL # remove Acc from df_vel

# assign Interval
df_vel_diff$Interval <- rep(1:71, nrow(df_vel_diff)/71)

# assign Subcomponents
# for each note
df_vel$Subcomponent <- NA
# Legato
for (phrase in 1:length(ls_legato2)){
  for (note in 1:length(ls_legato2[[phrase]])){
    df_vel$Subcomponent[df_vel$Skill == "articulation" & df_vel$Note == ls_legato2[[phrase]][note]] <- "Legato"
  }
}

# Staccato
for (phrase in 1:length(ls_staccato2)){
  for (note in 1:length(ls_staccato2[[phrase]])){
    df_vel$Subcomponent[df_vel$Skill == "articulation" & df_vel$Note == ls_staccato2[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte2)){
  for (note in 1:length(ls_forte2[[phrase]])){
    df_vel$Subcomponent[df_vel$Skill == "dynamics" & df_vel$Note == ls_forte2[[phrase]][note]] <- "Forte"
  }
}

# Piano
for (phrase in 1:length(ls_piano2)){
  for (note in 1:length(ls_piano2[[phrase]])){
    df_vel$Subcomponent[df_vel$Skill == "dynamics" & df_vel$Note == ls_piano2[[phrase]][note]] <- "Piano"
  }
}

# assign Subcomponents
# for intervals
df_vel_diff$Subcomponent <- NA
# Legato
for (phrase in 1:length(ls_legato)){
  for (note in 1:length(ls_legato[[phrase]])){
    df_vel_diff$Subcomponent[df_vel_diff$Skill == "articulation" & df_vel_diff$Interval == ls_legato[[phrase]][note]] <- "Legato"
  }
}
# Stdiffato
for (phrase in 1:length(ls_staccato)){
  for (note in 1:length(ls_staccato[[phrase]])){
    df_vel_diff$Subcomponent[df_vel_diff$Skill == "articulation" & df_vel_diff$Interval == ls_staccato[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte)){
  for (note in 1:length(ls_forte[[phrase]])){
    df_vel_diff$Subcomponent[df_vel_diff$Skill == "dynamics" & df_vel_diff$Interval == ls_forte[[phrase]][note]] <- "Forte"
  }
}
# Piano
for (phrase in 1:length(ls_piano)){
  for (note in 1:length(ls_piano[[phrase]])){
    df_vel_diff$Subcomponent[df_vel_diff$Skill == "dynamics" & df_vel_diff$Interval == ls_piano[[phrase]][note]] <- "Piano"
  }
}

# Assign Skill Change
for (i in change_1){
  df_vel_diff$Subcomponent[df_vel_diff$Skill == "articulation" & df_vel_diff$Interval == i] <- "LtoS"
  df_vel_diff$Subcomponent[df_vel_diff$Skill == "dynamics" & df_vel_diff$Interval == i] <- "FtoP"
}
for (i in change_2){
  df_vel_diff$Subcomponent[df_vel_diff$Skill == "articulation" & df_vel_diff$Interval == i] <- "StoL"
  df_vel_diff$Subcomponent[df_vel_diff$Skill == "dynamics" & df_vel_diff$Interval == i] <- "PtoF"
}

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    df_vel$Grouping[df_vel$Condition == ls_grouping$Condition[cond] & df_vel$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    df_vel_diff$Grouping[df_vel_diff$Condition == ls_grouping$Condition[cond] & df_vel_diff$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

####################################
# Remove outliers
####################################
# exclude irrelevant notes
df_vel_subset <- subset(df_vel, !is.na(df_vel$Subcomponent) & !is.na(df_vel$Velocity))
df_vel_diff_subset <- subset(df_vel_diff, !is.na(df_vel_diff$Subcomponent) & !is.na(df_vel_diff$Diff))

###### df_vel
# draw histogram
p_vel_hist <- ggplot(df_vel_subset, aes(x = Velocity, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 1)
plot(p_vel_hist)

p_vel_box <- ggboxplot(df_vel_subset, x = "Skill", y = "Velocity", color = "Condition")
p_vel_box <- ggpar(p_vel_box, ylab = "Velocity (0-127)")
plot(p_vel_box)

# exclude vel > +- 3SD (within a given condition)
vel_subcomponent <- aggregate(Velocity~Subcomponent, data = df_vel_subset,
                              FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_subcomponent <- cbind(vel_subcomponent, as.data.frame(vel_subcomponent[,2]))
df_vel_trim_sd <- data.frame()
for (subcomponent in unique(df_vel_subset$Subcomponent)){
  upper = vel_subcomponent$mean[vel_subcomponent$Subcomponent == subcomponent]+3*vel_subcomponent$sd[vel_subcomponent$Subcomponent == subcomponent]
  lower = vel_subcomponent$mean[vel_subcomponent$Subcomponent == subcomponent]-3*vel_subcomponent$sd[vel_subcomponent$Subcomponent == subcomponent]
  df_current <- df_vel_subset %>% dplyr::filter(Subcomponent == subcomponent & Velocity < upper & Velocity > lower)
  df_vel_trim_sd <- rbind(df_vel_trim_sd, df_current)
}
removed_vel <- nrow(df_vel_subset)-nrow(df_vel_trim_sd)
proportion_vel <- round(removed_vel/nrow(df_vel_subset), 5)
print(sprintf("Velocity: Remove %i responses beyond +- 3SD / %f percent", removed_vel, proportion_vel*100))

p_vel_hist_sd <- ggplot(df_vel_trim_sd, aes(x = Velocity, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 1)
plot(p_vel_hist_sd)

p_vel_box_sd <- ggboxplot(df_vel_trim_sd, x = "Skill", y = "Velocity", color = "Condition")
p_vel_box_sd <- ggpar(p_vel_box_sd, ylab = "Velocity (0-127)")
plot(p_vel_box_sd)

# Save plots
# png files
ggsave("./trimmed/vel_hist.png", plot = p_vel_hist, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_hist_sd.png", plot = p_vel_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_box.png", plot = p_vel_box, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_box_sd.png", plot = p_vel_box_sd, dpi = 600, width = 5, height = 4)

# export a csv file for df_trim_sd
write.csv(df_vel_trim_sd, file = "./trimmed/data_vel.csv", row.names = F)

###### df_vel_diff
# draw histogram
p_vel_diff_hist <- ggplot(df_vel_diff_subset, aes(x = Diff, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 1)
plot(p_vel_diff_hist)

p_vel_diff_box <- ggboxplot(df_vel_diff_subset, x = "Skill", y = "Diff", color = "Condition")
p_vel_diff_box <- ggpar(p_vel_diff_box, ylab = "Difference")
plot(p_vel_diff_box)

# exclude vel > +- 3SD (within a given condition)
vel_diff_subcomponent <- aggregate(Diff~Subcomponent, data = df_vel_diff_subset,
                              FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_diff_subcomponent <- cbind(vel_diff_subcomponent, as.data.frame(vel_diff_subcomponent[,2]))
df_vel_diff_trim_sd <- data.frame()
for (subcomponent in unique(df_vel_diff_subset$Subcomponent)){
  upper = vel_diff_subcomponent$mean[vel_diff_subcomponent$Subcomponent == subcomponent]+3*vel_diff_subcomponent$sd[vel_diff_subcomponent$Subcomponent == subcomponent]
  lower = vel_diff_subcomponent$mean[vel_diff_subcomponent$Subcomponent == subcomponent]-3*vel_diff_subcomponent$sd[vel_diff_subcomponent$Subcomponent == subcomponent]
  df_current <- df_vel_diff_subset %>% dplyr::filter(Subcomponent == subcomponent & Diff < upper & Diff > lower)
  df_vel_diff_trim_sd <- rbind(df_vel_diff_trim_sd, df_current)
}
removed_vel_diff <- nrow(df_vel_diff_subset)-nrow(df_vel_diff_trim_sd)
proportion_vel_diff <- round(removed_vel_diff/nrow(df_vel_diff_subset), 5)
print(sprintf("Diff: Remove %i responses beyond +- 3SD / %f percent", removed_vel_diff, proportion_vel_diff*100))

p_vel_diff_hist_sd <- ggplot(df_vel_diff_trim_sd, aes(x = Diff, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 1)
plot(p_vel_diff_hist_sd)

p_vel_diff_box_sd <- ggboxplot(df_vel_diff_trim_sd, x = "Skill", y = "Diff", color = "Condition")
p_vel_diff_box_sd <- ggpar(p_vel_diff_box_sd, ylab = "Difference")
plot(p_vel_diff_box_sd)

# Save plots
# png files
ggsave("./trimmed/vel_diff_hist.png", plot = p_vel_diff_hist, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_diff_hist_sd.png", plot = p_vel_diff_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_diff_box.png", plot = p_vel_diff_box, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_diff_box_sd.png", plot = p_vel_diff_box_sd, dpi = 600, width = 5, height = 4)

# Export a csv file for df_trim_sd
write.csv(df_vel_diff_trim_sd, file = "./trimmed/data_vel_diff.csv", row.names = F)
