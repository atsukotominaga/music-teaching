#!/usr/local/bin/R
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 25/09/2020
# This script removes outliers.
# GitHub repo (private): https://github.com/atsukotominaga/teaching-v1.0/tree/master/script/R/preprocessor

# set working directory
if (!require("here")) {install.packages("here"); require("here")}
here::i_am("trimming_vel.R")

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
dt_onset_all <- fread(file = "./filtered/dt_correct_onset.txt")
dt_offset_all <- fread(file = "./filtered/dt_correct_offset.txt")

# sort by SubNr, BlockNr, TrialNr, NoteNr
dt_onset_all <- dt_onset_all[order(SubNr, BlockNr, TrialNr, NoteNr)]
dt_offset_all <- dt_offset_all[order(SubNr, BlockNr, TrialNr, NoteNr)]

####################################
# Exclude 3 participants
# (see error_summary.Rmd
# and trimming_ioi.R) 
####################################
dt_onset <- dt_onset_all[SubNr != 3 & SubNr != 14 & SubNr != 16]
dt_offset <- dt_offset_all[SubNr != 3 & SubNr != 14 & SubNr != 16]

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
# Key Velocity - dynamics
####################################
# calculate Acc (acceleration - velocity difference between notes)
dt_vel <- dt_onset
dt_vel$Diff <- diff(c(0, dt_vel$Velocity))

# remove the first note
dt_vel_diff <- dt_vel[RowNr != 1]
dt_vel$Diff <- NULL # remove Diff from dt_vel

# assign Interval
dt_vel_diff$Interval <- rep(1:66, nrow(dt_vel_diff)/66)

# assign Subcomponents
# for each note
dt_vel$Subcomponent <- "NA"
# Legato
for (phrase in 1:length(ls_legato_2)){
  for (note in 1:length(ls_legato_2[[phrase]])){
    dt_vel[Skill == "articulation" & RowNr == ls_legato_2[[phrase]][note]]$Subcomponent <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato_2)){
  for (note in 1:length(ls_staccato_2[[phrase]])){
    dt_vel[Skill == "articulation" & RowNr == ls_staccato_2[[phrase]][note]]$Subcomponent <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte_2)){
  for (note in 1:length(ls_forte_2[[phrase]])){
    dt_vel[Skill == "dynamics" & RowNr == ls_forte_2[[phrase]][note]]$Subcomponent <- "Forte"
  }
}

# Piano
for (phrase in 1:length(ls_piano_2)){
  for (note in 1:length(ls_piano_2[[phrase]])){
    dt_vel[Skill == "dynamics" & RowNr == ls_piano_2[[phrase]][note]]$Subcomponent <- "Piano"
  }
}

# assign Subcomponents
# for intervals
dt_vel_diff$Subcomponent <- "NA"
# Legato
for (phrase in 1:length(ls_legato)){
  for (note in 1:length(ls_legato[[phrase]])){
    dt_vel_diff[Skill == "articulation" & Interval == ls_legato[[phrase]][note]]$Subcomponent <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato)){
  for (note in 1:length(ls_staccato[[phrase]])){
    dt_vel_diff[Skill == "articulation" & Interval == ls_staccato[[phrase]][note]]$Subcomponent <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte)){
  for (note in 1:length(ls_forte[[phrase]])){
    dt_vel_diff[Skill == "dynamics" & Interval == ls_forte[[phrase]][note]]$Subcomponent <- "Forte"
  }
}
# Piano
for (phrase in 1:length(ls_piano)){
  for (note in 1:length(ls_piano[[phrase]])){
    dt_vel_diff[Skill == "dynamics" & Interval == ls_piano[[phrase]][note]]$Subcomponent <- "Piano"
  }
}

# Assign Skill Change
for (i in change_1){
  dt_vel_diff[Skill == "articulation" & Interval == i]$Subcomponent <- "LtoS"
  dt_vel_diff[Skill == "dynamics" & Interval == i]$Subcomponent <- "FtoP"
}
for (i in change_2){
  dt_vel_diff[Skill == "articulation" & Interval == i]$Subcomponent <- "StoL"
  dt_vel_diff[Skill == "dynamics" & Interval == i]$Subcomponent <- "PtoF"
}

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
dt_vel$Grouping <- "NA"
dt_vel_diff$Grouping <- "NA"
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    dt_vel[Condition == ls_grouping$Condition[cond] & Skill == ls_grouping$Skill[skill]]$Grouping <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    dt_vel_diff[Condition == ls_grouping$Condition[cond] & Skill == ls_grouping$Skill[skill]]$Grouping <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

####################################
# Remove outliers
####################################
# exclude irrelevant notes (Subcomponent == NA means not 16th notes / Velocity/Diff == NA means a missing value)
dt_vel_subset <- dt_vel[Subcomponent != "NA" & !is.na(Velocity)]
dt_vel_diff_subset <- dt_vel_diff[Subcomponent != "NA" & !is.na(Diff)]

###### dt_vel
# draw histogram and boxplot
p_vel_hist <- ggplot(dt_vel_subset, aes(x = Velocity, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 1)
plot(p_vel_hist)

p_vel_box <- ggboxplot(dt_vel_subset, x = "Subcomponent", y = "Velocity", color = "Condition")
p_vel_box <- ggpar(p_vel_box, ylab = "Velocity (0-127)")
plot(p_vel_box)

# exclude vel > +- 3SD (within a given condition)
vel_subcomponent <- dt_vel_subset[, .(N = .N, Mean = mean(Velocity), SD = sd(Velocity)), by = Subcomponent]
 
dt_vel_trim_sd <- data.table()
for (subcomponent in unique(dt_vel_subset$Subcomponent)){
  upper <- vel_subcomponent[Subcomponent == subcomponent]$Mean+3*vel_subcomponent[Subcomponent == subcomponent]$SD
  lower <- vel_subcomponent[Subcomponent == subcomponent]$Mean-3*vel_subcomponent[Subcomponent == subcomponent]$SD
  dt_current <- dt_vel_subset[Subcomponent == subcomponent & Velocity < upper & Velocity > lower]
  dt_vel_trim_sd <- rbind(dt_vel_trim_sd, dt_current)
}

removed_vel <- nrow(dt_vel_subset)-nrow(dt_vel_trim_sd)
proportion_vel <- round(removed_vel/nrow(dt_vel_subset), 5)
write(sprintf("Velocity: Remove %i responses beyond +- 3SD / %f percent", removed_vel, proportion_vel*100), file = "./trimmed/outlier.txt", append = T)
print(sprintf("Velocity: Remove %i responses beyond +- 3SD / %f percent", removed_vel, proportion_vel*100))

# draw histogram and boxplot
p_vel_hist_sd <- ggplot(dt_vel_trim_sd, aes(x = Velocity, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 1)
plot(p_vel_hist_sd)

p_vel_box_sd <- ggboxplot(dt_vel_trim_sd, x = "Subcomponent", y = "Velocity", color = "Condition")
p_vel_box_sd <- ggpar(p_vel_box_sd, ylab = "Velocity (0-127)")
plot(p_vel_box_sd)

# Save plots
# png files
ggsave("./trimmed/vel_hist.png", plot = p_vel_hist, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_hist_sd.png", plot = p_vel_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_box.png", plot = p_vel_box, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_box_sd.png", plot = p_vel_box_sd, dpi = 600, width = 5, height = 4)

# export a txt file for dt_trim_sd
fwrite(dt_vel_trim_sd, file = "./trimmed/data_vel.txt", row.names = F)

###### dt_vel_diff
# draw histogram and boxplot
p_vel_diff_hist <- ggplot(dt_vel_diff_subset, aes(x = Diff, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 1)
plot(p_vel_diff_hist)

p_vel_diff_box <- ggboxplot(dt_vel_diff_subset, x = "Subcomponent", y = "Diff", color = "Condition")
p_vel_diff_box <- ggpar(p_vel_diff_box, ylab = "Difference")
plot(p_vel_diff_box)

# exclude vel > +- 3SD (within a given condition)
vel_diff_subcomponent <- dt_vel_diff_subset[, .(N = .N, Mean = mean(Diff), SD = sd(Diff)), by = Subcomponent]

dt_vel_diff_trim_sd <- data.table()
for (subcomponent in unique(dt_vel_diff_subset$Subcomponent)){
  upper <- vel_diff_subcomponent[Subcomponent == subcomponent]$Mean+3*vel_diff_subcomponent[Subcomponent == subcomponent]$SD
  lower <- vel_diff_subcomponent[Subcomponent == subcomponent]$Mean-3*vel_diff_subcomponent[Subcomponent == subcomponent]$SD
  dt_current <- dt_vel_diff_subset[Subcomponent == subcomponent & Diff < upper & Diff > lower]
  dt_vel_diff_trim_sd <- rbind(dt_vel_diff_trim_sd, dt_current)
}

removed_vel_diff <- nrow(dt_vel_diff_subset)-nrow(dt_vel_diff_trim_sd)
proportion_vel_diff <- round(removed_vel_diff/nrow(dt_vel_diff_subset), 5)
write(sprintf("Diff: Remove %i responses beyond +- 3SD / %f percent", removed_vel_diff, proportion_vel_diff*100), file = "./trimmed/outlier.txt", append = T)
print(sprintf("Diff: Remove %i responses beyond +- 3SD / %f percent", removed_vel_diff, proportion_vel_diff*100))

# draw histogram and boxplot
p_vel_diff_hist_sd <- ggplot(dt_vel_diff_trim_sd, aes(x = Diff, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 1)
plot(p_vel_diff_hist_sd)

p_vel_diff_box_sd <- ggboxplot(dt_vel_diff_trim_sd, x = "Subcomponent", y = "Diff", color = "Condition")
p_vel_diff_box_sd <- ggpar(p_vel_diff_box_sd, ylab = "Difference")
plot(p_vel_diff_box_sd)

# Save plots
# png files
ggsave("./trimmed/vel_diff_hist.png", plot = p_vel_diff_hist, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_diff_hist_sd.png", plot = p_vel_diff_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_diff_box.png", plot = p_vel_diff_box, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_diff_box_sd.png", plot = p_vel_diff_box_sd, dpi = 600, width = 5, height = 4)

# Export a csv file for dt_trim_sd
fwrite(dt_vel_diff_trim_sd, file = "./trimmed/data_vel_diff.txt", row.names = F)
