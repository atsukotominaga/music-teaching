#!/usr/local/bin/R
#rm(list=ls(all=T)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 26/11/2019
# This script removes outliers.

####################################
#  Requirements
####################################
# set working directory
if (!require("here")) {install.packages("here"); require("here")}
here::i_am("trimming_vel.R")

# install and load required packages
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}

# setting
# ggplots
theme_set(theme_classic())

# create necessary folders if not exist
# filtered - all of the outputs will be stored in this folder
if (!file.exists(here("trimmed"))){
  dir.create(here("trimmed"))
}

# read a text file for ideal performance
dt_ideal <- read.table(here("ideal.txt"))
colnames(dt_ideal) <- "Pitch"
dt_ideal$RowNr <- c(1:nrow(dt_ideal))
dt_ideal <- dt_ideal[c(2, 1)]

# read txt files
dt_onset <- fread(file = here("filtered", "dt_correct_onset.txt"), header = T)
dt_offset <- fread(file = here("filtered", "dt_correct_offset.txt"), header = T)

# assign RowNr
dt_onset$RowNr <- rep(1:72, nrow(dt_onset)/72)
dt_offset$RowNr <- rep(1:72, nrow(dt_offset)/72)

# sort by SubNr, BlockNr, TrialNr and NoteNr
dt_onset <- dt_onset[order(SubNr, BlockNr, TrialNr, NoteNr)]
dt_offset <- dt_offset[order(SubNr, BlockNr, TrialNr, NoteNr)]

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
# calculate Diff (velocity difference between notes)
dt_vel <- dt_onset
dt_vel$Diff <- diff(c(0, dt_vel$Velocity))
# convert bpm to ms
dt_vel[Tempo == 120]$Tempo <- 250
dt_vel[Tempo == 110]$Tempo <- 273
dt_vel[Tempo == 100]$Tempo <- 300

# remove the first note
dt_vel_diff <- dt_vel[RowNr != 1]
dt_vel$Diff <- NULL # remove Diff from dt_vel

# assign Interval
dt_vel_diff$Interval <- rep(1:71, nrow(dt_vel_diff)/71)

# assign Subcomponents
# for each note
dt_vel$Subcomponent <- "NA"
# Legato
for (phrase in 1:length(ls_legato2)){
  for (note in 1:length(ls_legato2[[phrase]])){
    dt_vel[Skill == "articulation" & RowNr == ls_legato2[[phrase]][note]]$Subcomponent <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato2)){
  for (note in 1:length(ls_staccato2[[phrase]])){
    dt_vel[Skill == "articulation" & RowNr == ls_staccato2[[phrase]][note]]$Subcomponent <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte2)){
  for (note in 1:length(ls_forte2[[phrase]])){
    dt_vel[Skill == "dynamics" & RowNr == ls_forte2[[phrase]][note]]$Subcomponent <- "Forte"
  }
}

# Piano
for (phrase in 1:length(ls_piano2)){
  for (note in 1:length(ls_piano2[[phrase]])){
    dt_vel[Skill == "dynamics" & RowNr == ls_piano2[[phrase]][note]]$Subcomponent <- "Piano"
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
for (number in change_1){
  dt_vel_diff[Skill == "articulation" & Interval == number]$Subcomponent <- "LtoS"
  dt_vel_diff[Skill == "dynamics" & Interval == number]$Subcomponent <- "FtoP"
}
for (number in change_2){
  dt_vel_diff[Skill == "articulation" & Interval == number]$Subcomponent <- "StoL"
  dt_vel_diff[Skill == "dynamics" & Interval == number]$Subcomponent <- "PtoF"
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
# exclude irrelevant notes (Subcomponent == NA means not 8th notes / Velocity/Diff == NA means a missing value)
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
write(sprintf("Velocity: Remove %i responses beyond +- 3SD / %f percent", removed_vel, proportion_vel*100), file = here("trimmed", "outlier.txt"), append = T)
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
ggsave(here("trimmed", "vel_hist.png"), plot = p_vel_hist, dpi = 600, width = 5, height = 4)
ggsave(here("trimmed", "vel_hist_sd.png"), plot = p_vel_hist_sd, dpi = 600, width = 5, height = 4)
ggsave(here("trimmed", "vel_box.png"), plot = p_vel_box, dpi = 600, width = 5, height = 4)
ggsave(here("trimmed", "vel_box_sd.png"), plot = p_vel_box_sd, dpi = 600, width = 5, height = 4)

# export a txt file for dt_trim_sd
fwrite(dt_vel_trim_sd, file = here("trimmed", "data_vel.txt"), row.names = F)

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
write(sprintf("Diff: Remove %i responses beyond +- 3SD / %f percent", removed_vel_diff, proportion_vel_diff*100), file = here("trimmed", "outlier.txt"), append = T)
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
ggsave(here("trimmed", "vel_diff_hist.png"), plot = p_vel_diff_hist, dpi = 600, width = 5, height = 4)
ggsave(here("trimmed", "vel_diff_hist_sd.png"), plot = p_vel_diff_hist_sd, dpi = 600, width = 5, height = 4)
ggsave(here("trimmed", "vel_diff_box.png"), plot = p_vel_diff_box, dpi = 600, width = 5, height = 4)
ggsave(here("trimmed", "vel_diff_box_sd.png"), plot = p_vel_diff_box_sd, dpi = 600, width = 5, height = 4)

# Export a txt file for dt_trim_sd
fwrite(dt_vel_diff_trim_sd, file = here("trimmed", "data_vel_diff.txt"), row.names = F)
