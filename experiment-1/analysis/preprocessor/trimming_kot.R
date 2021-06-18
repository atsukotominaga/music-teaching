#!/usr/local/bin/R
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 25/09/2020
# This script removes outliers.

# set working directory
if (!require("here")) {install.packages("here"); require("here")}
here::i_am("trimming_kot.R")

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
if (!file.exists(here("trimmed"))){
  dir.create(here("trimmed"))
}

# read txt files
dt_onset_all <- fread(file = here("filtered", "dt_correct_onset.txt"))
dt_offset_all <- fread(file = here("filtered", "dt_correct_offset.txt"))

# check whether each trial has both onset/offset datasets
onset_trials <- dt_onset_all[, .(N = .N), by = .(SubNr, BlockNr, TrialNr)]
offset_trials <- dt_offset_all[, .(N = .N), by = .(SubNr, BlockNr, TrialNr)]
valid_trials <- rbind(onset_trials, offset_trials)
valid_trials$Duplicate <- duplicated(valid_trials)
valid_trials_included <- valid_trials[Duplicate == TRUE]
# 0 trial was excluded
write(sprintf("KOT (Onset-Offset check): %i trial was excluded because it lacks either onset or offset dataset", nrow(onset_trials)-nrow(valid_trials_included)), file = here("trimmed", "outlier.txt"), append = T)
print(sprintf("KOT (Onset-Offset check): %i trial was excluded because it lacks either onset or offset dataset", nrow(onset_trials)-nrow(valid_trials_included)))

dt_onset <- data.table()
for (row in 1:nrow(valid_trials_included)){
 current <- dt_onset_all[SubNr == valid_trials_included$SubNr[row] & BlockNr == valid_trials_included$BlockNr[row] & TrialNr == valid_trials_included$TrialNr[row]]
 dt_onset <- rbind(dt_onset, current)
}

dt_offset <- data.table()
for (row in 1:nrow(valid_trials_included)){
  current <- dt_offset_all[SubNr == valid_trials_included$SubNr[row] & BlockNr == valid_trials_included$BlockNr[row] & TrialNr == valid_trials_included$TrialNr[row]]
  dt_offset <- rbind(dt_offset, current)
}

# sort by SubNr, BlockNr, TrialNr, NoteNr
dt_onset <- dt_onset[order(SubNr, BlockNr, TrialNr, NoteNr)]
dt_offset <- dt_offset[order(SubNr, BlockNr, TrialNr, NoteNr)]

####################################
# Exclude 4 participants
# (see error_summary.Rmd
# and trimming_ioi.R) 
####################################
dt_kot_onset <- dt_onset[SubNr != 1 & SubNr != 3 & SubNr != 14 & SubNr != 16]
dt_kot_offset <- dt_offset[SubNr != 1 & SubNr != 3 & SubNr != 14 & SubNr != 16]

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
dt_ioi_1 <- fread(file.path(here("trimmed", "data_ioi_1.txt")), header = T) # remove outliers outside 3SD across the conditions
dt_ioi_3 <- fread(file.path(here("trimmed", "data_ioi_3.txt")), header = T) # remove outliers per Boundary

####################################
# Key Overlap Time - articulation
####################################
dt_kot <- dt_kot_onset

# Offset 1 - Onset 2
dt_kot$KOT <- NA
for (row in 1:nrow(dt_kot)){
  if (row < nrow(dt_kot)){
    dt_kot$KOT[row+1] <- dt_kot_offset$TimeStamp[row] - dt_kot_onset$TimeStamp[row+1] # offset(n) - onset(n+1)
  }
}

# remove the first note
dt_kot <- dt_kot[RowNr != 1]

# assign a sequence number for each tone
dt_kot$Interval <- rep(1:66, nrow(dt_kot)/66)

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
dt_kot$Grouping = "NA"
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    dt_kot[Condition == ls_grouping$Condition[cond] & Skill == ls_grouping$Skill[skill]]$Grouping <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

####################################
# Remove outliers (KOT)
####################################
# exclude irrelevant notes (Subcomponent == NA means not 16th notes / KOT == NA means a missing value)
dt_kot_subset <- dt_kot[Subcomponent != "NA" & !is.na(KOT)]

# draw histogram and boxplot
p_kot_hist <- ggplot(dt_kot_subset, aes(x = KOT, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 10) +
  theme_classic()
plot(p_kot_hist)

p_kot_box <- ggboxplot(dt_kot_subset, x = "Subcomponent", y = "KOT", color = "Condition")
p_kot_box <- ggpar(p_kot_box, ylab = "Key-Overlap Time")
plot(p_kot_box)

# exclude kot > +- 3SD (per subcomponent)
kot_subcomponent <- dt_kot_subset[, .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = Subcomponent]
dt_kot_trim_sd <- data.table()
for (subcomponent in unique(dt_kot_subset$Subcomponent)){
  upper <- kot_subcomponent[Subcomponent == subcomponent]$Mean+3*kot_subcomponent[Subcomponent == subcomponent]$SD
  lower <- kot_subcomponent[Subcomponent == subcomponent]$Mean-3*kot_subcomponent[Subcomponent == subcomponent]$SD
  dt_current <- dt_kot_subset[Subcomponent == subcomponent & KOT < upper & KOT > lower]
  dt_kot_trim_sd <- rbind(dt_kot_trim_sd, dt_current)
}
removed_kot <- nrow(dt_kot_subset)-nrow(dt_kot_trim_sd)
proportion_kot <- round(removed_kot/nrow(dt_kot_subset), 5)
write(sprintf("KOT: Remove %i responses beyond +- 3SD / %f percent", removed_kot, proportion_kot*100), file = here("trimmed", "outlier.txt"), append = T)
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
ggsave(here("trimmed", "kot_hist.png"), plot = p_kot_hist, dpi = 600, width = 5, height = 4)
ggsave(here("trimmed", "kot_hist_sd.png"), plot = p_kot_hist_sd, dpi = 600, width = 5, height = 4)
ggsave(here("trimmed", "kot_box.png"), plot = p_kot_box, dpi = 600, width = 5, height = 4)
ggsave(here("trimmed", "kot_box_sd.png"), plot = p_kot_box_sd, dpi = 600, width = 5, height = 4)

# Export a csv file for dt_kot_trim_sd
fwrite(dt_kot_trim_sd, file = here("trimmed", "data_kot.txt"), row.names = F)
