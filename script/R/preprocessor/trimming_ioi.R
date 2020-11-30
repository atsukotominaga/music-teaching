#!/usr/local/bin/R
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 24/09/2020
# This script removes outliers.
# GitHub repo (private): https://github.com/atsukotominaga/teaching-v1.0/tree/master/script/R/preprocessor

# set working directory
if (!require("here")) {install.packages("here"); require("here")}
here::i_am("trimming_ioi.R")

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
# Exclude two participants
# (see error_summary.Rmd)
####################################
dt_onset <- dt_onset_all[SubNr != 3 & SubNr != 14]
dt_offset <- dt_offset_all[SubNr != 3 & SubNr != 14]

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
# calculate IOIs
dt_ioi <- dt_onset
dt_ioi$IOI <- diff(c(0, dt_ioi$TimeStamp))

# remove the first note
dt_ioi <- dt_ioi[RowNr != 1]

# assign Interval
dt_ioi$Interval <- rep(1:66, nrow(dt_ioi)/66)

# assign Subcomponents# assign Subcomponents
dt_ioi$Subcomponent <- "NA"
# Legato
for (phrase in 1:length(ls_legato)){
  for (note in 1:length(ls_legato[[phrase]])){
    dt_ioi[Skill == "articulation" & Interval == ls_legato[[phrase]][note]]$Subcomponent <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato)){
  for (note in 1:length(ls_staccato[[phrase]])){
    dt_ioi[Skill == "articulation" & Interval == ls_staccato[[phrase]][note]]$Subcomponent <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte)){
  for (note in 1:length(ls_forte[[phrase]])){
    dt_ioi[Skill == "dynamics" & Interval == ls_forte[[phrase]][note]]$Subcomponent <- "Forte"
  }
}
# Piano
for (phrase in 1:length(ls_piano)){
  for (note in 1:length(ls_piano[[phrase]])){
    dt_ioi[Skill == "dynamics" & Interval == ls_piano[[phrase]][note]]$Subcomponent <- "Piano"
  }
}

# assign Subcomponent Change
for (number in change_1){
  dt_ioi[Skill == "articulation" & Interval == number]$Subcomponent <- "LtoS"
  dt_ioi[Skill == "dynamics" & Interval == number]$Subcomponent <- "FtoP"
}
for (number in change_2){
  dt_ioi[Skill == "articulation" & Interval == number]$Subcomponent <- "StoL"
  dt_ioi[Skill == "dynamics" & Interval == number]$Subcomponent <- "PtoF"
}

# add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
dt_ioi$Grouping <- "NA"
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    dt_ioi[Condition == ls_grouping$Condition[cond] & Skill == ls_grouping$Skill[skill]]$Grouping <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

####################################
# Remove outliers (3 methods)
####################################
# exclude irrelevant notes (Subcomponent == NA means not 16th notes / IOI == NA means a missing value)
dt_ioi_subset <- dt_ioi[Subcomponent != "NA" & !is.na(IOI)]

# draw histogram and boxplot
p_ioi_hist <- ggplot(dt_ioi_subset, aes(x = IOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 10)
plot(p_ioi_hist)

p_ioi_box <- ggboxplot(dt_ioi_subset, x = "Skill", y = "IOI", color = "Condition")
p_ioi_box <- ggpar(p_ioi_box, ylab = "IOI")
plot(p_ioi_box)

####################################
# exclude deviated participants
####################################
ioi_summary <- dt_ioi_subset[, .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = .(SubNr)]
# exclude tempo deviated participants
ioi_summary$Include <- "No"
ioi_summary[Mean < mean(ioi_summary$Mean)+3*sd(ioi_summary$Mean) & Mean > mean(ioi_summary$Mean)-3*sd(ioi_summary$Mean)]$Include <- "Yes"
# exclude SubNr 16
dt_ioi_subset <- dt_ioi_subset[SubNr != 16]

####################################
# 1. > +- 3SD across the conditions
####################################
# exclude ioi > +- 3SD (across the conditions)
upper_ioi_1 <- mean(dt_ioi_subset$IOI)+3*sd(dt_ioi_subset$IOI)
lower_ioi_1 <- mean(dt_ioi_subset$IOI)-3*sd(dt_ioi_subset$IOI)
dt_ioi_trim_sd_1 <- dt_ioi_subset[IOI < upper_ioi_1 & IOI > lower_ioi_1]
removed_ioi_1 <- nrow(dt_ioi_subset)-nrow(dt_ioi_trim_sd_1)
proportion_ioi_1 <- round(removed_ioi_1/nrow(dt_ioi_subset), 5)
write(sprintf("(Method 1)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_1, proportion_ioi_1*100), file = "./trimmed/outlier.txt", append = T)
print(sprintf("(Method 1)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_1, proportion_ioi_1*100))

# draw histogram and boxplot
p_ioi_hist_sd_1 <- ggplot(dt_ioi_trim_sd_1, aes(x = IOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5)
plot(p_ioi_hist_sd_1)

p_ioi_box_sd_1 <- ggboxplot(dt_ioi_trim_sd_1, x = "Skill", y = "IOI", color = "Condition")
p_ioi_box_sd_1 <- ggpar(p_ioi_box_sd_1, ylab = "IOI (ms)")
plot(p_ioi_box_sd_1)

# Save plots
# png files
ggsave("./trimmed/ioi_hist.png", plot = p_ioi_hist, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_hist_sd_1.png", plot = p_ioi_hist_sd_1, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_box.png", plot = p_ioi_box, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_box_sd_1.png", plot = p_ioi_box_sd_1, dpi = 600, width = 5, height = 4)

# Export a txt file for dt_ioi_trim_sd
fwrite(dt_ioi_trim_sd_1, file = "./trimmed/data_ioi_1.txt", row.names = F)

####################################
# 2. > +- 3SD per condition
####################################
# exclude ioi > +- 3SD (per condition)
ioi_skill <- dt_ioi_subset[, .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = Grouping]

dt_ioi_trim_sd_2 <- data.table()
for (grouping in unique(ioi_skill$Grouping)){
  upper_ioi_2 <- ioi_skill[Grouping == grouping]$Mean+3*ioi_skill[Grouping == grouping]$SD
  lower_ioi_2 <- ioi_skill[Grouping == grouping]$Mean-3*ioi_skill[Grouping == grouping]$SD
  dt_current <- dt_ioi_subset[Grouping == grouping & IOI < upper_ioi_2 & IOI > lower_ioi_2]
  dt_ioi_trim_sd_2 <- rbind(dt_ioi_trim_sd_2, dt_current)
}

removed_ioi_2 <- nrow(dt_ioi_subset)-nrow(dt_ioi_trim_sd_2)
proportion_ioi_2 <- round(removed_ioi_2/nrow(dt_ioi_subset), 5)
write(sprintf("(Method 2)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_2, proportion_ioi_2*100), file = "./trimmed/outlier.txt", append = T)
print(sprintf("(Method 2)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_2, proportion_ioi_2*100))

# draw histogram and boxplot
p_ioi_hist_sd_2 <- ggplot(dt_ioi_trim_sd_2, aes(x = IOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5)
plot(p_ioi_hist_sd_2)

p_ioi_box_sd_2 <- ggboxplot(dt_ioi_trim_sd_2, x = "Skill", y = "IOI", color = "Condition")
p_ioi_box_sd_2 <- ggpar(p_ioi_box_sd_2, ylab = "IOI (ms)")
plot(p_ioi_box_sd_2)

# Save plots
# png files
ggsave("./trimmed/ioi_hist_sd_2.png", plot = p_ioi_hist_sd_2, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_box_sd_2.png", plot = p_ioi_box_sd_2, dpi = 600, width = 5, height = 4)

# Export a txt file for dt_ioi_trim_sd
fwrite(dt_ioi_trim_sd_2, file = "./trimmed/data_ioi_2.txt", row.names = F)

####################################
# 3. > +- 3SD across the conditions
# separately for Subcomponent Change
# (YES or NO - as a factor)
####################################
# evaluate whether Boundary (i.e., LtoS, StoL, FtoP, PtoF) or not
dt_ioi_subset$Boundary <- "No"
dt_ioi_subset[Subcomponent == "LtoS" | Subcomponent == "StoL" | Subcomponent == "FtoP" | Subcomponent == "PtoF"]$Boundary <- "Yes"

# exclude ioi > +- 3SD (separately for each Boundary)
ioi_boundary <- dt_ioi_subset[, .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = Boundary]

dt_ioi_trim_sd_3 <- data.table()
for (boundary in unique(ioi_boundary$Boundary)){
  upper_ioi_3 <- ioi_boundary[Boundary == boundary]$Mean+3*ioi_boundary[Boundary == boundary]$SD
  lower_ioi_3 <- ioi_boundary[Boundary == boundary]$Mean-3*ioi_boundary[Boundary == boundary]$SD
  dt_current <- dt_ioi_subset[Boundary == boundary & IOI < upper_ioi_3 & IOI > lower_ioi_3]
  dt_ioi_trim_sd_3 <- rbind(dt_ioi_trim_sd_3, dt_current)
}

removed_ioi_3 <- nrow(dt_ioi_subset)-nrow(dt_ioi_trim_sd_3)
proportion_ioi_3 <- round(removed_ioi_3/nrow(dt_ioi_subset), 5)
write(sprintf("(Method 3)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_3, proportion_ioi_3*100), file = "./trimmed/outlier.txt", append = T)
print(sprintf("(Method 3)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_3, proportion_ioi_3*100))

# draw histogram and boxplot
p_ioi_hist_sd_3 <- ggplot(dt_ioi_trim_sd_3, aes(x = IOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5)
plot(p_ioi_hist_sd_3)

p_ioi_box_sd_3 <- ggboxplot(dt_ioi_trim_sd_3, x = "Boundary", y = "IOI", color = "Condition")
p_ioi_box_sd_3 <- ggpar(p_ioi_box_sd_3, ylab = "IOI (ms)")
plot(p_ioi_box_sd_3)

# Save plots
# png files
ggsave("./trimmed/ioi_hist_sd_3.png", plot = p_ioi_hist_sd_3, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_box_sd_3.png", plot = p_ioi_box_sd_3, dpi = 600, width = 5, height = 4)

# Export a txt file for dt_ioi_trim_sd
fwrite(dt_ioi_trim_sd_3, file = "./trimmed/data_ioi_3.txt", row.names = F)
