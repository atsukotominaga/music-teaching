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
dt_onset <- fread(file = "./filtered/dt_correct_onset.txt", header = T)
dt_offset <- fread(file = "./filtered/dt_correct_offset.txt", header = T)

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
# Inter-Onset intervals
####################################
# calculate IOIs
dt_ioi <- dt_onset
dt_ioi$IOI <- diff(c(0, dt_ioi$TimeStamp))
# convert bpm to ms
dt_ioi[Tempo == 120]$Tempo <- 250
dt_ioi[Tempo == 110]$Tempo <- 273
dt_ioi[Tempo == 100]$Tempo <- 300
# normalise IOIs
dt_ioi$normIOI <- dt_ioi$IOI/dt_ioi$Tempo

# remove the first note
dt_ioi <- dt_ioi[RowNr != 1]

# assign Interval
dt_ioi$Interval <- rep(1:71, nrow(dt_ioi)/71)

# assign Subcomponents
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
# exclude irrelevant notes (Subcomponent == NA means not 8th notes / normIOI == NA means a missing value)
dt_ioi_subset <- dt_ioi[Subcomponent != "NA" & !is.na(normIOI)]

# draw histogram and boxplot
p_ioi_hist <- ggplot(dt_ioi_subset, aes(x = normIOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .01)
plot(p_ioi_hist)

p_ioi_box <- ggboxplot(dt_ioi_subset, x = "Skill", y = "normIOI", color = "Condition")
p_ioi_box <- ggpar(p_ioi_box, ylab = "Normalised IOI (IOI/Tempo)")
plot(p_ioi_box)

####################################
# exclude deviated participants
####################################
ioi_summary <- dt_ioi_subset[, .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = .(SubNr)]
# exclude tempo deviated participants
ioi_summary$Include <- "No"
ioi_summary[Mean < mean(ioi_summary$Mean)+3*sd(ioi_summary$Mean) & Mean > mean(ioi_summary$Mean)-3*sd(ioi_summary$Mean)]$Include <- "Yes"
# No excluded participants

####################################
# 1. > +- 3SD across the conditions
####################################
# exclude ioi > +- 3SD (across the conditions)
upper_ioi_1 <- mean(dt_ioi_subset$normIOI)+3*sd(dt_ioi_subset$normIOI)
lower_ioi_1 <- mean(dt_ioi_subset$normIOI)-3*sd(dt_ioi_subset$normIOI)
dt_ioi_trim_sd_1 <- dt_ioi_subset[normIOI < upper_ioi_1 & normIOI > lower_ioi_1]
removed_ioi_1 <- nrow(dt_ioi_subset)-nrow(dt_ioi_trim_sd_1)
proportion_ioi_1 <- round(removed_ioi_1/nrow(dt_ioi_subset), 5)
write(sprintf("(Method 1)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_1, proportion_ioi_1*100), file = "./trimmed/outlier.txt", append = T)
print(sprintf("(Method 1)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_1, proportion_ioi_1*100))

# draw histogram and boxplot
p_ioi_hist_sd_1 <- ggplot(dt_ioi_trim_sd_1, aes(x = normIOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .01)
plot(p_ioi_hist_sd_1)

p_ioi_box_sd_1 <- ggboxplot(dt_ioi_trim_sd_1, x = "Skill", y = "normIOI", color = "Condition")
p_ioi_box_sd_1 <- ggpar(p_ioi_box_sd_1, ylab = "Normalised IOI (IOI/Tempo)")
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
ioi_skill <- dt_ioi_subset[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = Grouping]

dt_ioi_trim_sd_2 <- data.table()
for (grouping in unique(ioi_skill$Grouping)){
  upper_ioi_2 <- ioi_skill[Grouping == grouping]$Mean+3*ioi_skill[Grouping == grouping]$SD
  lower_ioi_2 <- ioi_skill[Grouping == grouping]$Mean-3*ioi_skill[Grouping == grouping]$SD
  dt_current <- dt_ioi_subset[Grouping == grouping & normIOI < upper_ioi_2 & normIOI > lower_ioi_2]
  dt_ioi_trim_sd_2 <- rbind(dt_ioi_trim_sd_2, dt_current)
}

removed_ioi_2 <- nrow(dt_ioi_subset)-nrow(dt_ioi_trim_sd_2)
proportion_ioi_2 <- round(removed_ioi_2/nrow(dt_ioi_subset), 5)
write(sprintf("(Method 2)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_2, proportion_ioi_2*100), file = "./trimmed/outlier.txt", append = T)
print(sprintf("(Method 2)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_2, proportion_ioi_2*100))

# draw histogram and boxplot
p_ioi_hist_sd_2 <- ggplot(dt_ioi_trim_sd_2, aes(x = normIOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .01)
plot(p_ioi_hist_sd_2)

p_ioi_box_sd_2 <- ggboxplot(dt_ioi_trim_sd_2, x = "Skill", y = "normIOI", color = "Condition")
p_ioi_box_sd_2 <- ggpar(p_ioi_box_sd_2, ylab = "Normalised IOI (IOI/Tempo)")
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
ioi_boundary <- dt_ioi_subset[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = Boundary]

dt_ioi_trim_sd_3 <- data.table()
for (boundary in unique(ioi_boundary$Boundary)){
  upper_ioi_3 <- ioi_boundary[Boundary == boundary]$Mean+3*ioi_boundary[Boundary == boundary]$SD
  lower_ioi_3 <- ioi_boundary[Boundary == boundary]$Mean-3*ioi_boundary[Boundary == boundary]$SD
  dt_current <- dt_ioi_subset[Boundary == boundary & normIOI < upper_ioi_3 & normIOI > lower_ioi_3]
  dt_ioi_trim_sd_3 <- rbind(dt_ioi_trim_sd_3, dt_current)
}

removed_ioi_3 <- nrow(dt_ioi_subset)-nrow(dt_ioi_trim_sd_3)
proportion_ioi_3 <- round(removed_ioi_3/nrow(dt_ioi_subset), 5)
write(sprintf("(Method 3)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_3, proportion_ioi_3*100), file = "./trimmed/outlier.txt", append = T)
print(sprintf("(Method 3)IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi_3, proportion_ioi_3*100))

# draw histogram and boxplot
p_ioi_hist_sd_3 <- ggplot(dt_ioi_trim_sd_3, aes(x = normIOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .01)
plot(p_ioi_hist_sd_3)

p_ioi_box_sd_3 <- ggboxplot(dt_ioi_trim_sd_3, x = "Skill", y = "normIOI", color = "Condition")
p_ioi_box_sd_3 <- ggpar(p_ioi_box_sd_3, ylab = "Normalised IOI (IOI/Tempo)")
plot(p_ioi_box_sd_3)

# Save plots
# png files
ggsave("./trimmed/ioi_hist_sd_3.png", plot = p_ioi_hist_sd_3, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_box_sd_3.png", plot = p_ioi_box_sd_3, dpi = 600, width = 5, height = 4)

# Export a txt file for dt_ioi_trim_sd
fwrite(dt_ioi_trim_sd_3, file = "./trimmed/data_ioi_3.txt", row.names = F)
