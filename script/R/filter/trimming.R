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
df_onset <- read.csv(file.path("./filtered/data_correct.csv"), header = T)

# read functions
source("./function.R")

# make sure there is no pitch errors - outputs should be only missing trials
ls_removed_onset <- pitch_remover(df_onset, df_ideal)

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
# define Component change (StoL, PtoF)
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
# Remove outliers
####################################
# exclude irrelevant notes
df_ioi_subset <- subset(df_ioi, !is.na(df_ioi$Subcomponent) & !is.na(df_ioi$normIOI))

# draw histogram and
p_ioi_hist <- ggplot(df_ioi_subset, aes(x = normIOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .05)
plot(p_ioi_hist)

p_ioi_box <- ggboxplot(df_ioi_subset, x = "Skill", y = "normIOI", color = "Condition")
p_ioi_box <- ggpar(p_ioibox, ylab = "Normalised IOI (IOI/Tempo)")
plot(p_ioi_box)

# exclude ioi > +- 3SD (across the conditions)
upper <- mean(df_ioi_subset$normIOI)+3*sd(df_ioi_subset$normIOI)
lower <- mean(df_ioi_subset$normIOI)-3*sd(df_ioi_subset$normIOI)
df_ioi_trim_sd <- df_ioi_subset %>% dplyr::filter(normIOI < upper & normIOI > lower)
removed_ioi <- nrow(df_ioi_subset)-nrow(df_ioi_trim_sd)
proportion_ioi <- round(removed_ioi/nrow(df_ioi_subset), 5)
write(sprintf("IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi, proportion_ioi*100), file = "./trimmed/outlier.txt", append = T) # Export the results as a txt file
print(sprintf("IOI: Remove %i responses beyond +- 3SD / %f percent", removed_ioi, proportion_ioi*100))

p_ioi_hist_sd <- ggplot(df_ioi_trim_sd, aes(x = normIOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .01)
plot(p_ioi_hist_sd)

p_ioi_box_sd <- ggboxplot(df_ioi_trim_sd, x = "Skill", y = "normIOI", color = "Condition")
p_ioi_box_sd <- ggpar(p_box_sd, ylab = "Normalised IOI (IOI/Tempo)")
plot(p_ioi_box_sd)

# Save plots
# png files
ggsave("./trimmed/ioi_hist.png", plot = p_ioi_hist, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_hist_sd.png", plot = p_ioi_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_box.png", plot = p_ioi_box, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/ioi_box_sd.png", plot = p_ioi_box_sd, dpi = 600, width = 5, height = 4)

# Export a csv file for df_ioi_trim_sd
write.csv(df_ioi_trim_sd, file = "./trimmed/data_ioi.csv", row.names = F)

####################################
# Key Velocity - dynamics
####################################
# calculate Acc (acceleration - velocity difference between notes)
df_vel <- df_onset
df_vel$Acc <- diff(c(0, df_vel$Velocity))

# assign NoteNr
df_vel$NoteNr <- rep(1:72, nrow(df_vel)/72)

# remove the first note
df_vel_acc <- df_vel %>% dplyr::filter(NoteNr != 1)
df_vel$Acc <- NULL # remove Acc from df_vel

# assign Interval
df_vel_acc$Interval <- rep(1:71, nrow(df_vel_acc)/71)

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
df_vel_acc$Subcomponent <- NA
# Legato
for (phrase in 1:length(ls_legato)){
  for (note in 1:length(ls_legato[[phrase]])){
    df_vel_acc$Subcomponent[df_vel_acc$Skill == "articulation" & df_vel_acc$Interval == ls_legato[[phrase]][note]] <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato)){
  for (note in 1:length(ls_staccato[[phrase]])){
    df_vel_acc$Subcomponent[df_vel_acc$Skill == "articulation" & df_vel_acc$Interval == ls_staccato[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte)){
  for (note in 1:length(ls_forte[[phrase]])){
    df_vel_acc$Subcomponent[df_vel_acc$Skill == "dynamics" & df_vel_acc$Interval == ls_forte[[phrase]][note]] <- "Forte"
  }
}
# Piano
for (phrase in 1:length(ls_piano)){
  for (note in 1:length(ls_piano[[phrase]])){
    df_vel_acc$Subcomponent[df_vel_acc$Skill == "dynamics" & df_vel_acc$Interval == ls_piano[[phrase]][note]] <- "Piano"
  }
}

# Assign Skill Change
for (i in change_1){
  df_vel_acc$Subcomponent[df_vel_acc$Skill == "articulation" & df_vel_acc$Interval == i] <- "LtoS"
  df_vel_acc$Subcomponent[df_vel_acc$Skill == "dynamics" & df_vel_acc$Interval == i] <- "FtoP"
}
for (i in change_2){
  df_vel_acc$Subcomponent[df_vel_acc$Skill == "articulation" & df_vel_acc$Interval == i] <- "StoL"
  df_vel_acc$Subcomponent[df_vel_acc$Skill == "dynamics" & df_vel_acc$Interval == i] <- "PtoF"
}

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    df_vel$Grouping[df_vel$Condition == ls_grouping$Condition[cond] & df_vel$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    df_vel_acc$Grouping[df_vel_acc$Condition == ls_grouping$Condition[cond] & df_vel_acc$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

####################################
# Remove outliers
####################################
# exclude irrelevant notes
df_vel_subset <- subset(df_vel, !is.na(df_vel$Subcomponent) & !is.na(df_vel$Velocity))
df_vel_acc_subset <- subset(df_vel_acc, !is.na(df_vel_acc$Subcomponent) & !is.na(df_vel_acc$Acc))

###### df_vel
# draw histogram
p_vel_hist <- ggplot(df_vel_subset, aes(x = Velocity, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 1)
plot(p_vel_hist)

p_vel_box <- ggboxplot(df_vel_subset, x = "Skill", y = "Velocity", color = "Condition")
p_vel_box <- ggpar(p_vel_box, ylab = "Velocity (0-127)")
plot(p_vel_box)

# exclude velocity > +- 3SD (across the conditions)
upper <- mean(df_vel_subset$Velocity)+3*sd(df_vel_subset$Velocity)
lower <- mean(df_vel_subset$Velocity)-3*sd(df_vel_subset$Velocity)
df_vel_trim_sd <- df_vel_subset %>% dplyr::filter(Velocity < upper & Velocity > lower)
removed_vel <- nrow(df_vel_subset)-nrow(df_vel_trim_sd)
proportion_vel <- round(removed_vel/nrow(df_vel_subset), 5)
write(sprintf("Velocity: Remove %i responses beyond +- 3SD / %f percent", removed_vel, proportion_vel*100), file = "./trimmed/outlier.txt", append = T) # Export the results as a txt file
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

# Export a csv file for df_trim_sd
write.csv(df_vel_trim_sd, file = "./trimmed/data_vel.csv", row.names = F)

###### df_vel_acc
# draw histogram
p_vel_acc_hist <- ggplot(df_vel_acc_subset, aes(x = Acc, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 1)
plot(p_vel_acc_hist)

p_vel_acc_box <- ggboxplot(df_vel_acc_subset, x = "Skill", y = "Acc", color = "Condition")
p_vel_acc_box <- ggpar(p_vel_acc_box, ylab = "Difference")
plot(p_vel_acc_box)

# exclude acc > +- 3SD (across the conditions)
upper <- mean(df_vel_acc_subset$Acc)+3*sd(df_vel_acc_subset$Acc)
lower <- mean(df_vel_acc_subset$Acc)-3*sd(df_vel_acc_subset$Acc)
df_vel_acc_trim_sd <- df_vel_acc_subset %>% dplyr::filter(Acc < upper & Acc > lower)
removed_vel_acc <- nrow(df_vel_acc_subset)-nrow(df_vel_acc_trim_sd)
proportion_vel_acc <- round(removed_vel_acc/nrow(df_vel_acc_subset), 5)
write(sprintf("Acc: Remove %i responses beyond +- 3SD / %f percent", removed_vel_acc, proportion_vel_acc*100), file = "./trimmed/outlier.txt", append = T) # Export the results as a txt file
print(sprintf("Acc: Remove %i responses beyond +- 3SD / %f percent", removed_vel_acc, proportion_vel_acc*100))

p_vel_acc_hist_sd <- ggplot(df_vel_acc_trim_sd, aes(x = Acc, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 1)
plot(p_vel_acc_hist_sd)

p_vel_acc_box_sd <- ggboxplot(df_vel_acc_trim_sd, x = "Skill", y = "Acc", color = "Condition")
p_vel_acc_box_sd <- ggpar(p_vel_acc_box_sd, ylab = "Difference")
plot(p_vel_acc_box_sd)

# Save plots
# png files
ggsave("./trimmed/vel_acc_hist.png", plot = p_vel_acc_hist, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_acc_hist_sd.png", plot = p_vel_acc_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_acc_box.png", plot = p_vel_acc_box, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/vel_acc_box_sd.png", plot = p_vel_acc_box_sd, dpi = 600, width = 5, height = 4)

# Export a csv file for df_trim_sd
write.csv(df_vel_acc_trim_sd, file = "./trimmed/data_vel_acc.csv", row.names = F)
