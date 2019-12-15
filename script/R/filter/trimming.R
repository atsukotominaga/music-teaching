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
p_ioi_box <- ggpar(p_ioi_box, ylab = "Normalised IOI (IOI/Tempo)")
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
p_ioi_box_sd <- ggpar(p_ioi_box_sd, ylab = "Normalised IOI (IOI/Tempo)")
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

# calculate KOR
df_kot$KOR <- df_kot$KOT/df_kot$Tempo

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

# Assign Skill Change
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

####################################
# Remove outliers
####################################
# exclude irrelevant notes
df_kot_subset <- subset(df_kot, !is.na(df_kot$Subcomponent) & !is.na(df_kot$KOR))

# draw histogram and boxplot
p_kot_hist <- ggplot(df_kot_subset, aes(x = KOR, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .05) +
  theme_classic()
plot(p_kot_hist)

p_kot_box <- ggboxplot(df_kot_subset, x = "Skill", y = "KOR", color = "Condition")
p_kot_box <- ggpar(p_kotbox, ylab = "Key-Overlap Ratio (KOT/Tempo)")
plot(p_kot_box)

# exclude kot > +- 3SD (within a given condition)
kot_subcomponent <- aggregate(KOR~Subcomponent, data = df_kot_subset,
                              FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
kot_subcomponent <- cbind(kot_subcomponent, as.data.frame(kot_subcomponent[,2]))
df_kot_trim_sd <- data.frame()
for (subcomponent in unique(df_kot_subset$Subcomponent)){
  upper = kot_subcomponent$mean[kot_subcomponent$Subcomponent == subcomponent]+3*kot_subcomponent$sd[kot_subcomponent$Subcomponent == subcomponent]
  lower = kot_subcomponent$mean[kot_subcomponent$Subcomponent == subcomponent]-3*kot_subcomponent$sd[kot_subcomponent$Subcomponent == subcomponent]
  df_current <- df_kot_subset %>% dplyr::filter(Subcomponent == subcomponent & KOR < upper & KOR > lower)
  df_kot_trim_sd <- rbind(df_kot_trim_sd, df_current)
}
removed_kot <- nrow(df_kot_subset)-nrow(df_kot_trim_sd)
proportion_kot <- round(removed_kot/nrow(df_kot_subset), 5)
write(sprintf("KOR: Remove %i responses beyond +- 3SD / %f percent", removed_kot, proportion_kot*100), file = "./trimmed/outlier.txt", append = T)
print(sprintf("KOR: Remove %i responses beyond +- 3SD / %f percent", removed_kot, proportion_kot*100))

# draw histogram and boxplot
p_kot_hist_sd <- ggplot(df_kot_trim_sd, aes(x = KOR, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = .05) +
  theme_classic()
plot(p_kot_hist_sd)

p_kot_box_sd <- ggboxplot(df_kot_trim_sd, x = "Skill", y = "KOR", color = "Condition")
p_kot_box_sd <- ggpar(p_kot_box_sd, ylab = "Key-Overlap Ratio (KOT/Tempo)")
plot(p_kot_box_sd)

# Save plots
# png files
ggsave("./trimmed/kot_hist.png", plot = p_kot_hist, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kot_hist_sd.png", plot = p_kot_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kot_box.png", plot = p_kot_box, dpi = 600, width = 5, height = 4)
ggsave("./trimmed/kot_box_sd.png", plot = p_kot_box_sd, dpi = 600, width = 5, height = 4)

# Export a csv file for df_kot_trim_sd
write.csv(df_kot_trim_sd, file = "./trimmed/data_kot.csv", row.names = F)

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
write(sprintf("Velocity: Remove %i responses beyond +- 3SD / %f percent", removed_vel, proportion_vel*100), file = "./trimmed/outlier.txt", append = T)
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

###### df_vel_acc
# draw histogram
p_vel_acc_hist <- ggplot(df_vel_acc_subset, aes(x = Acc, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 1)
plot(p_vel_acc_hist)

p_vel_acc_box <- ggboxplot(df_vel_acc_subset, x = "Skill", y = "Acc", color = "Condition")
p_vel_acc_box <- ggpar(p_vel_acc_box, ylab = "Difference")
plot(p_vel_acc_box)

# exclude vel > +- 3SD (within a given condition)
vel_acc_subcomponent <- aggregate(Acc~Subcomponent, data = df_vel_acc_subset,
                              FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_acc_subcomponent <- cbind(vel_acc_subcomponent, as.data.frame(vel_acc_subcomponent[,2]))
df_vel_acc_trim_sd <- data.frame()
for (subcomponent in unique(df_vel_acc_subset$Subcomponent)){
  upper = vel_acc_subcomponent$mean[vel_acc_subcomponent$Subcomponent == subcomponent]+3*vel_acc_subcomponent$sd[vel_acc_subcomponent$Subcomponent == subcomponent]
  lower = vel_acc_subcomponent$mean[vel_acc_subcomponent$Subcomponent == subcomponent]-3*vel_acc_subcomponent$sd[vel_acc_subcomponent$Subcomponent == subcomponent]
  df_current <- df_vel_acc_subset %>% dplyr::filter(Subcomponent == subcomponent & Acc < upper & Acc > lower)
  df_vel_acc_trim_sd <- rbind(df_vel_acc_trim_sd, df_current)
}
removed_vel_acc <- nrow(df_vel_acc_subset)-nrow(df_vel_acc_trim_sd)
proportion_vel_acc <- round(removed_vel_acc/nrow(df_vel_acc_subset), 5)
write(sprintf("Acc: Remove %i responses beyond +- 3SD / %f percent", removed_vel_acc, proportion_vel_acc*100), file = "./trimmed/outlier.txt", append = T)
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
