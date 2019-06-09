#!/usr/local/bin/R
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 10/04/2019
# This script calculates each dependent variable (IOI, KOT, KV) and removes outliers.
# GitHub repo (private): https://github.com/atsukotominaga/teaching-v1.0/tree/master/script/R 

####################################
#  Requirements
####################################
# !!! Set working directory to file source location !!!

# Install and load required packages
# data manipulation
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
# plot
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}

# Create necessary folders if not exist
# 2_trimmed
if (!file.exists("2_trimmed")){
  dir.create("2_trimmed")
}

# 2_trimmed/plot
if (!file.exists("2_trimmed/plot")){
  dir.create("2_trimmed/plot")
}
# 2_trimmed/plot/ioi
if (!file.exists("2_trimmed/plot/ioi/")){
  dir.create("2_trimmed/plot/ioi")
}
# 2_trimmed/plot/kot
if (!file.exists("2_trimmed/plot/kot/")){
  dir.create("2_trimmed/plot/kot")
}
# 2_trimmed/plot/vel
if (!file.exists("2_trimmed/plot/vel/")){
  dir.create("2_trimmed/plot/vel")
}

####################################
# Reading and formatting data
####################################
# Read filtered csv files
df_all <- read.csv("./1_filtered/data_analysis.csv", header = T, sep = ",", dec = ".") # clear data without pitch errors
df_exc <- read.csv("./1_filtered/data_errorRate.csv", header = T, sep = ",", dec = ".") # exclusion criteria

# Exclude participants with more than 10% errors
include <- df_exc$SubNr[df_exc$LessThan10 == "include"]

# Data frame with only included participants
df_analysis <- data.frame()
for (subnr in include){
  df_current <- df_all %>% dplyr::filter(SubNr == subnr)
  df_analysis <- rbind(df_analysis, df_current)
}

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
# Calculate IOIs
df_ioi <- df_analysis %>% dplyr::filter(Key_OnOff == 1)
df_ioi$IOI <- diff(c(0, df_ioi$TimeStamp))

# Remove the first note
df_ioi <- df_ioi %>% dplyr::filter(NoteNr != 17)

# Assign a sequence number for each tone
df_ioi$Interval <- rep(1:66, length(df_ioi$NoteNr)/66)

# Asssign Subcomponents
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

# Assign Skill Change
for (number in change_1){
  df_ioi$Subcomponent[df_ioi$Skill == "articulation" & df_ioi$Interval == number] <- "LtoS"
  df_ioi$Subcomponent[df_ioi$Skill == "dynamics" & df_ioi$Interval == number] <- "FtoP"
}
for (number in change_2){
  df_ioi$Subcomponent[df_ioi$Skill == "articulation" & df_ioi$Interval == number] <- "StoL"
  df_ioi$Subcomponent[df_ioi$Skill == "dynamics" & df_ioi$Interval == number] <- "PtoF"
}

# Add a grouping name
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
# Exclude irrelevant notes
df_subset <- subset(df_ioi, df_ioi$Interval != 32 & df_ioi$Interval != 33 & df_ioi$Interval != 65 & df_ioi$Interval != 66)

# Draw histogram
p_hist <- ggplot(df_subset, aes(x = IOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

# Exclude deviated participants (>+3/<-3SD)
ioi_subject <- aggregate(IOI~SubNr, data = df_subset,
                      FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
ioi_subject <- cbind(ioi_subject, as.data.frame(ioi_subject[,2]))
mean = mean(ioi_subject$mean)
sd = sd(ioi_subject$mean)
exclude = c() # excluded participants
for (subnr in unique(ioi_subject$SubNr)){
  if (ioi_subject$mean[ioi_subject$SubNr == subnr] > mean+3*sd | 
      ioi_subject$mean[ioi_subject$SubNr == subnr] < mean-3*sd){
    exclude = c(exclude, subnr)
    print(sprintf("Exclude participant %i", subnr))
  }
}

# Exclude participants based on IOI deviation
df_exc$Final <- df_exc$LessThan10
for (subject in exclude){
  df_exc$Final[df_exc$SubNr == subject] <- "exclude"
}

df_trim <- data.frame()
if (length(exclude) != 0){ # if a vector is not 0
  for (subnr in exclude){
    df_trim <- rbind(df_trim, df_subset %>% dplyr::filter(SubNr != subnr))
  } 
} else {
  df_trim <- df_subset
}

# Update a csv file for df_exc
write.csv(df_exc, file = "./1_filtered/data_errorRate.csv", row.names = F)

# Exclude ioi > +- 3SD (across the conditions)
upper <- mean(df_trim$IOI)+3*sd(df_subset$IOI)
lower <- mean(df_trim$IOI)-3*sd(df_subset$IOI)
df_trim_sd <- df_trim %>% dplyr::filter(IOI < upper & IOI > lower)
removed_ioi <- nrow(df_trim)-nrow(df_trim_sd)
proportion_ioi <- round(removed_ioi/nrow(df_trim), 5)
write(sprintf("IOI: Remove %i trials beyond +- 3SD / %f percent", removed_ioi, proportion_ioi), file = "./2_trimmed/outlier.txt", append = T) # Export the results as a txt file
print(sprintf("IOI: Remove %i trials beyond +- 3SD / %f percent", removed_ioi, proportion_ioi))

p_hist_sd <- ggplot(df_trim_sd, aes(x = IOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

p_box_sd <- ggboxplot(df_trim_sd, x = "Skill", y = "IOI", color = "Condition")
p_box_sd <- ggpar(p_box_sd, ylab = "IOI (ms)")

# Save plots
# png files
ggsave("./2_trimmed/plot/ioi/p_hist.png", plot = p_hist, dpi = 600, width = 5, height = 4)
ggsave("./2_trimmed/plot/ioi/p_hist_sd.png", plot = p_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./2_trimmed/plot/ioi/p_box_sd.png", plot = p_box_sd, dpi = 600, width = 5, height = 4)

# Export a csv file for df_trimmed
write.csv(df_trim_sd, file = "./2_trimmed/data_ioi.csv", row.names = F)

####################################
# Key Overlap Time - articulation
####################################
df_exc <- read.csv("./1_filtered/data_errorRate.csv", header = T, sep = ",", dec = ".") # exclusion criteria

# Exclude participants
include <- df_exc$SubNr[df_exc$Final == "include"]

# Data frame with only included participants
df_analysis <- data.frame()
for (subnr in include){
  df_current <- df_all %>% dplyr::filter(SubNr == subnr)
  df_analysis <- rbind(df_analysis, df_current)
}

df_onset <- df_analysis %>% dplyr::filter(Key_OnOff == 1)
df_offset <- df_analysis %>% dplyr::filter(Key_OnOff == 0)

# Offset 1 - Onset 2
df_onset$KOT <- NA
for (row in 1:length(df_onset$NoteNr)){
  if (row < length(df_onset$NoteNr)){
    df_onset$KOT[row+1] <- df_offset$TimeStamp[row] - df_onset$TimeStamp[row+1] # offset(n) - onset(n+1)
  }
}

# Remove the first note
df_kot <- df_onset %>% dplyr::filter(NoteNr != 17)

# Assign a sequence number for each tone
df_kot$Interval <- rep(1:66, length(df_kot$NoteNr)/66)

# Assign Subcomponents
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
df_subset <- subset(df_kot, df_kot$Interval != 32 & df_kot$Interval != 33 & df_kot$Interval != 65 & df_kot$Interval != 66) # Exclude irrelevant notes
 
# Draw histogram and
p_hist <- ggplot(df_subset, aes(x = KOT, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

# Exclude kot > +- 3SD (within a given condition)
kot_subcomponent <- aggregate(KOT~Subcomponent, data = df_subset,
                          FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
kot_subcomponent <- cbind(kot_subcomponent, as.data.frame(kot_subcomponent[,2]))
df_trim_sd <- data.frame()
for (subcomponent in unique(df_subset$Subcomponent)){
  upper = kot_subcomponent$mean[kot_subcomponent$Subcomponent == subcomponent]+3*kot_subcomponent$sd[kot_subcomponent$Subcomponent == subcomponent]
  lower = kot_subcomponent$mean[kot_subcomponent$Subcomponent == subcomponent]-3*kot_subcomponent$sd[kot_subcomponent$Subcomponent == subcomponent]
  df_current <- df_subset %>% dplyr::filter(Subcomponent == subcomponent & KOT < upper & KOT > lower)
  df_trim_sd <- rbind(df_trim_sd, df_current)
}
removed_kot <- nrow(df_subset)-nrow(df_trim_sd)
proportion_kot <- removed_kot/nrow(df_subset)
write(sprintf("KOT: Remove %i trials beyond +- 3SD / %f percent", removed_kot, proportion_kot), file = "./2_trimmed/outlier.txt", append = T)
print(sprintf("KOT: Remove %i trials beyond +- 3SD / %f percent", removed_kot, proportion_kot))

# Sort by RowNr
df_trim_sd <- df_trim_sd[order(df_trim_sd$RowNr),]

p_hist_sd <- ggplot(df_trim_sd, aes(x = KOT, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

p_box_sd <- ggboxplot(df_trim_sd, x = "Skill", y = "KOT", color = "Condition")
p_box_sd <- ggpar(p_box_sd, ylab = "KOT (ms)")

# Save plots
# png files
ggsave("./2_trimmed//plot/kot/p_hist.png", plot = p_hist, dpi = 600, width = 5, height = 4)
ggsave("./2_trimmed//plot/kot/p_hist_sd.png", plot = p_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./2_trimmed//plot/kot/p_box_sd.png", plot = p_box_sd, dpi = 600, width = 5, height = 4)

# Export a csv file for df_trimmed
write.csv(df_trim_sd, file = "./2_trimmed/data_kot.csv", row.names = F)

####################################
# Key Velocity - dynamics
####################################
df_exc <- read.csv("./1_filtered/data_errorRate.csv", header = T, sep = ",", dec = ".") # exclusion criteria

# Exclude participants
include <- df_exc$SubNr[df_exc$Final == "include"]

# Calculate Acc (acceleration - velocity difference between notes)
df_vel <- df_analysis %>% dplyr::filter(Key_OnOff == 1)
df_vel$Acc <- diff(c(0, df_vel$Velocity))

# Remove the first note
df_vel_acc <- df_vel %>% dplyr::filter(NoteNr != 17)
df_vel$Acc <- NULL # Remove Acc from df_vel

# Assign a sequence number for each tone / interval
df_vel$Note <- rep(1:67, length(df_vel$NoteNr)/67) # for vel_seq
df_vel_acc$Interval <- rep(1:66, length(df_vel_acc$NoteNr)/66) # for vel_acc_seq

# Assign Subcomponents
# For each note
df_vel$Subcomponent <- NA
# Legato
for (phrase in 1:length(ls_legato_2)){
  for (note in 1:length(ls_legato_2[[phrase]])){
    df_vel$Subcomponent[df_vel$Skill == "articulation" & df_vel$Note == ls_legato_2[[phrase]][note]] <- "Legato"
  }
}

# Staccato
for (phrase in 1:length(ls_staccato_2)){
  for (note in 1:length(ls_staccato_2[[phrase]])){
    df_vel$Subcomponent[df_vel$Skill == "articulation" & df_vel$Note == ls_staccato_2[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte_2)){
  for (note in 1:length(ls_forte_2[[phrase]])){
    df_vel$Subcomponent[df_vel$Skill == "dynamics" & df_vel$Note == ls_forte_2[[phrase]][note]] <- "Forte"
  }
}

# Piano
for (phrase in 1:length(ls_piano_2)){
  for (note in 1:length(ls_piano_2[[phrase]])){
    df_vel$Subcomponent[df_vel$Skill == "dynamics" & df_vel$Note == ls_piano_2[[phrase]][note]] <- "Piano"
  }
}

# Assign Subcomponents
# For intervals
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
# Exclude irrelevant notes
df_subset <- subset(df_vel, df_vel$Note != 33 & df_vel$Note != 66 & df_vel$Note != 67)
df_subset_acc <- subset(df_vel_acc, df_vel_acc$Interval != 32 & df_vel_acc$Interval != 33 & df_vel_acc$Interval != 65 & df_vel_acc$Interval != 66)

# Draw histogram
p_hist <- ggplot(df_subset, aes(x = Velocity, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

p_hist_acc <- ggplot(df_subset_acc, aes(x = Acc, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

# Exclude vel > +- 3SD (within a given condition)
vel_subcomponent <- aggregate(Velocity~Subcomponent, data = df_subset,
                          FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_subcomponent <- cbind(vel_subcomponent, as.data.frame(vel_subcomponent[,2]))
df_trim_sd <- data.frame()
for (subcomponent in unique(df_subset$Subcomponent)){
  upper = vel_subcomponent$mean[vel_subcomponent$Subcomponent == subcomponent]+3*vel_subcomponent$sd[vel_subcomponent$Subcomponent == subcomponent]
  lower = vel_subcomponent$mean[vel_subcomponent$Subcomponent == subcomponent]-3*vel_subcomponent$sd[vel_subcomponent$Subcomponent == subcomponent]
  df_current <- df_subset %>% dplyr::filter(Subcomponent == subcomponent & Velocity < upper & Velocity > lower)
  df_trim_sd <- rbind(df_trim_sd, df_current)
}
removed_kv <- nrow(df_subset)-nrow(df_trim_sd)
proportion_kv <- removed_kv/nrow(df_subset)
write(sprintf("Velocity - Remove %i trials beyond +- 3SD / %f percent", removed_kv, proportion_kv), file = "./2_trimmed/outlier.txt", append = T)
print(sprintf("Velocity - Remove %i trials beyond +- 3SD / %f percent", removed_kv, proportion_kv))

# Exclude vel_acc > +- 3SD (within a given condition)
vel_subcomponent_acc <- aggregate(Acc~Subcomponent, data = df_subset_acc,
                              FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_subcomponent_acc <- cbind(vel_subcomponent_acc, as.data.frame(vel_subcomponent_acc[,2]))
df_trim_sd_acc <- data.frame()
for (subcomponent in unique(df_subset_acc$Subcomponent)){
  upper = vel_subcomponent_acc$mean[vel_subcomponent_acc$Subcomponent == subcomponent]+3*vel_subcomponent_acc$sd[vel_subcomponent_acc$Subcomponent == subcomponent]
  lower = vel_subcomponent_acc$mean[vel_subcomponent_acc$Subcomponent == subcomponent]-3*vel_subcomponent_acc$sd[vel_subcomponent_acc$Subcomponent == subcomponent]
  df_current <- df_subset_acc %>% dplyr::filter(Subcomponent == subcomponent & Acc < upper & Acc > lower)
  df_trim_sd_acc <- rbind(df_trim_sd_acc, df_current)
}
removed_acc <- nrow(df_subset_acc)-nrow(df_trim_sd_acc)
proportion <- removed_acc/nrow(df_subset_acc)
write(sprintf("VelocityAcc - Remove %i trials beyond +- 3SD / %f percent", removed_acc, proportion), file = "./2_trimmed/outlier.txt", append = T)
print(sprintf("VelocityAcc - Remove %i trials beyond +- 3SD / %f percent", removed_acc, proportion))

p_hist_sd <- ggplot(df_trim_sd, aes(x = Velocity, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

p_box_sd <- ggboxplot(df_trim_sd, x = "Skill", y = "Velocity", color = "Condition")
p_box_sd <- ggpar(p_box_sd, ylab = "Velocity (0-127)")

p_hist_sd_acc <- ggplot(df_trim_sd_acc, aes(x = Acc, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

p_box_sd_acc <- ggboxplot(df_trim_sd_acc, x = "Skill", y = "Acc", color = "Condition")
p_box_sd_acc <- ggpar(p_box_sd_acc, ylab = "Acceleration")

# png files
ggsave("./2_trimmed/plot/vel/p_hist.png", plot = p_hist, dpi = 600, width = 5, height = 4)
ggsave("./2_trimmed/plot/vel/p_hist_acc.png", plot = p_hist_acc, dpi = 600, width = 5, height = 4)
ggsave("./2_trimmed/plot/vel/p_hist_sd.png", plot = p_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./2_trimmed/plot/vel/p_box_sd.png", plot = p_box_sd, dpi = 600, width = 5, height = 4)
ggsave("./2_trimmed/plot/vel/p_hist_sd_acc.png", plot = p_hist_sd_acc, dpi = 600, width = 5, height = 4)
ggsave("./2_trimmed/plot/vel/p_box_sd_acc.png", plot = p_box_sd_acc, dpi = 600, width = 5, height = 4)

# Export a csv file for df_trimmed
write.csv(df_trim_sd, file = "./2_trimmed/data_vel.csv", row.names = F)
write.csv(df_trim_sd_acc, file = "./2_trimmed/data_vel_acc.csv", row.names = F)
