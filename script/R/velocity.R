#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 26/02/2019
# This script aggregate and plot data (Velocity)
# GitHub repo (private): https://github.com/atsukotominaga/expertpiano/tree/master/script/R 

####################################
#  Requirements
####################################
### !!! Set working directory to file source location !!!

### Install and load required packages
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}

### Create necessary folders if not exist
# plot
if (!file.exists("plot")){
  dir.create("plot")
}
# velocity
if (!file.exists("plot/velocity/")){
  dir.create("plot/velocity")
}

####################################
# Reading and formatting data
####################################
df_all <- read.csv("./csv/data_analysis.csv", header = T, sep = ",", dec = ".")
df_exc <- read.csv("./csv/data_errorRate.csv", header = T, sep = ",", dec = ".")

### Exclude participants
include <- df_exc$SubNr[df_exc$LessThan10 == "include"]

### Data frame with only included participants
df_analysis <- data.frame()
for (subnr in include){
  df_current <- df_all %>% dplyr::filter(SubNr == subnr)
  df_analysis <- rbind(df_analysis, df_current)
}

# ### Data frame for each individual
# subnr = 18
# df_analysis <- df_all %>% dplyr::filter(SubNr == subnr)

####################################
# Velocity - dynamics
####################################
### Calculate Acc (acceleration - velocity difference between notes)
df_vel <- df_analysis %>% dplyr::filter(Key_OnOff == 1)
df_vel$Acc <- diff(c(0, df_vel$Velocity))

### Remove the first note
df_vel_acc <- df_vel %>% dplyr::filter(NoteNr != 17)

### Assign a sequence number for each tone / interval
df_vel$Note <- rep(1:67, length(df_vel$NoteNr)/67) # for vel_seq
df_vel_acc$Interval <- rep(1:66, length(df_vel_acc$NoteNr)/66) # for vel_acc_seq

### Define SubSkills
# For each note (for df_vel)
ls_legato_1 <- list(c(1:8), c(17:24), c(42:49), c(58:65))
ls_staccato_1 <- list(c(9:16), c(25:32), c(34:41), c(50:57))
ls_forte_1 <- list(c(1:8), c(17:24), c(42:49), c(58:65))
ls_piano_1 <- list(c(9:16), c(25:32), c(34:41), c(50:57))
# For intervals (for df_vel_acc)
ls_legato_2 <- list(c(1:7), c(17:23), c(42:48), c(58:64))
ls_staccato_2 <- list(c(9:15), c(25:31), c(34:40), c(50:56))
ls_forte_2 <- list(c(1:7), c(17:23), c(42:48), c(58:64))
ls_piano_2 <- list(c(9:15), c(25:31), c(34:40), c(50:56))

### Assign SubSkills
# For each note (for df_vel)
df_vel$SubSkill <- NA
# Legato
for (phrase in 1:length(ls_legato_1)){
  for (note in 1:length(ls_legato_1[[phrase]])){
    df_vel$SubSkill[df_vel$Skill == "articulation" & df_vel$Note == ls_legato_1[[phrase]][note]] <- "Legato"
  }
}

# Staccato
for (phrase in 1:length(ls_staccato_1)){
  for (note in 1:length(ls_staccato_1[[phrase]])){
    df_vel$SubSkill[df_vel$Skill == "articulation" & df_vel$Note == ls_staccato_1[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte_1)){
  for (note in 1:length(ls_forte_1[[phrase]])){
    df_vel$SubSkill[df_vel$Skill == "dynamics" & df_vel$Note == ls_forte_1[[phrase]][note]] <- "Forte"
  }
}

# Piano
for (phrase in 1:length(ls_piano_1)){
  for (note in 1:length(ls_piano_1[[phrase]])){
    df_vel$SubSkill[df_vel$Skill == "dynamics" & df_vel$Note == ls_piano_1[[phrase]][note]] <- "Piano"
  }
}

# For intervals (for df_vel_acc)
# Legato
df_vel_acc$SubSkill <- NA
for (phrase in 1:length(ls_legato_2)){
  for (note in 1:length(ls_legato_2[[phrase]])){
    df_vel_acc$SubSkill[df_vel_acc$Skill == "articulation" & df_vel_acc$Interval == ls_legato_2[[phrase]][note]] <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato_2)){
  for (note in 1:length(ls_staccato_2[[phrase]])){
    df_vel_acc$SubSkill[df_vel_acc$Skill == "articulation" & df_vel_acc$Interval == ls_staccato_2[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte_2)){
  for (note in 1:length(ls_forte_2[[phrase]])){
    df_vel_acc$SubSkill[df_vel_acc$Skill == "dynamics" & df_vel_acc$Interval == ls_forte_2[[phrase]][note]] <- "Forte"
  }
}
# Piano
for (phrase in 1:length(ls_piano_2)){
  for (note in 1:length(ls_piano_2[[phrase]])){
    df_vel_acc$SubSkill[df_vel_acc$Skill == "dynamics" & df_vel_acc$Interval == ls_piano_2[[phrase]][note]] <- "Piano"
  }
}

# Define LtoS and FtoP
change_1 <- c(8, 24, 49)
# Define StoL and PtoF
change_2 <- c(16, 41, 57)

for (i in change_1){
  df_vel_acc$SubSkill[df_vel_acc$Skill == "articulation" & df_vel_acc$Interval == i] <- "LtoS"
  df_vel_acc$SubSkill[df_vel_acc$Skill == "dynamics" & df_vel_acc$Interval == i] <- "FtoP"
}
for (i in change_2){
  df_vel_acc$SubSkill[df_vel_acc$Skill == "articulation" & df_vel_acc$Interval == i] <- "StoL"
  df_vel_acc$SubSkill[df_vel_acc$Skill == "dynamics" & df_vel_acc$Interval == i] <- "PtoF"
}

### Average data
# Overall average
vel <- aggregate(Velocity~Condition*Skill, data = subset(df_vel, df_vel$Note != 33 & df_vel$Note != 66 & df_vel$Note != 67), 
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each sub-skill
vel_sub <- aggregate(Velocity~Condition*Skill*SubSkill, data = subset(df_vel, df_vel$Note != 33 & df_vel$Note != 66 & df_vel$Note != 67),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for the first note
vel_first <- aggregate(Velocity~Condition*Skill*SubSkill, data = subset(df_vel, df_vel$Note == 1 | df_vel$Note == 9 | df_vel$Note == 17 | df_vel$Note == 25
                                                                        | df_vel$Note == 34 | df_vel$Note == 42 | df_vel$Note == 50 | df_vel$Note == 58),
                       FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for the fist interval
vel_acc_first <- aggregate(Acc~Condition*Skill*SubSkill, data = subset(df_vel_acc, df_vel_acc$Interval == 8 | df_vel_acc$Interval == 16 | df_vel_acc$Interval == 24 | df_vel_acc$Interval == 41
                                                                        | df_vel_acc$Interval == 49 | df_vel_acc$Interval == 57),
                       FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
vel_seq <- aggregate(Velocity~Note*Condition*Skill, data = df_vel, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_seq[,4][vel_seq$Note == 33 | vel_seq$Note == 66 | vel_seq$Note == 67] <- NA
vel_acc_seq <- aggregate(Acc~Interval*Condition*Skill, data = df_vel_acc, 
                         FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_acc_seq[,4][vel_acc_seq$Interval == 32 | vel_acc_seq$Interval == 33 | vel_acc_seq$Interval == 65 | vel_acc_seq$Interval == 66] <- NA

# Create data frames
vel <- cbind(vel, as.data.frame(vel[,3]))
vel_sub <- cbind(vel_sub, as.data.frame(vel_sub[,4]))
vel_first <- cbind(vel_first, as.data.frame(vel_first[,4]))
vel_acc_first <- cbind(vel_acc_first, as.data.frame(vel_acc_first[,4]))
vel_seq <- cbind(vel_seq, as.data.frame(vel_seq[,4]))
vel_acc_seq <- cbind(vel_acc_seq, as.data.frame(vel_acc_seq[,4]))

# Add order info
vel_sub$Order[vel_sub$Skill == "articulation"] <- 1
vel_sub$Order[vel_sub$Skill == "dynamics"] <- 2
vel_acc_first$Order[vel_acc_first$Skill == "articulation"] <- 1
vel_acc_first$Order[vel_acc_first$Skill == "dynamics"] <- 2

# # Add a grouping name
# for (cond in 1:length(ls_grouping$Condition)){
#   for (skill in 1:length(ls_grouping$Skill)){
#     vel$Grouping[vel$Condition == ls_grouping$Condition[cond] & vel$Skill == ls_grouping$Skill[skill]] <- 
#       paste(ls_grouping$Condition[cond], "-", ls_grouping$Skill[skill], sep = "")
#     vel_seq$Grouping[vel_seq$Condition == ls_grouping$Condition[cond] & vel_seq$Skill == ls_grouping$Skill[skill]] <- 
#       paste(ls_grouping$Condition[cond], "-", ls_grouping$Skill[skill], sep = "")
#     vel_acc_seq$Grouping[vel_acc_seq$Condition == ls_grouping$Condition[cond] & vel_acc_seq$Skill == ls_grouping$Skill[skill]] <- 
#       paste(ls_grouping$Condition[cond], "-", ls_grouping$Skill[skill], sep = "")
#   }
# }

####################################
### Velocity plots
####################################
# Overall average for velocity
p_vel <- ggplot(data = vel, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = .2, position=position_dodge(.9)) + 
  labs(y = "Velocity (0-127)") + coord_cartesian(ylim = c(40, 80)) + 
  theme_classic()

# Average velocity for each sub-skill
p_vel_sub <- ggplot(data = vel_sub, aes(x = reorder(SubSkill, Order), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "Velocity (0-127)") + coord_cartesian(ylim = c(40, 90)) + 
  theme_classic()

# Average velocity (only first-note) for each sub-skill
p_vel_first <- ggplot(data = vel_first, aes(x = SubSkill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Velocity (0-127)") + coord_cartesian(ylim = c(40, 90)) + 
  theme_classic()

# Average velocity (only first-interval / skill change - e.g., legato to staccato) for each sub-skill
p_vel_acc_first <- ggplot(data = vel_acc_first, aes(x = reorder(SubSkill, Order), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(x = "Skill change", y = "Acceleration") + #coord_cartesian(ylim = c(0, 20)) + 
  theme_classic()

# plot graphs for each skill
p_vel_seq_f <- ggplot(data = vel_seq, aes(x = Note, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(0.05)) + 
  labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,67,1)) +
  theme_classic()

p_vel_acc_seq_f <- ggplot(data = vel_acc_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "Acceleration") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()

# plot all graphs
# p_vel_seq <- ggplot(data = vel_seq, aes(x = Note, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
#   geom_line() +
#   geom_point() +
#   geom_hline(data = vel, aes(yintercept = mean, colour = Grouping), linetype = "dashed") + # Mean Velocity for each grouping
#   geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
#                 position = position_dodge(0.05)) + 
#   labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,67,1)) +
#   theme_classic()

# p_vel_acc_seq <- ggplot(data = vel_acc_seq, aes(x = Interval, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
#                 position=position_dodge(.05)) + 
#   labs(x = "Interval", y = "Acceleration") + scale_x_continuous(breaks=seq(1,66,1)) +
#   theme_classic()

# Save plots
# png files
ggsave("./plot/velocity/p_vel.png", plot = p_vel, dpi = 600, width = 5, height = 4)
ggsave("./plot/velocity/p_vel_sub.png", plot = p_vel_sub, dpi = 600, width = 5, height = 4)
ggsave("./plot/velocity/p_vel_first.png", plot = p_vel_first, dpi = 600, width = 5, height = 4)
ggsave("./plot/velocity/p_vel_acc_first.png", plot = p_vel_acc_first, dpi = 600, width = 5, height = 4)
ggsave("./plot/velocity/p_vel_seq_f.png", plot = p_vel_seq_f, dpi = 600, width = 15, height = 4)
ggsave("./plot/velocity/p_vel_acc_seq_f.png", plot = p_vel_acc_seq_f, dpi = 600, width = 15, height = 4) 

# ggsave("./plot/velocity/p_vel_seq.png", plot = p_vel_seq, dpi = 600, width = 15, height = 4)
# ggsave("./plot/velocity/p_vel_acc_seq.png", plot = p_vel_acc_seq, dpi = 600, width = 15, height = 4)