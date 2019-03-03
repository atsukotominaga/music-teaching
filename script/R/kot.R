#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 26/02/2019
# This script aggregate and plot data (IOI)
# GitHub repo (private): https://github.com/atsukotominaga/expertpiano/tree/master/script/R 

####################################
#  Requirements
####################################
# !!! Set working directory to file source location !!!

# Install and load required packages
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}

# Create necessary folders if not exist
# plot
if (!file.exists("plot")){
  dir.create("plot")
}
# ioi
if (!file.exists("plot/kot/")){
  dir.create("plot/kot")
}

####################################
# Reading and formatting data
####################################
df_all <- read.csv('./csv/data_analysis.csv', header = T, sep = ",", dec = '.')
df_exc <- read.csv('./csv/data_errorRate.csv', header = T, sep = ",", dec = '.')

# Exclude participants
include <- df_exc$SubNr[df_exc$Exclude == 'include']

# Data frame with only included participants
df_analysis <- data.frame()
for (subnr in include){
  df_current <- df_all %>% dplyr::filter(SubNr == subnr)
  df_analysis <- rbind(df_analysis, df_current)
}

# # Data frame for each individual
# subnr = 18
# df_analysis <- df_all %>% dplyr::filter(SubNr == subnr)

####################################
# Key Overlap Time - articulation
####################################
df_onset <- df_analysis %>% dplyr::filter(Key_OnOff == 1)
df_offset <- df_analysis %>% dplyr::filter(Key_OnOff == 0)

# Offset 1 - Onset 2
df_onset$KOT <- NA
for (i in 1:length(df_onset$NoteNr)){
  if (i < length(df_onset$NoteNr)){
    df_onset$KOT[i+1] <- df_offset$TimeStamp[i] - df_onset$TimeStamp[i+1]
  }
}

# Remove the first note
df_kot <- df_onset %>% dplyr::filter(NoteNr != 17)

# Assign a sequence number for each tone
df_kot$Interval <- rep(1:66, length(df_kot$NoteNr)/66)

### Define SubSkills
# For intervals (for df_kot)
ls_legato <- list(c(1:7), c(17:23), c(42:48), c(58:64))
ls_staccato <- list(c(9:15), c(25:31), c(34:40), c(50:56))
ls_forte <- list(c(1:7), c(17:23), c(42:48), c(58:64))
ls_piano <- list(c(9:15), c(25:31), c(34:40), c(50:56))

# For intervals (for df_kot)
# Legato
df_kot$SubSkill <- NA
for (phrase in 1:length(ls_legato)){
  for (note in 1:length(ls_legato[[phrase]])){
    df_kot$SubSkill[df_kot$Skill == "articulation" & df_kot$Interval == ls_legato[[phrase]][note]] <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato)){
  for (note in 1:length(ls_staccato[[phrase]])){
    df_kot$SubSkill[df_kot$Skill == "articulation" & df_kot$Interval == ls_staccato[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte)){
  for (note in 1:length(ls_forte[[phrase]])){
    df_kot$SubSkill[df_kot$Skill == "dynamics" & df_kot$Interval == ls_forte[[phrase]][note]] <- "Forte"
  }
}
# Piano
for (phrase in 1:length(ls_piano)){
  for (note in 1:length(ls_piano[[phrase]])){
    df_kot$SubSkill[df_kot$Skill == "dynamics" & df_kot$Interval == ls_piano[[phrase]][note]] <- "Piano"
  }
}

# Define LtoS and FtoP
# change_1 <- c(8, 24, 49)
# # Define StoL and PtoF
# change_2 <- c(16, 41, 57)
# 
# for (i in change_1){
#   df_kot$SubSkill[df_kot$Skill == "articulation" & df_kot$Interval == i] <- "LtoS"
#   df_kot$SubSkill[df_kot$Skill == "dynamics" & df_kot$Interval == i] <- "FtoP"
# }
# for (i in change_2){
#   df_kot$SubSkill[df_kot$Skill == "articulation" & df_kot$Interval == i] <- "StoL"
#   df_kot$SubSkill[df_kot$Skill == "dynamics" & df_kot$Interval == i] <- "PtoF"
# }

# Aggregate data
# Overall average
kot <- aggregate(KOT~Condition*Skill, data = subset(df_kot, df_kot$Interval != 32 & df_kot$Interval != 33 & df_kot$Interval != 65 & df_kot$Interval != 66),
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each sub-skill
kot_sub <- aggregate(KOT~Condition*Skill*SubSkill, data = subset(df_kot, df_kot$Note != 33 & df_kot$Note != 66 & df_kot$Note != 67),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
kot_seq <- aggregate(KOT~Interval*Condition*Skill, data = df_kot, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
kot_seq[,4][kot_seq$Interval == 32 | kot_seq$Interval == 33 | kot_seq$Interval == 65 | kot_seq$Interval == 66] <- NA

# Descriptive stats
kot <- cbind(kot, as.data.frame(kot[,3]))
kot_sub <- cbind(kot_sub, as.data.frame(kot_sub[,4]))
kot_seq <- cbind(kot_seq, as.data.frame(kot_seq[,4]))

# Add order info
kot_sub$Order[kot_sub$Skill == "articulation"] <- 1
kot_sub$Order[kot_sub$Skill == "dynamics"] <- 2

# # Add a grouping name
# ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
# for (cond in 1:length(ls_grouping$Condition)){
#   for (skill in 1:length(ls_grouping$Skill)){
#     kot_seq$Grouping[kot_seq$Condition == ls_grouping$Condition[cond] & kot_seq$Skill == ls_grouping$Skill[skill]] <-
#       paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
#   }
# }

####################################
# KOT plots
####################################
p_kot <- ggplot(data = kot, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(-70, 25)) + 
  theme_classic()

p_kot_sub <- ggplot(data = kot_sub, aes(x = reorder(SubSkill, Order), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(0, 40)) + 
  theme_classic()

p_kot_seq_f <- ggplot(data = kot_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean KOT (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()

# plot all graphs
# p_kot_seq <- ggplot(data = kot_seq, aes(x = Interval, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
#                 position = position_dodge(.05)) + 
#   labs(x = 'Interval', y = "Mean KOT (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
#   theme_classic()

# Save plots 
# png files
ggsave('./plot/kot/p_kot.png', plot = p_kot, dpi = 600, width = 5, height = 4)
ggsave('./plot/kot/p_kot_sub.png', plot = p_kot_sub, dpi = 600, width = 5, height = 4)
ggsave('./plot/kot/p_kot_seq_f.png', plot = p_kot_seq_f, dpi = 600, width = 15, height = 4)

# ggsave('./plot/kot/p_kot_seq.png', plot = p_kot_seq, dpi = 600, width = 15, height = 4)