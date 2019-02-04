#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 30/01/2019
# This script aggregate and plot data.
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
# eps
if (!file.exists("plot/eps/")){
  dir.create("plot/eps")
}
# png
if (!file.exists("plot/png/")){
  dir.create("plot/png")
}

####################################
### Reading and formatting data
####################################
df_all <- read.csv('./csv/data_analysis.csv', header = T, sep = ",", dec = '.')
df_exc <- read.csv('./csv/data_errorRate.csv', header = T, sep = ",", dec = '.')

# Exclude participants
include <- df_exc$SubNr[df_exc$Exclude == 'include']

df_analysis <- data.frame()
for (subnr in include){
  df_current <- df_all %>% dplyr::filter(SubNr == subnr)
  df_analysis <- rbind(df_analysis, df_current)
}

####################################
### Inter-Onset intervals
####################################
# Calculate IOIs
df_ioi <- df_analysis %>% dplyr::filter(Key_OnOff == 1)
df_ioi$IOI <- diff(c(0, df_ioi$TimeStamp))

# Remove the first note
df_ioi <- df_ioi %>% dplyr::filter(NoteNr != 17)

# Assign a sequence number for each tone
df_ioi$Interval <- rep(1:66, length(df_ioi$NoteNr)/66)

# Aggregate data
# Overall average
ioi <- aggregate(IOI~Condition*Skill, data = subset(df_ioi, df_ioi$Interval != 32 & df_ioi$Interval != 33 & df_ioi$Interval != 65 & df_ioi$Interval != 66),
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
ioi_seq <- aggregate(IOI~Interval*Condition*Skill, data = df_ioi, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
ioi_seq[,4][ioi_seq$Interval == 32 | ioi_seq$Interval == 33 | ioi_seq$Interval == 65 | ioi_seq$Interval == 66] <- NA

# Descriptive stats
ioi <- cbind(ioi, as.data.frame(ioi[,3]))
ioi_seq <- cbind(ioi_seq, as.data.frame(ioi_seq[,4]))

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    ioi$Grouping[ioi$Condition == ls_grouping$Condition[cond] & ioi$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_seq$Grouping[ioi_seq$Condition == ls_grouping$Condition[cond] & ioi_seq$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}


####################################
### IOIs plots
####################################
plot_ioi <- ggplot(data = ioi, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean IOI (ms)") + coord_cartesian(ylim = c(0, 200)) + 
  theme_classic()

plot_ioi_seq <- ggplot(data = ioi_seq, aes(x = Interval, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 188, linetype = 'dashed') + # Tempo
  geom_hline(data = ioi, aes(yintercept = mean, colour = Grouping), linetype = 'dashed') + # Mean IOI for each grouping
  annotate('text', 0, 188, label = 'Tempo (80bpm)', vjust = -1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean IOI (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()

plot_ioi_seq_f <- ggplot(data = ioi_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 188, linetype = 'dashed') + # Tempo
  facet_grid(Skill ~ .) +
  annotate('text', 0, 188, label = 'Tempo (80bpm)', vjust = -1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean IOI (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()

# Save plots
# eps files
ggsave('./plot/eps/plot_ioi.eps', plot = plot_ioi, dpi = 600, width = 5, height = 4)
ggsave('./plot/eps/plot_ioi_seq.eps', plot = plot_ioi_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_ioi_seq_f.eps', plot = plot_ioi_seq_f, dpi = 600, width = 15, height = 4)

# png files
ggsave('./plot/png/plot_ioi.png', plot = plot_ioi, dpi = 600, width = 5, height = 4)
ggsave('./plot/png/plot_ioi_seq.png', plot = plot_ioi_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_ioi_seq_f.png', plot = plot_ioi_seq_f, dpi = 600, width = 15, height = 4)


####################################
### Key Overlap Time - articulation
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

# Assign numbers for legato and staccato
ls_legato <- list(c(1:7), c(17:23), c(42:48), c(58:64))
ls_staccato <- list(c(9:15), c(25:31), c(34:40), c(50:56))

# Legato
df_kot$Articulation <- NA
for (phrase in 1:length(ls_legato)){
  for (note in 1:length(ls_legato[[phrase]])){
    df_kot$Articulation[df_kot$Skill == 'articulation' & df_kot$Interval == ls_legato[[phrase]][note]] <- 'Legato'
  }
}

# Staccato
for (phrase in 1:length(ls_staccato)){
  for (note in 1:length(ls_staccato[[phrase]])){
    df_kot$Articulation[df_kot$Skill == 'articulation' & df_kot$Interval == ls_staccato[[phrase]][note]] <- 'Staccato'
  }
}

# Aggregate data
# Overall average
kot <- aggregate(KOT~Condition*Skill, data = subset(df_kot, df_kot$Interval != 32 & df_kot$Interval != 33 & df_kot$Interval != 65 & df_kot$Interval != 66),
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for legato and staccato
kot_art <- aggregate(KOT~Condition*Skill*Articulation, data = subset(df_kot),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
kot_seq <- aggregate(KOT~Interval*Condition*Skill, data = df_kot, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
kot_seq[,4][kot_seq$Interval == 32 | kot_seq$Interval == 33 | kot_seq$Interval == 65 | kot_seq$Interval == 66] <- NA

# Descriptive stats
kot <- cbind(kot, as.data.frame(kot[,3]))
kot_art <- cbind(kot_art, as.data.frame(kot_art[,4]))
kot_seq <- cbind(kot_seq, as.data.frame(kot_seq[,4]))

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    kot_seq$Grouping[kot_seq$Condition == ls_grouping$Condition[cond] & kot_seq$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

####################################
# KOT plots
####################################
plot_kot <- ggplot(data = kot, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean KOT (ms)") + coord_cartesian(ylim = c(-70, 25)) + 
  theme_classic()

plot_kot_art <- ggplot(data = kot_art, aes(x = Articulation, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(0, 40)) + 
  theme_classic()

plot_kot_seq <- ggplot(data = kot_seq, aes(x = Interval, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean KOT (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()

plot_kot_seq_f <- ggplot(data = kot_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean KOT (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()

# Save plots 
# eps files
ggsave('./plot/eps/plot_kot.eps', plot = plot_kot, dpi = 600, width = 5, height = 4)
ggsave('./plot/eps/plot_kot_art.eps', plot = plot_kot_art, dpi = 600, width = 5, height = 4)
ggsave('./plot/eps/plot_kot_seq.eps', plot = plot_kot_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_kot_seq_f.eps', plot = plot_kot_seq_f, dpi = 600, width = 15, height = 4)

# png files
ggsave('./plot/png/plot_kot.png', plot = plot_kot, dpi = 600, width = 5, height = 4)
ggsave('./plot/png/plot_kot_art.png', plot = plot_kot_art, dpi = 600, width = 5, height = 4)
ggsave('./plot/png/plot_kot_seq.png', plot = plot_kot_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_kot_seq_f.png', plot = plot_kot_seq_f, dpi = 600, width = 15, height = 4)


####################################
### Velocity - dynamics
####################################
# Calculate Acc (acceleration - velocity difference between notes)
df_vel <- df_analysis %>% dplyr::filter(Key_OnOff == 1)
df_vel$Acc <- diff(c(0, df_vel$Velocity))

# Remove the first note for pilot data
df_vel_acc <- data.frame()
for (subnr in unique(df_vel$SubNr)){
  for (block in unique(df_vel$BlockNr[df_vel$SubNr == subnr])){
    for (trial in unique(df_vel$TrialNr[df_vel$SubNr == subnr & df_vel$BlockNr == block])){
      df_current <- df_vel %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      df_vel_acc <- rbind(df_vel_acc, subset(df_current, df_current$NoteNr != min(df_current$NoteNr)))
    }
  }
}

# Remove the first note
df_vel_acc <- df_vel %>% dplyr::filter(NoteNr != 17)

# Assign a sequence number for each tone
df_vel$Note <- rep(1:67, length(df_vel$NoteNr)/67) # for vel_seq
df_vel_acc$Interval <- rep(1:66, length(df_vel_acc$NoteNr)/66) # for vel_acc_seq

# Assign numbers for forte and piano
ls_forte <- list(c(1:8), c(17:24), c(42:49), c(58:65))
ls_piano <- list(c(9:16), c(25:32), c(34:41), c(50:57))

# Forte
df_vel$Dynamics <- NA
for (phrase in 1:length(ls_forte)){
  for (note in 1:length(ls_forte[[phrase]])){
    df_vel$Dynamics[df_vel$Skill == 'dynamics' & df_vel$Note == ls_forte[[phrase]][note]] <- 'Forte'
  }
}

# Piano
for (phrase in 1:length(ls_piano)){
  for (note in 1:length(ls_piano[[phrase]])){
    df_vel$Dynamics[df_vel$Skill == 'dynamics' & df_vel$Note == ls_piano[[phrase]][note]] <- 'Piano'
  }
}

# Average data
# Overall average
vel <- aggregate(Velocity~Condition*Skill, data = subset(df_vel, df_ioi$Note != 33 & df_ioi$Note != 66 & df_ioi$Note != 67), 
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for forte and piano
vel_dyn <- aggregate(Velocity~Condition*Skill*Dynamics, data = df_vel,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
vel_seq <- aggregate(Velocity~Note*Condition*Skill, data = df_vel, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_seq[,4][vel_seq$Note == 33 | vel_seq$Note == 66 | vel_seq$Note == 67] <- NA
vel_acc_seq <- aggregate(Acc~Interval*Condition*Skill, data = df_vel_acc, 
                         FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_acc_seq[,4][vel_acc_seq$Interval == 32 | vel_acc_seq$Interval == 33 | vel_acc_seq$Interval == 65 | vel_acc_seq$Interval == 66] <- NA

# Aggregated data
vel <- cbind(vel, as.data.frame(vel[,3]))
vel_dyn <- cbind(vel_dyn, as.data.frame(vel_dyn[,4]))
vel_seq <- cbind(vel_seq, as.data.frame(vel_seq[,4]))
vel_acc_seq <- cbind(vel_acc_seq, as.data.frame(vel_acc_seq[,4]))

# Add a grouping name
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    vel$Grouping[vel$Condition == ls_grouping$Condition[cond] & vel$Skill == ls_grouping$Skill[skill]] <- 
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    vel_seq$Grouping[vel_seq$Condition == ls_grouping$Condition[cond] & vel_seq$Skill == ls_grouping$Skill[skill]] <- 
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    vel_acc_seq$Grouping[vel_acc_seq$Condition == ls_grouping$Condition[cond] & vel_acc_seq$Skill == ls_grouping$Skill[skill]] <- 
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

####################################
### Velocity plots
####################################
plot_vel <- ggplot(data = vel, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = .2, position=position_dodge(.9)) + 
  labs(y = "Velocity (0-127)") + coord_cartesian(ylim = c(0, 95)) + 
  theme_classic()

plot_vel_dyn <- ggplot(data = vel_dyn, aes(x = Dynamics, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Velocity (0-127)") + #coord_cartesian(ylim = c(0, 95)) + 
  theme_classic()

plot_vel_seq <- ggplot(data = vel_seq, aes(x = Note, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_hline(data = vel, aes(yintercept = mean, colour = Grouping), linetype = 'dashed') + # Mean Velocity for each grouping
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(0.05)) + 
  labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,67,1)) +
  theme_classic()

plot_vel_seq_f <- ggplot(data = vel_seq, aes(x = Note, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(0.05)) + 
  labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,67,1)) +
  theme_classic()

plot_vel_acc_seq <- ggplot(data = vel_acc_seq, aes(x = Interval, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position=position_dodge(.05)) + 
  labs(x = 'Interval', y = "Acceleration") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()

plot_vel_acc_seq_f <- ggplot(data = vel_acc_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Acceleration") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()

# Save plots
# eps files
ggsave('./plot/eps/plot_vel.eps', plot = plot_vel, dpi = 600, width = 5, height = 4)
ggsave('./plot/eps/plot_vel_dyn.eps', plot = plot_vel_dyn, dpi = 600, width = 5, height = 4)
ggsave('./plot/eps/plot_vel_seq.eps', plot = plot_vel_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_vel_seq_f.eps', plot = plot_vel_seq_f, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_vel_acc_seq.eps', plot = plot_vel_acc_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_vel_acc_seq_f.eps', plot = plot_vel_acc_seq_f, dpi = 600, width = 15, height = 4)

# png files
ggsave('./plot/png/plot_vel.png', plot = plot_vel, dpi = 600, width = 5, height = 4)
ggsave('./plot/png/plot_vel_dyn.png', plot = plot_vel_dyn, dpi = 600, width = 5, height = 4)
ggsave('./plot/png/plot_vel_seq.png', plot = plot_vel_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_vel_seq_f.png', plot = plot_vel_seq_f, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_vel_acc_seq.png', plot = plot_vel_acc_seq, dpi = 600, width = 15, height = 4) 
ggsave('./plot/png/plot_vel_acc_seq_f.png', plot = plot_vel_acc_seq_f, dpi = 600, width = 15, height = 4) 