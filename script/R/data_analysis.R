#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 5/12/2018
# Modified: 27/12/2018
# This script aggregate and plot data.
# GitHub repo (private): https://github.com/atsukotominaga/expertpiano/tree/master/script/R 

# Install and load required packages
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}

####################################
### Reading and formatting data
####################################
df <- read.csv('./csv/data_analysis.csv', header = T, sep = ",", dec = '.')

####################################
### Inter-Onset intervals - tempoChange
####################################
# Calculate IOIs
df_ioi <- df %>% dplyr::filter(Key_OnOff == 1)
df_ioi$IOI <- diff(c(0, df_ioi$TimeStamp))

# Remove the first note for pilot data
df_ioi <- df_ioi %>% dplyr::filter(IOI > 0 & IOI < 2000)

# Remove the first note
# df_ioi <- df_ioi %>% dplyr::filter(IntervalNr != 17)

# Assign a sequence number for each tone
df_ioi$Interval <- rep(1:50, length(df_ioi$NoteNr)/50)

# Change labels for pilot data
levels(df_ioi$Condition) <- c('performing', 'teaching')
levels(df_ioi$Skill)[levels(df_ioi$Skill) == 'tempoChange'] <- 'dynamics'

# Aggregate data
# Overall average
ioi <- aggregate(IOI~Condition*Skill, data = subset(df_ioi, df_ioi$Interval != 24 & df_ioi$Interval != 25 & df_ioi$Interval != 26 & df_ioi$Interval != 50),
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
ioi_seq <- aggregate(IOI~Interval*Condition*Skill, data = df_ioi, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
ioi_seq[,4][ioi_seq$Interval == 24 | ioi_seq$Interval == 25 | ioi_seq$Interval == 26 | ioi_seq$Interval == 50] <- NA

# Descriptive stats
ioi <- cbind(ioi, as.data.frame(ioi[,3]))
ioi_seq <- cbind(ioi_seq, as.data.frame(ioi_seq[,4]))

# Add a grouping name for pilot data
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (i in 1:length(ls_grouping$Condition)){
  for (j in 1:length(ls_grouping$Skill)){
  ioi$Grouping[ioi$Condition == ls_grouping$Condition[i] & ioi$Skill == ls_grouping$Skill[j]] <-
    paste(ls_grouping$Condition[i], '-', ls_grouping$Skill[j], sep = '')
  ioi_seq$Grouping[ioi_seq$Condition == ls_grouping$Condition[i] & ioi_seq$Skill == ls_grouping$Skill[j]] <- 
    paste(ls_grouping$Condition[i], '-', ls_grouping$Skill[j], sep = '')
  }
}

# # Add a grouping name
# ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'tempoChange', dynamics))
# for (i in 1:length(ls_grouping$Condition)){
#   for (j in 1:length(ls_grouping$Skill)){
#     ioi$Grouping[ioi$Condition == ls_grouping$Condition[i] & ioi$Skill == ls_grouping$Skill[j]] <-
#       paste(ls_grouping$Condition[i], '-', ls_grouping$Skill[j], sep = '')
#     ioi_seq$Grouping[ioi_seq$Condition == ls_grouping$Condition[i] & ioi_seq$Skill == ls_grouping$Skill[j]] <-
#       paste(ls_grouping$Condition[i], '-', ls_grouping$Skill[j], sep = '')
#   }
# }

####################################
### IOIs plots
####################################
plot_ioi <- ggplot(data = ioi, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean IOI (ms)") + coord_cartesian(ylim = c(100, 210)) + 
  theme_classic()

plot_ioi_seq <- ggplot(data = ioi_seq, aes(x = Interval, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 188, linetype = 'dashed') + # Tempo
  geom_hline(data = ioi, aes(yintercept = mean, colour = Grouping), linetype = 'dashed') + # Mean IOI for each grouping
  annotate('text', 0, 188, label = 'Tempo (80bpm)', vjust = -1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean IOI (ms)") + scale_x_continuous(breaks=seq(1,50,1)) +
  theme_classic()

plot_ioi_seq_f <- ggplot(data = ioi_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 188, linetype = 'dashed') + # Tempo
  facet_grid(Skill ~ .) +
  annotate('text', 0, 188, label = 'Tempo (80bpm)', vjust = -1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean IOI (ms)") + scale_x_continuous(breaks=seq(1,50,1)) +
  theme_classic()

plot_ioi_seq_a <- ggplot(data = subset(ioi_seq, ioi_seq$Skill == 'articulation'), aes(x = Interval, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 188, linetype = 'dashed') + # Tempo
  geom_hline(data = subset(ioi, ioi$Skill == 'articulation'), aes(yintercept = mean, colour = Grouping), linetype = 'dashed') + # Mean IOI for each grouping
  annotate('text', 0, 188, label = 'Tempo (80bpm)', vjust = -1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean IOI (ms)") + scale_x_continuous(breaks=seq(1,50,1)) +
  theme_classic()

plot_ioi_seq_d <- ggplot(data = subset(ioi_seq, ioi_seq$Skill == 'dynamics'), aes(x = Interval, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 188, linetype = 'dashed') + # Tempo
  geom_hline(data = subset(ioi, ioi$Skill == 'dynamics'), aes(yintercept = mean, colour = Grouping), linetype = 'dashed') + # Mean IOI for each grouping
  annotate('text', 0, 188, label = 'Tempo (80bpm)', vjust = -1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean IOI (ms)") + scale_x_continuous(breaks=seq(1,50,1)) +
  theme_classic()

# Save plots
# eps files
ggsave('./plot/eps/plot_ioi.eps', plot = plot_ioi, dpi = 600, width = 5, height = 4)
ggsave('./plot/eps/plot_ioi_seq.eps', plot = plot_ioi_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_ioi_seq_f.eps', plot = plot_ioi_seq_f, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_ioi_seq_a.eps', plot = plot_ioi_seq_a, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_ioi_seq_d.eps', plot = plot_ioi_seq_d, dpi = 600, width = 15, height = 4)

# png files
ggsave('./plot/png/plot_ioi.png', plot = plot_ioi, dpi = 600, width = 5, height = 4)
ggsave('./plot/png/plot_ioi_seq.png', plot = plot_ioi_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_ioi_seq_f.png', plot = plot_ioi_seq_f, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_ioi_seq_a.png', plot = plot_ioi_seq_a, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_ioi_seq_d.png', plot = plot_ioi_seq_d, dpi = 600, width = 15, height = 4)

####################################
### Key Overlap Time - articulation
####################################
df_onset <- df %>% dplyr::filter(Key_OnOff == 1)
df_offset <- df %>% dplyr::filter(Key_OnOff == 0)

# Offset 1 - Onset 2
df_onset$KOT <- NA
for (i in 1:length(df_onset$NoteNr)){
  if (i < length(df_onset$NoteNr)){
    df_onset$KOT[i+1] <- df_offset$TimeStamp[i] - df_onset$TimeStamp[i+1]
  }
}

# Remove the first note for pilot data
df_kot <- df_onset %>% dplyr::filter(KOT < 10000)

# Remove the first note
# df_kot <- df_onset %>% dplyr::filter(NoteNr != 17)

# Change labels for pilot data
levels(df_kot$Condition) <- c('performing', 'teaching')
levels(df_kot$Skill)[levels(df_kot$Skill) == 'tempoChange'] <- 'dynamics'

# Assign a sequence number for each tone
df_kot$Interval <- rep(1:50, length(df_kot$NoteNr)/50)

# Assign numbers for legato and staccato
ls_legato <- list(c(1:7), c(17:19), c(27:33), c(43:45))
ls_staccato <- list(c(9:15), c(21:23), c(35:41), c(47:49))

# Legato
df_kot$Articulation <- NA
for (i in 1:length(ls_legato)){
  for (j in 1:length(ls_legato[[i]])){
    df_kot$Articulation[df_kot$Interval == ls_legato[[i]][j]] <- 'Legato'
  }
}

# Staccato
for (i in 1:length(ls_staccato)){
  for (j in 1:length(ls_staccato[[i]])){
    df_kot$Articulation[df_kot$Interval == ls_staccato[[i]][j]] <- 'Staccato'
  }
}

# Aggregate data
# Overall average
kot <- aggregate(KOT~Condition*Skill, data = subset(df_kot, df_kot$Interval != 24 & df_kot$Interval != 25 & df_kot$Interval != 26 & df_kot$Interval != 50),
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Change labels for pilot data
levels(kot$Condition) <- c('performing', 'teaching')
levels(kot$Skill)[levels(kot$Skill) == 'tempoChange'] <- 'dynamics'

# Average for legato
kot_leg <- aggregate(KOT~Condition*Skill, data = subset(df_kot, df_kot$Skill == 'articulation' & df_kot$Articulation == 'Legato'),
                    FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for staccato
kot_sta <- aggregate(KOT~Condition*Skill, data = subset(df_kot, df_kot$Skill == 'articulation' & df_kot$Articulation == 'Staccato'),
                      FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
kot_seq <- aggregate(KOT~Interval*Condition*Skill, data = df_kot, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
kot_seq[,4][kot_seq$Interval == 24 | kot_seq$Interval == 25 | kot_seq$Interval == 26 | kot_seq$Interval == 50] <- NA

# Descriptive stats
kot <- cbind(kot, as.data.frame(kot[,3]))
kot_leg <- cbind(kot_leg, as.data.frame(kot_leg[,3]))
kot_sta <- cbind(kot_sta, as.data.frame(kot_sta[,3]))
kot_seq <- cbind(kot_seq, as.data.frame(kot_seq[,4]))

# Add a grouping name for pilot data
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (i in 1:length(ls_grouping$Condition)){
  for (j in 1:length(ls_grouping$Skill)){
    kot_seq$Grouping[kot_seq$Condition == ls_grouping$Condition[i] & kot_seq$Skill == ls_grouping$Skill[j]] <- 
      paste(ls_grouping$Condition[i], '-', ls_grouping$Skill[j], sep = '')
  }
}

# # Add a grouping name
# ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'tempoChange', dynamics))
# for (i in 1:length(ls_grouping$Condition)){
#   for (j in 1:length(ls_grouping$Skill)){
#     kot_seq$Grouping[kot_seq$Condition == ls_grouping$Condition[i] & kot_seq$Skill == ls_grouping$Skill[j]] <- 
#       paste(ls_grouping$Condition[i], '-', ls_grouping$Skill[j], sep = '')
#   }
# }

####################################
# KOT plots
####################################
plot_kot <- ggplot(data = kot, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(-70, 25)) + 
  theme_classic()

plot_kot_leg <- ggplot(data = kot_leg, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean KOT (ms)") + coord_cartesian(ylim = c(0, 35)) + 
  theme_classic()

plot_kot_sta <- ggplot(data = kot_sta, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean KOT (ms)") + coord_cartesian(ylim = c(-150, 30)) + 
  theme_classic()

plot_kot_seq <- ggplot(data = kot_seq, aes(x = Interval, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean KOT (ms)") + scale_x_continuous(breaks=seq(1,50,1)) +
  theme_classic()

plot_kot_seq_f <- ggplot(data = kot_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean KOT (ms)") + scale_x_continuous(breaks=seq(1,50,1)) +
  theme_classic()

plot_kot_seq_a <- ggplot(data = subset(kot_seq, kot_seq$Skill == 'articulation'), aes(x = Interval, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean KOT (ms)") + scale_x_continuous(breaks=seq(1,50,1)) +
  theme_classic()

plot_kot_seq_d <- ggplot(data = subset(kot_seq, kot_seq$Skill == 'dynamics'), aes(x = Interval, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean KOT (ms)") + scale_x_continuous(breaks=seq(1,50,1)) +
  theme_classic()

# Save plots 
# eps files
ggsave('./plot/eps/plot_kot.eps', plot = plot_kot, dpi = 600, width = 5, height = 4)
ggsave('./plot/eps/plot_kot_leg.eps', plot = plot_kot_leg, dpi = 600, width = 5, height = 4)
ggsave('./plot/eps/plot_kot_sta.eps', plot = plot_kot_sta, dpi = 600, width = 5, height = 4)
ggsave('./plot/eps/plot_kot_seq.eps', plot = plot_kot_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_kot_seq_f.eps', plot = plot_kot_seq_f, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_kot_seq_a.eps', plot = plot_kot_seq_a, dpi = 600, width = 15, height = 4) 
ggsave('./plot/eps/plot_kot_seq_d.eps', plot = plot_kot_seq_d, dpi = 600, width = 15, height = 4) 

# png files
ggsave('./plot/png/plot_kot.png', plot = plot_kot, dpi = 600, width = 5, height = 4)
ggsave('./plot/png/plot_kot_leg.png', plot = plot_kot_leg, dpi = 600, width = 5, height = 4)
ggsave('./plot/png/plot_kot_sta.png', plot = plot_kot_sta, dpi = 600, width = 5, height = 4)
ggsave('./plot/png/plot_kot_seq.png', plot = plot_kot_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_kot_seq_f.png', plot = plot_kot_seq_f, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_kot_seq_a.png', plot = plot_kot_seq_a, dpi = 600, width = 15, height = 4) 
ggsave('./plot/png/plot_kot_seq_d.png', plot = plot_kot_seq_d, dpi = 600, width = 15, height = 4) 

####################################
### Velocity - dynamics
####################################
# Calculate Acc (acceleration - velocity difference between notes)
df_vel <- df %>% dplyr::filter(Key_OnOff == 1)
df_vel$Acc <- diff(c(0, df_vel$Velocity))

# Remove the first note for pilot data
df_vel_acc <- data.frame()
for (i in unique(df_vel$SubNr)){
  for (j in unique(df_vel$BlockNr[df_vel$SubNr == i])){
    for (k in unique(df_vel$TrialNr[df_vel$SubNr == i & df_vel$BlockNr == j])){
      df_current <- df_vel %>% dplyr::filter(SubNr == i & BlockNr == j & TrialNr == k)
      df_vel_acc <- rbind(df_vel_acc, subset(df_current, df_current$NoteNr != min(df_current$NoteNr)))
    }
  }
}

# Remove the first note
# df_vel_acc <- df_vel %>% dplyr::filter(NoteNr != 17)

# Assign a sequence number for each tone
df_vel$Note <- rep(1:51, length(df_vel$NoteNr)/51) # for vel_seq
df_vel_acc$Interval <- rep(1:50, length(df_vel_acc$NoteNr)/50) # for vel_acc_seq

# Change labels for pilot data
levels(df_vel$Condition) <- c('performing', 'teaching')
levels(df_vel$Skill)[levels(df_vel$Skill) == 'tempoChange'] <- 'dynamics'
levels(df_vel_acc$Condition) <- c('performing', 'teaching')
levels(df_vel_acc$Skill)[levels(df_vel_acc$Skill) == 'tempoChange'] <- 'dynamics'

# Average data
# Overall average
vel <- aggregate(Velocity~Condition*Skill, data = subset(df_vel, df_ioi$Note != 24 & df_ioi$Note != 25 & df_ioi$Note != 26 & df_ioi$Note != 50), 
                      FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
# Average for each note
vel_seq <- aggregate(Velocity~Note*Condition*Skill, data = df_vel, 
                          FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_seq[,4][vel_seq$Note == 25 | vel_seq$Note == 26 | vel_seq$Note == 51] <- NA
vel_acc_seq <- aggregate(Acc~Interval*Condition*Skill, data = df_vel_acc, 
                              FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_acc_seq[,4][vel_acc_seq$Interval == 24 | vel_acc_seq$Interval == 25 | vel_acc_seq$Interval == 26 | vel_acc_seq$Interval == 50] <- NA

# Aggregated data
vel <- cbind(vel, as.data.frame(vel[,3]))
vel_seq <- cbind(vel_seq, as.data.frame(vel_seq[,4]))
vel_acc_seq <- cbind(vel_acc_seq, as.data.frame(vel_acc_seq[,4]))

# Add a grouping name
for (i in 1:length(ls_grouping$Condition)){
  for (j in 1:length(ls_grouping$Skill)){
    vel$Grouping[vel$Condition == ls_grouping$Condition[i] & vel$Skill == ls_grouping$Skill[j]] <- 
      paste(ls_grouping$Condition[i], '-', ls_grouping$Skill[j], sep = '')
    vel_seq$Grouping[vel_seq$Condition == ls_grouping$Condition[i] & vel_seq$Skill == ls_grouping$Skill[j]] <- 
      paste(ls_grouping$Condition[i], '-', ls_grouping$Skill[j], sep = '')
    vel_acc_seq$Grouping[vel_acc_seq$Condition == ls_grouping$Condition[i] & vel_acc_seq$Skill == ls_grouping$Skill[j]] <- 
      paste(ls_grouping$Condition[i], '-', ls_grouping$Skill[j], sep = '')
  }
}

####################################
### Velocity plots
####################################
plot_vel <- ggplot(data = vel, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = .2, position=position_dodge(.9)) + 
  labs(y = "Velocity (0-127)") + coord_cartesian(ylim = c(40, 70)) + theme_classic()

plot_vel_seq <- ggplot(data = vel_seq, aes(x = Note, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_hline(data = vel, aes(yintercept = mean, colour = Grouping), linetype = 'dashed') + # Mean Velocity for each grouping
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(0.05)) + 
  labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,51,1)) +
  theme_classic()

plot_vel_seq_f <- ggplot(data = vel_seq, aes(x = Note, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(0.05)) + 
  labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,51,1)) +
  theme_classic()

plot_vel_seq_a <- ggplot(data = subset(vel_seq, vel_seq$Skill == 'articulation'), aes(x = Note, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_hline(data = subset(vel, vel$Skill == 'articulation'), aes(yintercept = mean, colour = Grouping), linetype = 'dashed') + # Mean Velocity for each grouping
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position=position_dodge(0.05)) + 
  labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,51,1)) +
  theme_classic()

plot_vel_seq_d <- ggplot(data = subset(vel_seq, vel_seq$Skill == 'dynamics'), aes(x = Note, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_hline(data = subset(vel, vel$Skill == 'dynamics'), aes(yintercept = mean, colour = Grouping), linetype = 'dashed') + # Mean Velocity for each grouping
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position=position_dodge(0.05)) + 
  labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,51,1)) +
  theme_classic()

plot_vel_acc_seq <- ggplot(data = vel_acc_seq, aes(x = Interval, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position=position_dodge(.05)) + 
  labs(x = 'Interval', y = "Acceleration") + scale_x_continuous(breaks=seq(1,50,1)) +
  theme_classic()

plot_vel_acc_seq_f <- ggplot(data = vel_acc_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Acceleration") + scale_x_continuous(breaks=seq(1,50,1)) +
  theme_classic()

# Save plots
# eps files
ggsave('./plot/eps/plot_vel.eps', plot = plot_vel, dpi = 600, width = 5, height = 4)
ggsave('./plot/eps/plot_vel_seq.eps', plot = plot_vel_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_vel_seq_f.eps', plot = plot_vel_seq_f, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_vel_seq_a.eps', plot = plot_vel_seq_a, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_vel_seq_d.eps', plot = plot_vel_seq_d, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_vel_acc_seq.eps', plot = plot_vel_acc_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_vel_acc_seq_f.eps', plot = plot_vel_acc_seq_f, dpi = 600, width = 15, height = 4) 

# png files
ggsave('./plot/png/plot_vel.png', plot = plot_vel, dpi = 600, width = 5, height = 4)
ggsave('./plot/png/plot_vel_seq.png', plot = plot_vel_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_vel_seq_f.png', plot = plot_vel_seq_f, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_vel_seq_a.png', plot = plot_vel_seq_a, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_vel_seq_d.png', plot = plot_vel_seq_d, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_vel_acc_seq.png', plot = plot_vel_acc_seq, dpi = 600, width = 15, height = 4) 
ggsave('./plot/png/plot_vel_acc_seq_f.png', plot = plot_vel_acc_seq_f, dpi = 600, width = 15, height = 4) 

####################################
### Velocity Max-Min - dynamics
####################################
# Max - Min
# Define apriori max and min
ls_apriori <- list(c(1, 5), c(9, 13), c(17, 21), c(27, 31), c(35, 39), c(43, 47))

# Assign apriori max and min
vel_seq$ApriMaxMin <- NA
for (i in 1:length(ls_apriori)){
  # Assign min
  vel_seq$ApriMaxMin[vel_seq$Note == ls_apriori[[i]][1]] <- 'Min'
  # Assign max
  vel_seq$ApriMaxMin[vel_seq$Note == ls_apriori[[i]][2]] <- 'Max'
}

# Assign actual max and min
ls_range <- list(c(1, 8), c(9, 16), c(17, 24), c(27, 34), c(35, 42), c(43, 50))

# Find actual min and max
vel_seq$DataMaxMin <- NA
for (group in unique(vel_seq$Grouping)){
  for (range in 1:length(ls_range)){
    min <- min(vel_seq$mean[vel_seq$Grouping == group & vel_seq$Note >= ls_range[[range]][1] & vel_seq$Note <= ls_range[[range]][2]])
    max <- max(vel_seq$mean[vel_seq$Grouping == group & vel_seq$Note >= ls_range[[range]][1] & vel_seq$Note <= ls_range[[range]][2]])
    vel_seq$DataMaxMin[vel_seq$Grouping == group & vel_seq$Note >= ls_range[[range]][1] 
                       & vel_seq$Note <= ls_range[[range]][2] & vel_seq$mean == min] <- 'Min'
    vel_seq$DataMaxMin[vel_seq$Grouping == group & vel_seq$Note >= ls_range[[range]][1] 
                       & vel_seq$Note <= ls_range[[range]][2] & vel_seq$mean == max] <- 'Max'
  }
}

maxmin_apri <- subset(vel_seq, vel_seq$ApriMaxMin == 'Min' | vel_seq$ApriMaxMin == 'Max')
maxmin_data <- subset(vel_seq, vel_seq$DataMaxMin == 'Min' | vel_seq$DataMaxMin == 'Max')

####################################
### Velocity Max-Min plots
####################################
plot_maxmin_data <- ggplot(data = subset(maxmin_data, maxmin_data$Skill == 'dynamics'), 
                        aes(x = Note, y = mean, group = DataMaxMin, shape = DataMaxMin, colour = Condition)) +
  geom_point(size = 3) +
  geom_vline(xintercept = c(unlist(lapply(ls_apriori, `[[`, 1))), linetype = 'dotted', colour = 'grey') + # Apriori Max
  geom_vline(xintercept = c(unlist(lapply(ls_apriori, `[[`, 2))), linetype = 'longdash', colour = 'grey') + # Apriori Max
  facet_grid(Skill ~ .) +
  labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,51,1)) +
  theme_classic()
plot_maxmin_data

ggsave('./plot/eps/plot_maxmin_data.eps', plot = plot_maxmin_data, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_maxmin_data.png', plot = plot_maxmin_data, dpi = 600, width = 15, height = 4)