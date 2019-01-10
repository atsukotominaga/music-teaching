#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 30/12/2018
# Modified: 09/01/2019
# This script calculates correlation coefficients.
# GitHub repo (private): https://github.com/atsukotominaga/expertpiano/tree/master/script/R 

####################################
#  Requirements
####################################
# !!! Set working directory to file source location !!!

# Install and load required packages
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}

####################################
### Reading and formatting data
####################################
df <- read.csv('./csv/data_analysis.csv', header = T, sep = ",", dec = '.')

####################################
### Create a function
####################################
corr_function <- function(var){
  # Calculation correlation coefficients
  df_corr <- data.frame()
  for (cond in unique(var$Condition)){
    for (skill in unique(var$Skill)){
      # Compare to the first trial
      base <- var$mean[var$Condition == cond & var$Skill == skill & var$TrialNr == 1]
      # Compare to the average performance across trials
      agg <- aggregate(mean~Count, data = subset(var, var$Condition == cond & var$Skill == skill), FUN = function(x){M = mean(x)})
      ave <- agg$mean
      for (trial in unique(var$TrialNr)){
        current <- var$mean[var$Condition == cond & var$Skill == skill & var$TrialNr == trial]
        corr_base <- cor(base, current, method = "pearson")
        corr_ave <- cor(ave, current, method = "pearson")
        ls_current <- list(Condition = cond, Skill = skill, TrialNr = trial, Corr_base = corr_base, Corr_ave = corr_ave)
        df_corr <- rbind(df_corr, as.data.frame(ls_current))
      }
    }
  }
  
  plot_corr_base <- ggplot(data = df_corr, aes(x = TrialNr, y = Corr_base, group = Condition, colour = Condition)) +
    geom_line() +
    geom_point() +
    facet_grid(Skill ~ .) +
    labs(y = "Correlation coefficient") + 
    scale_x_continuous(breaks=seq(1,8,1)) + coord_cartesian(ylim = c(0, 1)) +
    theme_classic()
  
  plot_corr_ave <- ggplot(data = df_corr, aes(x = TrialNr, y = Corr_ave, group = Condition, colour = Condition)) +
    geom_line() +
    geom_point() +
    facet_grid(Skill ~ .) +
    labs(y = "Correlation coefficient") + 
    scale_x_continuous(breaks=seq(1,8,1)) + coord_cartesian(ylim = c(0, 1)) +
    theme_classic()
  
  filename_base = paste('./plot/png/', deparse(substitute(var)), '_base', '.png', sep = '')
  filename_ave = paste('./plot/png/', deparse(substitute(var)), '_ave', '.png', sep = '')
  ggsave(filename_base, plot = plot_corr_base, dpi = 600, width = 10, height = 4)
  ggsave(filename_ave, plot = plot_corr_ave, dpi = 600, width = 10, height = 4)
}

####################################
### Inter-Onset intervals - tempoChange
####################################
# Calculate IOIs
df_ioi <- df %>% dplyr::filter(Key_OnOff == 1)
df_ioi$IOI <- diff(c(0, df_ioi$TimeStamp))

# Remove the first note for pilot data
df_ioi <- df_ioi %>% dplyr::filter(IOI > 0 & IOI < 2000)

# Remove the first note
# df_ioi <- df_ioi %>% dplyr::filter(NoteNr != 17)

# Assign a sequence number for each tone
df_ioi$Count <- rep(1:50, length(df_ioi$NoteNr)/50)

# Change labels for pilot data
levels(df_ioi$Condition) <- c('performing', 'teaching')
levels(df_ioi$Skill)[levels(df_ioi$Skill) == 'tempoChange'] <- 'dynamics'

# Aggregate data
ioi <- aggregate(IOI~Count*TrialNr*Condition*Skill, data = df_ioi,
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Descriptive stats
ioi <- cbind(ioi, as.data.frame(ioi[,5]))

# Calculate correlation coefficients and plot correlations
corr_function(ioi)

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
df_kot$Count <- rep(1:50, length(df_kot$NoteNr)/50)

# Aggregate data
kot <- aggregate(KOT~Count*TrialNr*Condition*Skill, data = df_kot,
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Descriptive stats
kot <- cbind(kot, as.data.frame(kot[,5]))

# Calculate correlation coefficients
corr_function(kot)

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
df_vel$Count <- rep(1:51, length(df_vel$NoteNr)/51) # for vel_seq
df_vel_acc$Count <- rep(1:50, length(df_vel_acc$NoteNr)/50) # for vel_acc_seq

# Change labels for pilot data
levels(df_vel$Condition) <- c('performing', 'teaching')
levels(df_vel$Skill)[levels(df_vel$Skill) == 'tempoChange'] <- 'dynamics'
levels(df_vel_acc$Condition) <- c('performing', 'teaching')
levels(df_vel_acc$Skill)[levels(df_vel_acc$Skill) == 'tempoChange'] <- 'dynamics'

# Aggregate data
vel <- aggregate(Velocity~Count*TrialNr*Condition*Skill, data = df_vel,
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_acc <- aggregate(Acc~Count*TrialNr*Condition*Skill, data = df_vel_acc,
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Descriptive stats
vel <- cbind(vel, as.data.frame(vel[,5]))
vel_acc <- cbind(vel_acc, as.data.frame(vel_acc[,5]))

# Calculate correlation coefficients
corr_function(vel)
corr_function(vel_acc)