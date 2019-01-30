#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 22/01/2019
# Modified: 22/01/2019
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
df <- read.csv('./csv/ioi_seq_tri.csv', header = T, sep = ",", dec = '.')

####################################
### Create a function
####################################
# Create a dataframe for scatter plots
for (i in unique(df$Condition)){
  for (j in unique(df$Skill)){
    for (k in unique(df$TrialNr)){
      if (k == 1){
        trial_1 <- df$IOI.mean[df$Condition == i & df$Skill == j & df$TrialNr == k]
      }
      if (k != 1){
        trial_current <- df$IOI.mean[df$Condition == i & df$Skill == j & df$TrialNr == k]
        df_current <- data.frame(trial_1, trial_current)
        ylabel <- paste('trial_', k, sep = '')
        filename <- paste('./plot/png/scatter_trial_', k, '.png', sep = '')
        scatter_plot <- ggplot(df_current, aes(x = trial_1, y = trial_current)) +
          geom_point() +
          labs(y = ylabel) +
          theme_classic()
        ggsave(filename, plot = scatter_plot, dpi = 600, width = 5, height = 5)
      }
    }
  }
}

####################################
### Inter-Onset intervals - tempoChange
####################################

####################################
### Key Overlap Time - articulation
####################################

####################################
### Velocity - dynamics
####################################