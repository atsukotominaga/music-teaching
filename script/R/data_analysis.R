#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment
#ctrl+shift+C - comment out a block of code

# Install and load required packages
if (!require("plyr")) {install.packages("plyr"); require("plyr")}
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}

####################################
# Reading and formatting data
####################################
df <- read.csv('data_analysis.csv', header = T, sep = ",", dec = '.')
df$SubNr <- as.factor(df$SubNr)
df_onset <- df %>% dplyr::filter(Key_OnOff == 1)

####################################
# Interonset intervals
####################################
# Calculate IOIs
df_ioi <- df_onset
df_ioi$IOI <- diff(c(0, df_onset$TimeStamp))

# Detect the first note and remove them
df_ioi$FirstNote <- 0
df_ioi$FirstNote[df_ioi$IOI > 5000 | df_ioi$IOI < 0] <- 1
df_ioi <- df_ioi %>% dplyr::filter(FirstNote != 1)

# Assign a sequence number for each tone
df_ioi$Seq <- rep(2:51, length(df_ioi$NoteNr)/50)

# Remove three tones
df_analysis1 <- df_ioi %>% dplyr::filter(Seq != 26 & Seq != 27)

# Descriptive stats
ioi <- aggregate(IOI~Condition*Skill, data = df_analysis1, FUN = function(x){c(N = length(x), mean = mean(x), 
                                                          sd = sd(x), sem = sd(x)/sqrt(length(x)))})

ioi_seq <- aggregate(IOI~Seq*Condition*Skill, data = df_analysis1, FUN = function(x){c(N = length(x), mean = mean(x), 
                                                                               sd = sd(x), sem = sd(x)/sqrt(length(x)))})
####################################
# IOIs plots
####################################

####################################
# Velocity
####################################
# Create a dataframe for a velocity profile
df_velocity <- df_onset

# Assign a sequence number for each tone
df_velocity$Seq <- rep(1:51, length(df_velocity$NoteNr)/51)

# Remove three tones
df_analysis2 <- df_velocity %>% dplyr::filter(Seq != 26 & Seq != 27 & Seq != 51)

# Descriptive stats
velocity <- aggregate(Velocity~Condition*Skill, data = df_analysis2, FUN = function(x){c(N = length(x), mean = mean(x), 
                                                                              sd = sd(x), sem = sd(x)/sqrt(length(x)))})

velocity_seq <- aggregate(Velocity~Seq*Condition*Skill, data = df_analysis2, FUN = function(x){c(N = length(x), mean = mean(x), 
                                                                                         sd = sd(x), sem = sd(x)/sqrt(length(x)))})

####################################
# IOIs plots
####################################

####################################
# Articulation values
####################################
