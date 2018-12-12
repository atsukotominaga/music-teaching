#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment
#ctrl+shift+C - comment out a block of code

# Install and load required packages
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}

####################################
# Reading and formatting data
####################################
df <- read.csv('data_analysis.csv', header = T, sep = ",", dec = '.')
df$SubNr <- as.factor(df$SubNr)

####################################
# Interonset intervals
####################################
# Calculate IOIs
df_onset <- df %>% dplyr::filter(Key_OnOff == 1)
df_onset$IOI <- diff(c(0, df_onset$TimeStamp))

# Detect the first note and remove them
df_onset$FirstNote <- 0
df_onset$FirstNote[df_onset$IOI > 5000 | df_onset$IOI < 0] <- 1
df_onset <- df_onset %>% dplyr::filter(FirstNote != 1)

# Assign a sequence number for each tone
df_onset$Seq <- rep(2:51, length(df_onset$NoteNr)/50)

# Remove three tones
df_onset <- df_onset %>% dplyr::filter(Seq != 26 & Seq != 27 & Seq != 50)

IOI_all <- aggregate(IOI~Skill*Condition, data = df_onset, mean)
IOI_ind <- aggregate(IOI~Skill*Condition*SubNr, data = df_onset, mean)

####################################
# Articulation values
####################################

####################################
# Plots
####################################
IOIplot1 <- ggplot(data=IOI_all, aes(x=Condition, y=IOI, fill=Skill)) + geom_bar(position=position_dodge(), stat="identity")
IOIplot2 <- ggplot(data=IOI_ind, aes(x=Condition, y=IOI, fill=SubNr)) + geom_bar(position=position_dodge(), stat="identity")