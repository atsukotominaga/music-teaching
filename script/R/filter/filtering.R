#!/usr/local/bin/R
#rm(list=ls(all=T)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 24/06/2019
# This script organises raw data and removes pitch errors for the first filtering.
# GitHub repo (private): https://github.com/atsukotominaga/teaching-v2.0/tree/master/script/R 

####################################
#  Requirements
####################################
# set working directory to file source location
# install and load required packages
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("editData")) {install.packages("editData"); require("editData")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("wesanderson")) {install.packages("wesanderson"); require("wesanderson")}

# read functions
source("./function.R")

# setting
# ggplots
theme_set(theme_classic())

# create necessary folders if not exist
# filtered - all of the outputs will be stored in this folder
if (!file.exists("filtered")){
  dir.create("filtered")
}

# read a text file for ideal performance
df_ideal <- read.table("./ideal.txt")
colnames(df_ideal) <- "Pitch"
df_ideal$RowNr <- c(1:nrow(df_ideal))
df_ideal <- df_ideal[c(2, 1)]

# create a list of data file names
lf <- list.files("./raw_data", pattern = "txt")

# create raw_data - merge all data files into one
raw_data <- data.frame()
for (i in 1:length(lf)){
  data_i <- read.csv(file.path("./raw_data", lf[i]), header = F, sep = " ", dec = ".")
  raw_data <- rbind(raw_data, data_i)
}

# add column namesls
colnames(raw_data) <- c("NoteNr", "TimeStamp", "Pitch", "Velocity", "Key_OnOff", "Device", "Tempo",
                        "SubNr", "BlockNr", "TrialNr", "Skill", "Condition", "Image")

# clean raw_data
raw_data$NoteNr <- as.numeric(gsub(",", "", raw_data$NoteNr))
raw_data$Image <- gsub(";", "", raw_data$Image)

# sort by SubNr, BlockNr, TrialNr
raw_data <- raw_data[order(raw_data$SubNr, raw_data$BlockNr, raw_data$TrialNr),]
raw_data$RowNr <- c(1:nrow(raw_data))
raw_data <- raw_data[c(14, 1:13)]

####################################
# Detect pitch errors
####################################
# raw_data without metronome
df_all <- raw_data %>% dplyr::filter(Pitch != 31 & Pitch != 34)

# onset and offset
df_onset <- df_all %>% dplyr::filter(Key_OnOff == 1)
df_offset <- df_all %>% dplyr::filter(Key_OnOff == 0)

# calculate IOIs
df_onset$IOI <- diff(c(0, df_onset$TimeStamp))

# 1. FIRST FILTERING (AUTOMATIC)
### ONSET ###
# detect pitch errors
ls_removed_onset <- pitch_remover(df_onset)

# create a data frame of removed trials
df_removed_onset <- data.frame(t(data.frame(ls_removed_onset))) # transpose
colnames(df_removed_onset) <- c("SubNr", "BlockNr", "TrialNr")
rownames(df_removed_onset) <- c(1:nrow(df_removed_onset))

# mark trails with errors by the first filtering
# df_onset$Error1 <- 0
# #df_onset_ioi$Error1 <- 0
# for (error in 1:length(ls_removed_onset)){
#   df_onset$Error1[df_onset$SubNr == ls_removed_onset[[error]][1] & df_onset$BlockNr == ls_removed_onset[[error]][2] & df_onset$TrialNr == ls_removed_onset[[error]][3]] <- 1
# }

# 2. SECOND FILTERING (MANUAL)
### ONSET ###
# detect errorType and errorRowNr
for (i in 1:nrow(df_removed_onset)){
  current <- df_onset %>% dplyr::filter(SubNr == df_removed_onset[i,1] & BlockNr == df_removed_onset[i,2] & TrialNr == df_removed_onset[i,3])
  current$RowNr <- c(1:nrow(current))
  df_removed_onset <- check(df_removed_onset, current, df_ideal)
}

# manual removal for extra notes
print("----- Remove extra notes -----")
removed_more <- subset(df_removed_onset, df_removed_onset$errorType == "More") #only consider extra notes
df_corrected_more <- data.frame() #create data.frame to store corrected data
for (i in 1:nrow(removed_more)){
  current <- df_onset %>% dplyr::filter(SubNr == removed_more[i,1] & BlockNr == removed_more[i,2] & TrialNr == removed_more[i,3])
  counter <- 0
  continue <- 1
  while (continue == 1){
    print("----- First check -----")
    current <- manual(removed_more, current)
    print("----- Correction check -----")
    manual_more(removed_more, current)
    continue <- menu(c("y", "n", "discard"), title = "Continue removing errors?")
  }
  if (continue != 3){
    df_corrected_more <- rbind(df_corrected_more, current[, -c(5:6)])
  } else if (continue == 3){
    removed_more$errorType[i] <- "Exclude"
    removed_more$errorRowNr[i] <- "Exclude the trial (PitchError more than 5% (3 notes))"
  }
}

# manual addition for missing notes
print("----- Add NA to missing notes -----")
df_removed_onset_less <- subset(df_removed_onset, df_removed_onset$errorType == "Less")
df_rescued_onset_less <- data.frame()
for (i in 1:nrow(df_removed_onset_updated)){
  current <- df_onset %>% dplyr::filter(SubNr == df_removed_onset_errorType[i,1] & BlockNr == df_removed_onset_updated[i,2] & TrialNr == df_removed_onset_updated[i,3])
  current$RowNr <- c(1:nrow(current))
  length_diff <- diff_length(current$Pitch, df_ideal$Pitch)
  table <- data.frame("RowNr" = c(1:length(df_ideal$Pitch)))
  table$Observed <- fill_by_na(current$Pitch)
  table$Ideal <- df_ideal$Pitch
  table$Diff[table$Observed == table$Ideal] <- 0
  table$Diff[table$Observed != table$Ideal] <- 1
  print(table)
  graph <- ggplot() +
    geom_line(data = table, aes(x = RowNr, y = Observed), colour = "#F8766D") +
    geom_line(data = table, aes(x = RowNr, y = Ideal), colour = "#00BFC4") +
    geom_point(data = table, aes(x = RowNr, y = Observed), colour = "#F8766D") +
    geom_point(data = table, aes(x = RowNr, y = Ideal), colour = "#00BFC4") +
    scale_x_continuous("RowNr", table$RowNr) +
    coord_fixed(ratio = 1/5) +
    labs(title = sprintf("SubNr: %i, BlockNr: %i, TrialNr: %i", df_removed_onset_updated[i,1], df_removed_onset_updated[i,2], df_removed_onset_updated[i,3]), y = "Pitch")
  print(graph)
  corrected <- editData(current, viewer = "pane")
  df_rescued_onset_less <- rbind(df_rescued_onset_less, corrected)
}

# insert NA row and then edit dataframe

####################################
# Export csv files
####################################
# Export a csv file for df_all
write.csv(df_all, file = "./filtered/data_all.csv", row.names = F)

# Create data only containing metronome sounds
df_metro <- raw_data %>% dplyr::filter(Pitch == 31 | Pitch == 34)
# Export a csv file for df_metro
write.csv(df_metro, file = "./filtered/data_metro.csv", row.names = F)

# Export a csv file for df_onset
write.csv(df_onset, file = "./filtered/data_onset.csv", row.names = F)

# Export a csv file for df_onset_ioi
write.csv(df_onset_ioi, file = "./filtered/data_onset_ioi.csv", row.names = F)

# Export a csv file for df_removed
write.csv(df_removed_onset, file = "./filtered/data_removed_onset.csv", row.names = F)
