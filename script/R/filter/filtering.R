#!/usr/local/bin/R
#rm(list=ls(all=T)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 24/06/2019
# This script organises raw data and removes pitch errors.
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

# 1. FIRST FILTERING (AUTOMATIC)
### ONSET ###
# detect pitch errors
ls_removed_onset <- pitch_remover(df_onset, df_ideal)

# create a data frame of removed trials
df_removed_onset <- data.frame(t(data.frame(ls_removed_onset))) # transpose
colnames(df_removed_onset) <- c("SubNr", "BlockNr", "TrialNr", "ErrorMes")
rownames(df_removed_onset) <- c(1:nrow(df_removed_onset))

# mark trails with errors by the first filtering
df_onset$Error <- 0
for (error in 1:length(ls_removed_onset)){
  df_onset$Error[df_onset$SubNr == ls_removed_onset[[error]][1] & df_onset$BlockNr == ls_removed_onset[[error]][2] & df_onset$TrialNr == ls_removed_onset[[error]][3]] <- 1
}

df_correct_onset <- subset(df_onset, df_onset$Error == 0)

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
  decision = 2
  while (decision == 2){
    print(sprintf("SubNr: %i, BlockNr: %i, TrialNr: %i", removed_more[i,1], removed_more[i,2], removed_more[i,3]))
    print("----- First check -----")
    current <- manual(removed_more, current, df_ideal)
    print("----- Correction check -----")
    manual(removed_more, current, df_ideal)
    decision <- menu(c("y", "n", "other"), title = "Save the current data? (to continue, enter 'n')")
    if (decision == 1){
      df_corrected_more <- rbind(df_corrected_more, current[, -c(5:6)])
    } else if (decision == 3){
      removed_more$errorType[i] <- "Other"
      removed_more$errorRowNr[i] <- readline(prompt = "Reason?: ")
    } else if (decision == 2){
      print("----- Continue correction -----")
    }
  }
}

# manual addition for missing notes
print("----- Add NA to missing notes -----")
removed_less <- subset(df_removed_onset, df_removed_onset$errorType == "Less")
df_corrected_less <- data.frame() #create data.frame to store corrected data
for (i in 1:nrow(removed_less)){
  current <- df_onset %>% dplyr::filter(SubNr == removed_less[i,1] & BlockNr == removed_less[i,2] & TrialNr == removed_less[i,3])
  # insert NA row
  current <- insert_na(current, df_ideal)
  print(sprintf("SubNr: %i, BlockNr: %i, TrialNr: %i", removed_less[i,1], removed_less[i,2], removed_less[i,3]))
  print("----- Correction check -----")
  current <- manual(removed_less, current, df_ideal)
  decision <- menu(c("y", "other"), title = "Save the current data?")
  if (decision == 1){
    df_corrected_less <- rbind(df_corrected_less, current[, -c(5:6)])
  } else if (decision == 2){
    removed_less$errorType[i] <- "Other"
    removed_less$errorRowNr[i] <- readline(prompt = "Reason?: ")
  }
}

# others
# having an equal number of NoteNr
print("----- Equal -----")
removed_equal <- subset(df_removed_onset, df_removed_onset$errorType == "Equal")
df_corrected_equal <- data.frame() #create data.frame to store correced data
for (i in 1:nrow(removed_equal)){
  current <- df_onset %>% dplyr::filter(SubNr == removed_equal[i,1] & BlockNr == removed_equal[i,2] & TrialNr == removed_equal[i,3])
  print(sprintf("SubNr: %i, BlockNr: %i, TrialNr: %i", removed_equal[i,1], removed_equal[i,2], removed_equal[i,3]))
  print("----- First check -----")
  current <- manual(removed_equal, current, df_ideal)
  print("----- Correction check -----")
  manual(removed_equal, current, df_ideal)
  decision <- menu(c("y", "n", "other"), title = "Save the current data? (to continue, enter 'n')")
  if (decision == 1){
    df_corrected_equal <- rbind(df_corrected_equal, current[, -c(5:6)])
  } else if (decision == 3){
    removed_equal$errorType[i] <- "Other"
    removed_equal$errorRowNr[i] <- readline(prompt = "Reason?: ")
  } else if (decision == 2){
    print("----- Continue correction -----")
  }
}

# individual checking
# descriptions below are unique to my dataset
print("----- Check individually -----")
removed_others <- rbind(subset(df_removed_onset, df_removed_onset$errorType == "Check"),
                        subset(removed_more, removed_more$errorType == "Other"),
                        subset(removed_less, removed_less$errorType == "Other"),
                        subset(removed_equal, removed_equal$errorType == "Other"))
df_corrected_others <- data.frame() #create data.frame to store correced data

# SubNr 4, BlockNr 3, TrialNr 1
# He/She started stopped playing in the middle and started playing from the beginning >> removed the first try and check errors from the second try.
current431 <- df_onset %>% dplyr::filter(SubNr == removed_others[1,1] & BlockNr == removed_others[1,2] & TrialNr == removed_others[1,3])
current431 <- current431[-c(1:18),]
current431 <- manual(removed_others, current431, df_ideal)
df_corrected_others <- rbind(df_corrected_others, current431[,-c(5:6)])

# SubNr 11, BlockNr 1, TrialNr 1
# Could not finish performing till the end - Exclude

# SubNr 2, BlockNr 3, TrialNr 6
# Stopped in the middle and continued playing - Exclude

# SubNr 10, BlockNr 1, TrialNr 6
# Many mistakes (three times) - Exclude

# SubNr 7, BlockNr 1, TrialNr 1
# Did not follow the sheet music - Exclude

# create pitch-error-free responses
df_correct <- rbind(df_correct_onset[,-15], df_corrected_more, df_corrected_less, df_corrected_equal, df_corrected_others)

##### Additional participants #####
# TBC

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

# Export a csv file for df_correct
write.csv(df_correct, file = "./filtered/data_correct.csv", row.names = F)

# Export a csv file for removed_others
write.csv(removed_others, file = "./filtered/data_check.csv", row.names = F)
