#!/usr/local/bin/R
#rm(list=ls(all=T)) - clear all in Grobal Environment

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

####################################
# ONSET
####################################

####################################
# 1. FIRST FILTERING (AUTOMATIC)
####################################
# detect pitch errors
df_removed_onset <- data.frame(t(data.frame(pitch_remover(df_onset, df_ideal)))) #transpose

# create a data frame of removed trials
colnames(df_removed_onset) <- c("SubNr", "BlockNr", "TrialNr", "ErrorMes")
rownames(df_removed_onset) <- c(1:nrow(df_removed_onset))

# mark trials with errors by the first filtering
df_onset$Error <- 0
for (error in 1:nrow(df_removed_onset)){
  df_onset$Error[df_onset$SubNr == df_removed_onset[error,1] & df_onset$BlockNr == df_removed_onset[error,2] & df_onset$TrialNr == df_removed_onset[error,3]] <- 1
}

df_correct_onset <- subset(df_onset, df_onset$Error == 0)

####################################
# 2. SECOND FILTERING (MANUAL)
####################################
# detect errorType and errorRowNr
for (i in 1:nrow(df_removed_onset)){
  current <- df_onset %>% dplyr::filter(SubNr == df_removed_onset[i,1] & BlockNr == df_removed_onset[i,2] & TrialNr == df_removed_onset[i,3])
  current$RowNr <- c(1:nrow(current))
  df_removed_onset <- check(df_removed_onset, current, df_ideal)
}

####################################
# 2.1 manual removal
####################################

########################################################################
# look at Diff (marked as "!!DIFFERENT!!" if there is a difference between
# the current performance and the ideal one) and manually remove errors
# by editData function
#
# if the plot is identical to the ideal, enter y
# if you need to correct the sequence again, enter n
# if you find any idiosyncratic errors, enter Other & describe a reason
########################################################################
print("----- Remove extra notes -----")
removed_more_onset <- subset(df_removed_onset, df_removed_onset$errorType == "More") #only consider extra notes
df_corrected_more_onset <- data.frame() #create data.frame to store corrected data
for (i in 1:nrow(removed_more_onset)){
  current <- df_onset %>% dplyr::filter(SubNr == removed_more_onset[i,1] & BlockNr == removed_more_onset[i,2] & TrialNr == removed_more_onset[i,3])
  decision = 2
  correction = 0 # # of correction for reporting stats
  while (decision == 2){
    print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
    print("----- First check -----")
    current <- manual(current, df_ideal)
    print("----- Correction check -----")
    manual(current, df_ideal)
    decision <- menu(c("y", "n", "other"), title = "Save the current data? (to continue, enter 'n')")
    if (decision == 1){
      correction = correction + 1
      removed_more_onset$errorCorrection[i] <- correction
      df_corrected_more_onset <- rbind(df_corrected_more_onset, current[, -c(5:6)])
    } else if (decision == 3){
      removed_more_onset$errorType[i] <- "Other"
      removed_more_onset$errorCorrection[i] <- NA
      removed_more_onset$errorRowNr[i] <- readline(prompt = "Reason?: ")
    } else if (decision == 2){
      correction = correction + 1
      print("----- Continue correction -----")
    }
  }
}

####################################
# 2.2 manual addition
####################################

########################################################################
# firstly, the algorithm below inserts one NA automatically 
# after inserting NA, check whether the plot is identical to the ideal
# 
# if the plot is identical to the ideal, enter y
# if the plot is not identical, enter Other
########################################################################
print("----- Add NA to missing notes -----")
removed_less_onset <- subset(df_removed_onset, df_removed_onset$errorType == "Less")
df_corrected_less_onset <- data.frame() #create data.frame to store corrected data
for (i in 1:nrow(removed_less_onset)){
  current <- df_onset %>% dplyr::filter(SubNr == removed_less_onset[i,1] & BlockNr == removed_less_onset[i,2] & TrialNr == removed_less_onset[i,3])
  # insert NA row
  current <- insert_na(current, df_ideal)
  print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
  print("----- Correction check -----")
  current <- manual(current, df_ideal)
  decision <- menu(c("y", "other"), title = "Save the current data?")
  if (decision == 1){
    removed_less_onset$errorCorrection[i] <- removed_less_onset$errorDiff[i]
    df_corrected_less_onset <- rbind(df_corrected_less_onset, current[, -c(5:6)])
  } else if (decision == 2){
    removed_less_onset$errorType[i] <- "Other"
    removed_less_onset$errorCorrection[i] <- NA
    removed_less_onset$errorRowNr[i] <- readline(prompt = "Reason?: ")
  }
}

####################################
# 2.3 if equal number of NoteNr
####################################

########################################################################
# look at each plot
#
# if there is a few mishits of the keyboard, replace the values with NAs
# by editData function and enter y
# if there is a complicated problem, enter Other
########################################################################
print("----- Equal -----")
removed_equal_onset <- subset(df_removed_onset, df_removed_onset$errorType == "Equal")
df_corrected_equal_onset <- data.frame() #create data.frame to store correced data
for (i in 1:nrow(removed_equal_onset)){
  current <- df_onset %>% dplyr::filter(SubNr == removed_equal_onset[i,1] & BlockNr == removed_equal_onset[i,2] & TrialNr == removed_equal_onset[i,3])
  decision = 2
  correction = 0 # # of correction for reporting stats
  while (decision == 2){
    print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
    print("----- First check -----")
    current <- manual(current, df_ideal)
    print("----- Correction check -----")
    manual(current, df_ideal)
    decision <- menu(c("y", "n", "other"), title = "Save the current data? (to continue, enter 'n')")
    if (decision == 1){
      correction = correction + 1
      removed_equal_onset$errorCorrection[i] <- correction
      df_corrected_equal_onset <- rbind(df_corrected_equal_onset, current[, -c(5:6)])
    } else if (decision == 3){
      removed_equal_onset$errorType[i] <- "Other"
      removed_equal_onset$errorCorrection[i] <- NA
      removed_equal_onset$errorRowNr[i] <- readline(prompt = "Reason?: ")
    } else if (decision == 2){
      correction = correction + 1
      print("----- Continue correction -----")
    }
  }
}

####################################
# 2.4 individual checking
####################################

########################################################################
# check the data individually by listening to the original midi
# performance and decide whether to correct or discard the data
########################################################################
# !descriptions below are unique to my dataset!
print("----- Check individually -----")
df_removed_onset$errorCorrection <- NA # create errorCorrection column
removed_others_onset <- rbind(subset(df_removed_onset, df_removed_onset$errorType == "Check"),
                        subset(removed_more_onset, removed_more_onset$errorType == "Other"),
                        subset(removed_less_onset, removed_less_onset$errorType == "Other"),
                        subset(removed_equal_onset, removed_equal_onset$errorType == "Other"))
df_removed_onset$errorCorrection <- NULL # delete errorCorrection column
df_corrected_others_onset <- data.frame() #create data.frame to store correced data
print(removed_others_onset) # look at an individual performance

####################################
### 1. SubNr 4, BlockNr 3, TrialNr 1
# He/She stopped playing in the middle and started playing from the beginning >> removed the first try and check errors from the second try.
current431 <- df_onset %>% dplyr::filter(SubNr == removed_others_onset[1,1] & BlockNr == removed_others_onset[1,2] & TrialNr == removed_others_onset[1,3])
current431 <- current431[-c(1:18),]
# check the plot
manual(current431, df_ideal)

removed_others_onset$errorType[1] <- "More"
removed_others_onset$errorCorrection[1] <- 0 #because the second try was successful
df_corrected_others_onset <- rbind(df_corrected_others_onset, current431[,-15])

### 2. SubNr 11, BlockNr 1, TrialNr 1
# Could not finish performing till the end - Exclude
removed_others_onset$errorType[2] <- "Exclude"
removed_others_onset$errorCorrection[2] <- "Could not finish performing till the end"

### 3. SubNr 2, BlockNr 3, TrialNr 6
current236 <- df_onset %>% dplyr::filter(SubNr == removed_others_onset[3,1] & BlockNr == removed_others_onset[3,2] & TrialNr == removed_others_onset[3,3])
# check the plot
manual(current236, df_ideal)

# remove one extra note
current236 <- manual(current236, df_ideal)
# check the plot
manual(current236, df_ideal)

# replace wrong note to NA
current236 <- manual(current236, df_ideal)
# check the plot
manual(current236, df_ideal)

removed_others_onset$errorType[3] <- "Correction"
removed_others_onset$errorCorrection[3] <- "Removal: 1, Replacement to NA: 2"
df_corrected_others_onset <- rbind(df_corrected_others_onset, current236[,-c(5:6)])

### 4. SubNr 10, BlockNr 1, TrialNr 6
# Many mistakes >> correct manually
current1016 <- df_onset %>% dplyr::filter(SubNr == removed_others_onset[4,1] & BlockNr == removed_others_onset[4,2] & TrialNr == removed_others_onset[4,3])
# check the plot
manual(current1016, df_ideal)

# insert one row at the first mistake (RowNr 8)
current1016 <- add_row(current1016, .before = 8)
current1016$Pitch[8] <- df_ideal$Pitch[8]
current1016[c("Key_OnOff", "Device", "Tempo", "SubNr", "BlockNr", "TrialNr", "Skill", "Condition", "Image")][8,] <- current1016[c("Key_OnOff", "Device", "Tempo", "SubNr", "BlockNr", "TrialNr", "Skill", "Condition", "Image")][8-1,]
# check the plot again
manual(current1016, df_ideal)

# remove the first two mistakes
current1016 <- manual(current1016, df_ideal)
current1016 <- manual(current1016, df_ideal)
# check the plot again
manual(current1016, df_ideal) # perfectly corrected
removed_others_onset$errorType[4] <- "Correction"
removed_others_onset$errorCorrection[4] <- "Removal: 2, Addition: 1"
df_corrected_others_onset <- rbind(df_corrected_others_onset, current1016[,-c(5:6)])

### 5. SubNr 7, BlockNr 1, TrialNr 6
# Did not follow the sheet music - Exclude
removed_others_onset$errorType[5] <- "Exclude"
removed_others_onset$errorCorrection[5] <- "Did not follow the sheet music"

# create pitch-error-free responses 
df_correct_onset_updated <- rbind(df_correct_onset[,-15], df_corrected_more_onset, df_corrected_less_onset, df_corrected_equal_onset, df_corrected_others_onset)

# create info about correction
df_error_correction_onset <- rbind(subset(removed_more_onset, removed_more_onset$errorType == "More"), subset(removed_less_onset, removed_less_onset$errorType == "Less"), subset(removed_equal_onset, removed_equal_onset$errorType == "Equal"), removed_others_onset)

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

# Export a csv file for df_correct_onset_updated
write.csv(df_correct_onset_updated, file = "./filtered/data_correct_onset.csv", row.names = F)

# Export a csv file for df_corrected_more_onset
write.csv(df_corrected_more_onset, file = "./filtered/data_corrected_more_onset.csv", row.names = F)

# Export a csv file for df_corrected_less_onset
write.csv(df_corrected_less_onset, file = "./filtered/data_corrected_less_onset.csv", row.names = F)

# Export a csv file for df_corrected_equal_onset
write.csv(df_corrected_equal_onset, file = "./filtered/data_corrected_equal_onset.csv", row.names = F)

# Export a csv file for df_corrected_others_onset
write.csv(df_corrected_others_onset, file = "./filtered/data_corrected_others_onset.csv", row.names = F)

# Export a csv file for df_error_correction_onset
write.csv(df_error_correction_onset, file = "./filtered/data_error_correction_onset.csv", row.names = F)

####################################
# OFFSET
####################################

####################################
# 1. FIRST FILTERING (AUTOMATIC)
####################################
# detect pitch errors
df_removed_offset <- data.frame(t(data.frame(pitch_remover(df_offset, df_ideal)))) #transpose

# create a data frame of removed trials
colnames(df_removed_offset) <- c("SubNr", "BlockNr", "TrialNr", "ErrorMes")
rownames(df_removed_offset) <- c(1:nrow(df_removed_offset))

# mark trials with errors by the first filtering
df_offset$Error <- 0
for (error in 1:nrow(df_removed_offset)){
  df_offset$Error[df_offset$SubNr == df_removed_offset[error,1] & df_offset$BlockNr == df_removed_offset[error,2] & df_offset$TrialNr == df_removed_offset[error,3]] <- 1
}

df_correct_offset <- subset(df_offset, df_offset$Error == 0)

####################################
# 2. SECOND FILTERING (MANUAL)
####################################
# detect errorType and errorRowNr
for (i in 1:nrow(df_removed_offset)){
  current <- df_offset %>% dplyr::filter(SubNr == df_removed_offset[i,1] & BlockNr == df_removed_offset[i,2] & TrialNr == df_removed_offset[i,3])
  current$RowNr <- c(1:nrow(current))
  df_removed_offset <- check(df_removed_offset, current, df_ideal)
}

####################################
# 2.1 manual removal for extra notes
####################################

########################################################################
# look at Diff (marked as "!!!!!" if there is a difference between
# the current performance and the ideal one) and manually remove errors
# by editData function
#
# if the plot is identical to the ideal, enter y
# if you need to correct the sequence again, enter n
# if you find any idiosyncratic errors, enter Other & describe a reason
########################################################################
print("----- Remove extra notes -----")
removed_more_offset <- subset(df_removed_offset, df_removed_offset$errorType == "More") #only consider extra notes
df_corrected_more_offset <- data.frame() #create data.frame to store corrected data
for (i in 1:nrow(removed_more_offset)){
  current <- df_offset %>% dplyr::filter(SubNr == removed_more_offset[i,1] & BlockNr == removed_more_offset[i,2] & TrialNr == removed_more_offset[i,3])
  decision = 2
  correction = 0 # # of correction for reporting stats
  while (decision == 2){
    print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
    print("----- First check -----")
    current <- manual(current, df_ideal)
    print("----- Correction check -----")
    manual(current, df_ideal)
    decision <- menu(c("y", "n", "other"), title = "Save the current data? (to continue, enter 'n')")
    if (decision == 1){
      correction = correction + 1
      removed_more_offset$errorCorrection[i] <- correction
      df_corrected_more_offset <- rbind(df_corrected_more_offset, current[, -c(5:6)])
    } else if (decision == 3){
      removed_more_offset$errorType[i] <- "Other"
      removed_more_offset$errorCorrection[i] <- NA
      removed_more_offset$errorRowNr[i] <- readline(prompt = "Reason?: ")
    } else if (decision == 2){
      correction = correction + 1
      print("----- Continue correction -----")
    }
  }
}

####################################
# 2.2 manual addition for missing notes
####################################

########################################################################
# firstly, the algorithm below inserts one NA automatically 
# after inserting NA, check whether the plot is identical to the ideal
# 
# if the plot is identical to the ideal, enter y
# if the plot is not identical, enter Other
########################################################################
print("----- Add NA to missing notes -----")
removed_less_offset <- subset(df_removed_offset, df_removed_offset$errorType == "Less")
df_corrected_less_offset <- data.frame() #create data.frame to store corrected data
for (i in 1:nrow(removed_less_offset)){
  current <- df_offset %>% dplyr::filter(SubNr == removed_less_offset[i,1] & BlockNr == removed_less_offset[i,2] & TrialNr == removed_less_offset[i,3])
  # insert NA row
  current <- insert_na(current, df_ideal)
  print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
  print("----- Correction check -----")
  current <- manual(current, df_ideal)
  decision <- menu(c("y", "other"), title = "Save the current data?")
  if (decision == 1){
    removed_less_offset$errorCorrection[i] <- removed_less_offset$errorDiff[i]
    df_corrected_less_offset <- rbind(df_corrected_less_offset, current[, -c(5:6)])
  } else if (decision == 2){
    removed_less_offset$errorType[i] <- "Other"
    removed_less_offset$errorCorrection[i] <- NA
    removed_less_offset$errorRowNr[i] <- readline(prompt = "Reason?: ")
  }
}

####################################
# 2.3 if having an equal number of NoteNr
####################################

########################################################################
# look at each plot
#
# if there is a few mishits of the keyboard, replace the values with NAs
# by editData function and enter y
# if there is a complicated problem, enter Other
########################################################################
print("----- Equal -----")
removed_equal_offset <- subset(df_removed_offset, df_removed_offset$errorType == "Equal")
df_corrected_equal_offset <- data.frame() #create data.frame to store correced data
for (i in 1:nrow(removed_equal_offset)){
  current <- df_offset %>% dplyr::filter(SubNr == removed_equal_offset[i,1] & BlockNr == removed_equal_offset[i,2] & TrialNr == removed_equal_offset[i,3])
  decision = 2
  correction = 0 # # of correction for reporting stats
  while (decision == 2){
    print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
    print("----- First check -----")
    current <- manual(current, df_ideal)
    print("----- Correction check -----")
    manual(current, df_ideal)
    decision <- menu(c("y", "n", "other"), title = "Save the current data? (to continue, enter 'n')")
    if (decision == 1){
      correction = correction + 1
      removed_equal_offset$errorCorrection[i] <- correction
      df_corrected_equal_offset <- rbind(df_corrected_equal_offset, current[, -c(5:6)])
    } else if (decision == 3){
      removed_equal_offset$errorType[i] <- "Other"
      removed_equal_offset$errorCorrection[i] <- NA
      removed_equal_offset$errorRowNr[i] <- readline(prompt = "Reason?: ")
    } else if (decision == 2){
      correction = correction + 1
      print("----- Continue correction -----")
    }
  }
}

####################################
# 2.4 individual checking
####################################

########################################################################
# check the data individually by listening to the original midi
# performance and decide whether to correct or discard the data
# !descriptions below are unique to my dataset!
print("----- Check individually -----")
df_removed_offset$errorCorrection <- NA # create errorCorrection column
removed_others_offset <- rbind(subset(df_removed_offset, df_removed_offset$errorType == "Check"),
                              subset(removed_more_offset, removed_more_offset$errorType == "Other"),
                              subset(removed_less_offset, removed_less_offset$errorType == "Other"),
                              subset(removed_equal_offset, removed_equal_offset$errorType == "Other"))
df_removed_offset$errorCorrection <- NULL # delete errorCorrection column
df_corrected_others_offset <- data.frame() #create data.frame to store correced data
print(removed_others_offset) # look at an individual performance

### 1. SubNr 4, BlockNr 3, TrialNr 1
# He/She started stopped playing in the middle and started playing from the beginning >> removed the first try and check errors from the second try.
current431_offset <- df_offset %>% dplyr::filter(SubNr == removed_others_offset[1,1] & BlockNr == removed_others_offset[1,2] & TrialNr == removed_others_offset[1,3])
current431_offset <- current431_offset[-c(1:18),]
# check the plot
manual(current431_offset, df_ideal)

removed_others_offset$errorType[1] <- "More"
removed_others_offset$errorCorrection[1] <- 0 #because the second try was successful
df_corrected_others_offset <- rbind(df_corrected_others_offset, current431_offset[,-c(5:6)])

### 2. SubNr 11, BlockNr 1, TrialNr 1
# Could not finish performing till the end - Exclude
removed_others_offset$errorType[2] <- "Exclude"
removed_others_offset$errorCorrection[2] <- "Could not finish performing till the end"

### 3. SubNr 1, BlockNr 2, TrialNr 3
current123_offset <- df_offset %>% dplyr::filter(SubNr == removed_others_offset[3,1] & BlockNr == removed_others_offset[3,2] & TrialNr == removed_others_offset[3,3])
# check the plot
manual(current123_offset, df_ideal)

# remove the first wrong note
current123_offset <- manual(current123_offset, df_ideal)
# check the plot
manual(current123_offset, df_ideal)

# replace 4 wrong notes to NA
current123_offset <- manual(current123_offset, df_ideal)
# check the plot
manual(current123_offset, df_ideal)

removed_others_offset$errorType[3] <- "Correction"
removed_others_offset$errorCorrection[3] <- "Removal: 1, Replacement to NA: 4"
df_corrected_others_offset <- rbind(df_corrected_others_offset, current123_offset[,-c(5:6)])

### 4. SubNr 1, BlockNr 4, TrialNr 3
current143_offset <- df_offset %>% dplyr::filter(SubNr == removed_others_offset[4,1] & BlockNr == removed_others_offset[4,2] & TrialNr == removed_others_offset[4,3])
# check the plot
manual(current143_offset, df_ideal)

# replace 5 wrong notes to NA
current143_offset <- manual(current143_offset, df_ideal)
# check the plot
manual(current143_offset, df_ideal)

# remove the first wrong note
current143_offset <- manual(current143_offset, df_ideal)
# check the plot
manual(current143_offset, df_ideal)

removed_others_offset$errorType[4] <- "Correction"
removed_others_offset$errorCorrection[4] <- "Removal: 1, Replacement to NA: 5"
df_corrected_others_offset <- rbind(df_corrected_others_offset, current143_offset[,-c(5:6)])

### 5. SubNr 2, BlockNr 3, TrialNr 6
current236_offset <- df_offset %>% dplyr::filter(SubNr == removed_others_offset[5,1] & BlockNr == removed_others_offset[5,2] & TrialNr == removed_others_offset[5,3])
# check the plot
manual(current236_offset, df_ideal)

# replace 3 wrong notes to NA
current236_offset <- manual(current236_offset, df_ideal)
# remove tthe first wrong note
current236_offset <- manual(current236_offset, df_ideal)
# check the plot
manual(current236_offset, df_ideal)

removed_others_offset$errorType[5] <- "Correction"
removed_others_offset$errorCorrection[5] <- "Removal: 1, Replacement to NA: 3"
df_corrected_others_offset <- rbind(df_corrected_others_offset, current236_offset[,-c(5:6)])

### 6. SubNr 8, BlockNr 4,TrialNr 2
current842_offset <- df_offset %>% dplyr::filter(SubNr == removed_others_offset[6,1] & BlockNr == removed_others_offset[6,2] & TrialNr == removed_others_offset[6,3])
# check the plot
manual(current842_offset, df_ideal)

# replace 2 wrong notes to NA
current842_offset <- manual(current842_offset, df_ideal)
# remove the first wrong note
current842_offset <- manual(current842_offset, df_ideal)
# check the plot
manual(current842_offset, df_ideal)

removed_others_offset$errorType[6] <- "Correction"
removed_others_offset$errorCorrection[6] <- "Removal: 1, Replacement to NA: 2"
df_corrected_others_offset <- rbind(df_corrected_others_offset, current842_offset[,-c(5:6)])

# 7. SubNr 10, BlockNr 1, TrialNr 6
# Many mistakes >> correct manually
current1016_offset <- df_offset %>% dplyr::filter(SubNr == removed_others_offset[7,1] & BlockNr == removed_others_offset[7,2] & TrialNr == removed_others_offset[7,3])
# check the plot
manual(current1016_offset, df_ideal)

# insert one row at the first mistake (RowNr 8)
current1016_offset <- add_row(current1016_offset, .before = 8)
current1016_offset$Pitch[8] <- df_ideal$Pitch[8]
current1016_offset[c("Key_OnOff", "Device", "Tempo", "SubNr", "BlockNr", "TrialNr", "Skill", "Condition", "Image")][note,] <- current1016_offset[c("Key_OnOff", "Device", "Tempo", "SubNr", "BlockNr", "TrialNr", "Skill", "Condition", "Image")][note-1,]

# check the plot again
manual(current1016_offset, df_ideal)

# remove the first two errors
current1016_offset <- manual(current1016_offset, df_ideal)
current1016_offset <- manual(current1016_offset, df_ideal)
# check the plot
manual(current1016_offset, df_ideal)

removed_others_offset$errorType[7] <- "Correction"
removed_others_offset$errorCorrection[7] <- "Removal: 2, Addition: 2"
df_corrected_others_offset <- rbind(df_corrected_others_offset, current1016_offset[,-c(5:6)])

### 8. SubNr 15, BlockNr 3, TrialNr 5
current1535_offset <- df_offset %>% dplyr::filter(SubNr == removed_others_offset[8,1] & BlockNr == removed_others_offset[8,2] & TrialNr == removed_others_offset[8,3])
# check the plot
manual(current1535_offset, df_ideal)

# insert NA for the first two errors
current1535_offset <- manual(removed_others_offset, current1535_offset, df_ideal)
# check the plot again
manual(removed_others_offset, current1535_offset, df_ideal)

# replace 2 wrong notes to NA
current1535_offset <- manual(current1535_offset, df_ideal)
# remove the first wrong note
current1535_offset <- manual(current1535_offset, df_ideal)
# check the plot again
manual(current1535_offset, df_ideal)

removed_others_offset$errorType[8] <- "Correction"
removed_others_offset$errorCorrection[8] <- "Removal: 1, Replacement to NA: 2"
df_corrected_others_offset <- rbind(df_corrected_others_offset, current1535_offset[,-c(5:6)])

### 9. SubNr 7, BlockNr 1, TrialNr 6
# Did not follow the sheet music - Exclude
removed_others_offset$errorType[9] <- "Exclude"
removed_others_offset$errorCorrection[9] <- "Did not follow the sheet music"

# create pitch-error-free responses
df_correct_offset_updated <- rbind(df_correct_offset[,-15], df_corrected_more_offset, df_corrected_less_offset, df_corrected_equal_offset, df_corrected_others_offset)

# create info about correction
df_error_correction_offset <- rbind(subset(removed_more_offset, removed_more_offset$errorType == "More"), subset(removed_less_offset, removed_less_offset$errorType == "Less"), subset(removed_equal_offset, removed_equal_offset$errorType == "Equal"), removed_others_offset)

####################################
# Export csv files
####################################
# Export a csv file for df_offset
write.csv(df_offset, file = "./filtered/data_offset.csv", row.names = F)

# Export a csv file for df_correct_offset_updated
write.csv(df_correct_offset_updated, file = "./filtered/data_correct_offset.csv", row.names = F)

# Export a csv file for df_corrected_more_offset
write.csv(df_corrected_more_offset, file = "./filtered/data_corrected_more_offset.csv", row.names = F)

# Export a csv file for df_corrected_less_offset
write.csv(df_corrected_less_offset, file = "./filtered/data_corrected_less_offset.csv", row.names = F)

# Export a csv file for df_corrected_equal_offset
write.csv(df_corrected_equal_offset, file = "./filtered/data_corrected_equal_offset.csv", row.names = F)

# Export a csv file for df_corrected_others_offset
write.csv(df_corrected_others_offset, file = "./filtered/data_corrected_others_offset.csv", row.names = F)

# Export a csv file for df_error_correction_onset
write.csv(df_error_correction_offset, file = "./filtered/data_error_correction_offset.csv", row.names = F)
