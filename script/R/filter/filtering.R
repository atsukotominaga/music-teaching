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

# setting
# ggplots
theme_set(theme_classic())
theme_update(text = element_text(size = 20, family = "Helvetica Neue LT Std 57 Condensed"), legend.position = "bottom")

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
# 1. FIRST FILTERING (AUTOMATIC)
# raw_data without metronome
df_all <- raw_data %>% dplyr::filter(Pitch != 31 & Pitch != 34)

# onset and offset
df_onset <- df_all %>% dplyr::filter(Key_OnOff == 1)

# calculate IOIs
df_onset$IOI <- diff(c(0, df_onset$TimeStamp))

# remove the first note
df_onset_ioi <- df_onset %>% dplyr::filter(NoteNr != 17)

# list of pitch errors & missing trials
ls_error_onset <- list() 
ls_miss <- list()

# define pitch remove function (pitch_remover)
for (subnr in unique(df_all$SubNr)){
  write(sprintf("---SubNr %i---", subnr), file = "./filtered/errormes.txt", append = T) #export the results to a text file
  print(sprintf("---SubNr %i---", subnr))
  for (block in c(1:4)){
    for (trial in c(1:8)){
      current_onset <- df_onset %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      if (nrow(current_onset) != 0){ #if current data is not empty
        if (length(df_ideal$Pitch) != length(current_onset$NoteNr)){ #if # of onsets is not equal to ideal performance
          ls_error_onset <<- c(ls_error_onset, list(c(subnr, block, trial)))
          write(sprintf("Onset-NoteNr error - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial), file = "./filtered/errormes.txt", append = T)
          print(sprintf("Onset-NoteNr error - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial))
        } else if (length(df_ideal$Pitch) == length(current_onset$Pitch)) { #if # of onsets and offsets are correct
          counter = 0 #set a counter so that the following loop will terminate once it detects one pitch error in a trial
          for (note in 1:length(df_ideal$Pitch)){
            # detect onset error
            if (current_onset[note,]$Pitch != df_ideal$Pitch[note]){
              while (counter == 0){
                ls_error_onset <<- c(ls_error_onset, list(c(subnr, block, trial)))
                write(sprintf("Onset-Pitch error - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note), file = "./filtered/errormes.txt", append = T)
                print(sprintf("Onset-Pitch error - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note))
                counter = counter + 1
              }
            }
          }
        }
      } else { #if current data is emtpy"
        ls_miss <<- c(ls_miss, list(c(subnr, block, trial)))
        write(sprintf("Missing - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial), file = "./filtered/errormes.txt", append = T)
        print(sprintf("Missing - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial))
      }
    }
  }
}

# Create a list of removed trials
ls_removed_onset <- c(ls_error_onset, ls_miss)
# Remove duplication if exists
ls_removed_onset <- unique(ls_removed_onset)
# Create a data frame of removed trials
df_removed_onset <- data.frame(t(data.frame(ls_removed_onset))) # transpose
colnames(df_removed_onset) <- c("SubNr", "BlockNr", "TrialNr")
rownames(df_removed_onset) <- c(1:nrow(df_removed_onset))

# Mark trails with errors by the first filtering
df_onset$Error1 <- 0
df_onset_ioi$Error1 <- 0
for (error in 1:length(ls_removed_onset)){
  df_onset$Error1[df_onset$SubNr == ls_removed_onset[[error]][1] & df_onset$BlockNr == ls_removed_onset[[error]][2] & df_onset$TrialNr == ls_removed_onset[[error]][3]] <- 1
  df_onset_ioi$Error1[df_onset_ioi$SubNr == ls_removed_onset[[error]][1] & df_onset_ioi$BlockNr == ls_removed_onset[[error]][2] & df_onset_ioi$TrialNr == ls_removed_onset[[error]][3]] <- 1
}

# 2. SECOND FILTERING (MANUAL)
# Define fuction (manual)
manual <- function(data, ideal){
  counter = 0
  # define column names
  if (nrow(data) > length(ideal$Pitch) & nrow(data) < length(ideal$Pitch)*1.05){ # if there are more notes than ideal
    df_removed_onset$errorType[i] <<- "More"
    for (note in 1:length(ideal$Pitch)){
      # detect onset error
      if (data[note,]$Pitch != ideal$Pitch[note]){
        while (counter == 0) {
          df_removed_onset$errorRowNr[i] <<- note
          print(sprintf("More: Onset-Pitch error - RowNr: %i", note))
          counter = counter + 1
        } 
      }
    }
  } else if (nrow(data) > length(ideal$Pitch)*0.95 & nrow(data) < length(ideal$Pitch)){ # if there are less note than ideal
    df_removed_onset$errorType[i] <<- "Less"
    for (note in 1:nrow(data)){
      # detect onset error
      if (data[note,]$Pitch != ideal$Pitch[note]){
        while (counter == 0) {
          df_removed_onset$errorRowNr[i] <<- note
          print(sprintf("Less: Onset-Pitch error - RowNr: %i", note))
          counter = counter + 1
        }
      }
    }
  } else if (nrow(data) == length(ideal$Pitch)){ # there are equal notes to ideal
    for (note in 1:nrow(data)){
      df_removed_onset$errorType[i] <<- "Equal"
      # detect onset error
      if (data[note,]$Pitch != ideal$Pitch[note]){
        while (counter == 0) {
          df_removed_onset$errorRowNr[i] <<- note
          print(sprintf("Equal: Onset-Pitch error - RowNr: %i", note))
          counter = counter + 1
        }
      }
    } 
  } else if (nrow(data) >= length(ideal$Pitch)*1.05){ # error more than 5%
    df_removed_onset$errorType[i] <<- "Exclude"
    df_removed_onset$errorRowNr[i] <<- "Exclude the trial (more than 5%)"
    print("Exclude the trial (more than 5%)")
  } else if (nrow(data) <= length(ideal$Pitch)*0.95){ # error less than 5%
    df_removed_onset$errorType[i] <<- "Exclude"
    df_removed_onset$errorRowNr[i] <<- "Exclude the trial (less than 5%)"
    print("Exclude the trial (less than 5%)")
  } else { # other problems
    df_removed_onset$errorType[i] <<- "Other"
    df_removed_onset$errorRowNr[i] <<- "Check individually"
  }
}

# Detect errorType and errorRowNr
for (i in 1:nrow(df_removed_onset)){
  current <- df_onset %>% dplyr::filter(SubNr == df_removed_onset[i,1] & BlockNr == df_removed_onset[i,2] & TrialNr == df_removed_onset[i,3])
  current$RowNr <- c(1:nrow(current))
  manual(current, df_ideal)
}

# manual removal
for (i in 1:nrow(df_removed_onset)){
  current <- df_onset %>% dplyr::filter(SubNr == df_removed_onset[i,1] & BlockNr == df_removed_onset[i,2] & TrialNr == df_removed_onset[i,3])
  current$RowNr <- c(1:nrow(current))
  editData(current, viewer = "pane")
}

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
