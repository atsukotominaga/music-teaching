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
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("wesanderson")) {install.packages("wesanderson"); require("wesanderson")}

# create necessary folders if not exist
# filtered - all of the outputs will be stored in this folder
if (!file.exists("filtered")){
  dir.create("filtered")
}

# read a text file for ideal performance
ideal <- read.table("./ideal.txt")
colnames(ideal) <- "Pitch"

# create a list of data file names
lf <- list.files("./data", pattern = "txt")

# create raw_data - merge all data files into one
raw_data <- data.frame()
for (i in 1:length(lf)){
  data_i <- read.csv(file.path("./data", lf[i]), header = F, sep = " ", dec = ".")
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

# detect pitch errors
ls_error <- list() #list(SubNr/BlockNr/TrialNr/NoteNr for pitch errors)
ls_miss <- list() #list(SubNr/BlockNr/TrialNr for missintg trials)

for (subnr in unique(df_all$SubNr)){
  write(sprintf("---SubNr %i---", subnr), file = "./filtered/errormes.txt", append = T) #export the results to a text file
  print(sprintf("---SubNr %i---", subnr))
  for (block in c(1:4)){
    for (trial in c(1:8)){
      current_onset <- df_onset %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      current_offset <- df_offset %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      if (nrow(current_onset) != 0){ #if current data is not empty
        if (length(ideal$Pitch) != length(current_onset$NoteNr)){ #if # of onsets is not equal to ideal performance
          ls_error <- c(ls_error, list(c(subnr, block, trial)))
          write(sprintf("Onset-NoteNr error - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial), file = "./filtered/errormes.txt", append = T)
          print(sprintf("Onset-NoteNr error - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial))
        } else if (length(ideal$Pitch) != length(current_offset$NoteNr)){ #if # of offset is not equal to ideal performance
          ls_error <- c(ls_error, list(c(subnr, block, trial)))
          write(sprintf("Offset-NoteNr error - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial), file = "./filtered/errormes.txt", append = T)
          print(sprintf("Offset-NoteNr error - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial))
        } else if (length(ideal$Pitch) == length(current_onset$Pitch) & length(ideal$Pitch) == length(current_offset$Pitch)) { #if # of onsets and offsets are correct
          counter = 0 #set a counter so that the following loop will terminate once it detects one pitch error in a trial
          for (note in 1:length(ideal$Pitch)){
            # detect onset error
            if (current_onset[note,]$Pitch != ideal$Pitch[note]){
              while (counter == 0){
                ls_error <- c(ls_error, list(c(subnr, block, trial)))
                write(sprintf("Onset-Pitch error - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note), file = "./filtered/errormes.txt", append = T)
                print(sprintf("Onset-Pitch error - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note))
                counter = counter + 1
              }
            # detect offset error
            } else if (current_offset[note,]$Pitch != ideal$Pitch[note]){
              while (counter == 0){
                ls_error <- c(ls_error, list(c(subnr, block, trial)))
                write(sprintf("Offset-Pitch error - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note), file = "./filtered/errormes.txt", append = T)
                print(sprintf("Offset-Pitch error - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note))
                counter = counter + 1
              }
            }
          }
        }
      } else { #if current data is emtpy"
        ls_miss <- c(ls_miss, list(c(subnr, block, trial)))
        write(sprintf("Missing - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial), file = "./filtered/errormes.txt", append = T)
        print(sprintf("Missing - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial))
      }
    }
  }
}

# create a list of removed trials
ls_remove <- c(ls_error, ls_miss)
# Remove duplication if exists
ls_remove <- unique(ls_remove)
# Create a data frame of removed trials
df_removed <- t(data.frame(ls_remove)) # transpose
colnames(df_removed) <- c("SubNr", "BlockNr", "TrialNr")
rownames(df_removed) <- c(1:nrow(df_removed))
