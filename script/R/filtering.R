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
df_offset$IOI <- diff(c(0, df_offset$TimeStamp))

# remove the first note
df_onset_ioi <- df_onset %>% dplyr::filter(NoteNr != 17)
df_offset_ioi <- df_offset %>% dplyr::filter(NoteNr != 19)

# divide the piece into three parts
# create data.frame
df_onset_1 <- data.frame()
df_offset_1 <- data.frame()
df_onset_2 <- data.frame()
df_offset_2 <- data.frame()
df_onset_3 <- data.frame()
df_offset_3 <- data.frame()

for (subnr in unique(df_all$SubNr)){
  for (block in c(1:4)){
    for (trial in c(1:8)){
      current_onset <- df_onset %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      current_offset <- df_offset %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      current_onset_ioi  <- df_onset_ioi %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial) # detect meaningful pauses to parse out the piece
      
      if (length(current_onset$IOI[current_onset$IOI > 800]) == 4){
        
        # parse the piece into three parts based on onsets
        onset_first <- current_onset %>% dplyr::filter(NoteNr <= current_onset$NoteNr[current_onset_ioi$IOI > 800][1])
        onset_second <- current_onset %>% dplyr::filter(NoteNr > current_onset$NoteNr[current_onset_ioi$IOI > 800][1] & NoteNr <= current_onset$NoteNr[current_onset_ioi$IOI > 800][3])
        onset_third <- current_onset %>% dplyr::filter(NoteNr > current_onset$NoteNr[current_onset_ioi$IOI > 800][3])
        
        offset_first <- current_offset %>% dplyr::filter(NoteNr <= current_onset$NoteNr[current_onset$IOI > 800][1]+1)
        offset_second <- current_offset %>% dplyr::filter(NoteNr > current_onset$NoteNr[current_onset$IOI > 800][1]+1 & NoteNr <= current_onset$NoteNr[current_onset$IOI > 800][3]+1)
        offset_third <- current_offset %>% dplyr::filter(NoteNr > current_onset$NoteNr[current_onset$IOI > 800][3]+1)
        
        # create data.frame for each part
        df_onset_1 <- rbind(df_onset_1, onset_first)
        df_offset_1 <- rbind(df_offset_1, offset_first)
        df_onset_2 <- rbind(df_onset_2, onset_second)
        df_offset_2 <- rbind(df_offset_2, offset_second)
        df_onset_3 <- rbind(df_onset_3, onset_third)
        df_offset_3 <- rbind(df_offset_3, offset_third)}
    }
  }
}

# parse ideal.txt into three parts
ideal$RowNr <- c(1:nrow(ideal))
ideal_1 <- ideal %>% dplyr::filter(RowNr <= 28)
ideal_2 <- ideal %>% dplyr::filter(RowNr > 28 & RowNr <= 52)
ideal_3 <- ideal %>% dplyr::filter(RowNr > 52)

# pitch remove function (pitch_remover)
pitch_remover <- function(onset, offset, ideal){
  for (subnr in unique(df_all$SubNr)){
    write(sprintf("---SubNr %i---", subnr), file = "./filtered/errormes.txt", append = T) #export the results to a text file
    print(sprintf("---SubNr %i---", subnr))
    for (block in c(1:4)){
      for (trial in c(1:8)){
        current_onset <- onset %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
        current_offset <- offset %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
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
}

# detect pitch errors
ls_error <- list() #list(SubNr/BlockNr/TrialNr/NoteNr for pitch errors)
ls_miss <- list() #list(SubNr/BlockNr/TrialNr for missintg trials)

for (i in c(1:3)){
  # pitch error detection for each line
  if (i == 1){
    onset <- df_onset_1
    offset <- df_offset_1
    ideal <- ideal_1
    write(sprintf("------FIRST LINE------", subnr), file = "./filtered/errormes.txt", append = T) #export the results to a text file
    print(sprintf("------FIRST LINE------", subnr))
    } else if (i == 2){
      onset <- df_onset_2
      offset <- df_offset_2
      ideal <- ideal_2
      write(sprintf("------SECOND LINE------", subnr), file = "./filtered/errormes.txt", append = T) #export the results to a text file
      print(sprintf("------SECOND LINE------", subnr))
      } else if (i == 3){
        onset <- df_onset_3
        offset <- df_offset_3
        ideal <- ideal_3
        write(sprintf("------THIRD LINE------", subnr), file = "./filtered/errormes.txt", append = T) #export the results to a text file
        print(sprintf("------THIRD LINE------", subnr))
      }
  pitch_remover(onset, offset, ideal)
}
      
     
      
      
      


# create a list of removed trials
ls_remove <- c(ls_error, ls_miss)
# Remove duplication if exists
ls_remove <- unique(ls_remove)
# Create a data frame of removed trials
df_removed <- t(data.frame(ls_remove)) # transpose
colnames(df_removed) <- c("SubNr", "BlockNr", "TrialNr")
rownames(df_removed) <- c(1:nrow(df_removed))

# calculate error rate
df_errorRate <- data.frame()
for (subnr in unique(df_all$SubNr)){
  error = 0
  for (row in 1:length(ls_remove)){
    if (subnr == ls_remove[[row]][1]){
      error = error + 1
    }
  }
  df_errorRate <- rbind(df_errorRate, c(subnr, error, error/32)) # total trial number is 32 (8 trials * 4 blocks)
}
colnames(df_errorRate) <- c("SubNr", "N", "ErrorRate")

# mark pitch errors for data_all
df_all$Error <- 0
for (error in 1:length(ls_error)){
  df_all$Error[df_all$SubNr == ls_error[[error]][1] & df_all$BlockNr == ls_error[[error]][2] & df_all$TrialNr == ls_error[[error]][3]] <- 1
}

# create data without pitch errors
df_analysis <- df_all %>% dplyr::filter(Error != 1)

# create data only with pitch errors
df_error <- df_all %>% dplyr::filter(Error == 1)

# exclude participants - later

####################################
# Export csv files
####################################
# Export a csv file for df_all
write.csv(df_all, file = "./1iltered/data_all.csv", row.names = F)

# Create data only containing metronome sounds
df_metro <- raw_data %>% dplyr::filter(Pitch == 31 | Pitch == 34)
# Export a csv file for df_metro
write.csv(df_metro, file = "./filtered/data_metro.csv", row.names = F)

# Export a csv file for df_removed
write.csv(df_removed, file = "./filtered/data_removed.csv", row.names = F)

# Export a csv file for df_error
write.csv(df_error, file = "./filtered/data_error.csv", row.names = F)

# Export a csv file for df_errorRate
write.csv(df_errorRate, file = "./filtered/data_errorRate.csv", row.names = F)

# Export a csv file for data_analysis
write.csv(df_analysis, file = "./filtered/data_analysis.csv", row.names = F)
