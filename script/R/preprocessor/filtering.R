#!/usr/local/bin/R
#rm(list=ls(all=T)) # clear all

# created 24/08/2020
# install packages
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
if (!require("editData")) {install.packages("editData"); require("editData")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}

# create a folder if not exists
if (!file.exists("filtered")){
  dir.create("filtered")
}

# read functions
source("./function.R")

# ggplot settings
theme_set(theme_classic())

# read a text file for ideal performance
dt_ideal <- read.table("./ideal.txt", header = T, sep = ",")

# create a list of data file names
lf <- list.files("./raw_data", pattern = "txt")

# create raw_data - merge all data files into one
raw_data <- data.table()
for (i in 1:length(lf)){
  data_i <- read.table(file.path("./raw_data", lf[i]), header = F, sep = " ", dec = ".")
  raw_data <- rbind(raw_data, data_i)
}

# add column namesls
colnames(raw_data) <- c("NoteNr", "TimeStamp", "Pitch", "Velocity", "Key_OnOff", "Device",
                        "SubNr", "BlockNr", "TrialNr", "Skill", "Condition", "Image")

# clean raw_data
raw_data$NoteNr <- as.numeric(gsub(",", "", raw_data$NoteNr))
raw_data$Image <- gsub(";", "", raw_data$Image)

# sort by SubNr, BlockNr, TrialNr
raw_data <- raw_data[order(raw_data$SubNr, raw_data$BlockNr, raw_data$TrialNr),]
raw_data$RowNr <- c(1:nrow(raw_data))
raw_data <- setcolorder(raw_data, c(13, 1:12))

# raw_data without metronome
dt_all <- raw_data[Pitch != 31 & Pitch != 34]

# onset and offset
dt_onset <- dt_all[Key_OnOff == 1]
dt_offset <- dt_all[Key_OnOff == 0]

####################################
# ONSET
####################################
# 1. Detect error trials
dt_error_onset <- checker(dt_onset, dt_ideal)

# mark imperfect performances
dt_onset$Error <- 0
for (error in 1:nrow(dt_error_onset)){
  dt_onset[SubNr == dt_error_onset$SubNr[error] & BlockNr == dt_error_onset$BlockNr[error] & TrialNr == dt_error_onset$TrialNr[error]]$Error <- 1
}

dt_correct_onset_1 <- dt_onset[Error == 0]

# 2. Manual pitch error removal
dt_error_onset$CorrectionNr <- NA

# extra notes
error_extra <- dt_error_onset[Reason == "Extra Notes"]
dt_correct_onset_2 <- data.table()
for (row in 1:nrow(error_extra)){
  current <- dt_onset[SubNr == error_extra$SubNr[row] & BlockNr == error_extra$BlockNr[row] & TrialNr == error_extra$TrialNr[row]]
  if (nrow(current) > nrow(dt_ideal)){ # extra notes
    decision = 2 
    correction = 0 # # of correction for reporting stats
    while (decision == 2){
      print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
      print("----- First check -----")
      current <- extra(current, dt_ideal)
      print("----- Correction check -----")
      print(current)
      #extra(current, dt_ideal)
      decision <- menu(c("y", "n", "other"), title = "Save the current data? (to continue, enter 'n')")
      if (decision == 1){
        correction = correction + 1
        error_extra$CorrectionNr[row] <- correction
        dt_correct_onset_2 <- rbind(dt_correct_onset_2, current[, -c(5:6)])
      } else if (decision == 3){
        error_extra$CorrectionNr[row] <- readline(prompt = "Reason?: ")
      } else if (decision == 2){
        correction = correction + 1
        print("----- Continue correction -----")
      }
    }
  }
}

# missing notes

# substituted notes

####################################
# OFFSET
####################################











