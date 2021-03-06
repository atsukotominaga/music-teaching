#!/usr/local/bin/R
#rm(list=ls(all=T)) # clear all

####################################
#  Documentation
####################################
# Created: 12/11/2020
# This script organises raw data and removes pitch errors.

# set working directory
if (!require("here")) {install.packages("here"); require("here")}
here::i_am("filtering.R")

# create a folder if not exists
if (!file.exists(here("filtered"))){
  dir.create(here("filtered"))
}

# read functions
source(here("function.R"))

# read a text file for ideal performance
dt_ideal <- read.table(here("ideal.txt"))
colnames(dt_ideal) <- "Pitch"
dt_ideal$RowNr <- 1:nrow(dt_ideal)
setcolorder(dt_ideal, c(2, 1))

# create a list of data file names
lf <- list.files(here("raw_data"), pattern = "txt")

# create raw_data - merge all data files into one
raw_data <- data.table()
for (i in 1:length(lf)){
  data_i <- fread(file.path(here("raw_data"), lf[i]), header = F, sep = " ", dec = ".")
  raw_data <- rbind(raw_data, data_i)
}

# add column namesls
colnames(raw_data) <- c("NoteNr", "TimeStamp", "Pitch", "Velocity", "Key_OnOff", "Device", "Tempo", "SubNr", "BlockNr", "TrialNr", "Skill", "Condition", "Image")

# clean raw_data
raw_data$NoteNr <- as.numeric(gsub(",", "", raw_data$NoteNr))
raw_data$Image <- gsub(";", "", raw_data$Image)

# sort by SubNr, BlockNr, TrialNr
raw_data <- raw_data[order(raw_data$SubNr, raw_data$BlockNr, raw_data$TrialNr),]

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
dt_correct_onset_1$RowNr <- rep(1:72, nrow(dt_correct_onset_1)/72)
setcolorder(dt_correct_onset_1, c(15, 1:14))

# export csv
fwrite(dt_correct_onset_1, file = here("filtered", "dt_correct_onset_1.txt"))

# 2. Manual pitch error removal
dt_error_onset$CorrectionNr <- NA

# extra notes
error_extra_onset <- dt_error_onset[Reason == "Extra Notes"]
dt_correct_onset_2 <- data.table()
for (row in 1:nrow(error_extra_onset)){
  current <- dt_onset[SubNr == error_extra_onset$SubNr[row] & BlockNr == error_extra_onset$BlockNr[row] & TrialNr == error_extra_onset$TrialNr[row]]
  decision = 2 
  correction = 0 # # of correction for reporting stats
  while (decision == 2){
    print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
    print("----- First check -----")
    current <- edit(current, dt_ideal)
    print("----- Correction check -----")
    edit(current, dt_ideal)
    decision <- menu(c("y", "n", "other"), title = "Save the current data? (to continue, enter 'n')")
    if (decision == 1){
      correction = correction + 1
      error_extra_onset$CorrectionNr[row] <- correction
      dt_correct_onset_2 <- rbind(dt_correct_onset_2, current[, -c(5:6)])
    } else if (decision == 3){
      error_extra_onset$CorrectionNr[row] <- readline(prompt = "Reason?: ")
    } else if (decision == 2){
      correction = correction + 1
      print("----- Continue correction -----")
    }
  }
}

# export csv
fwrite(dt_correct_onset_2, file = here("filtered", "dt_correct_onset_2.txt"))

# missing notes
error_missing_onset <- dt_error_onset[Reason == "Missing Notes"]
dt_correct_onset_3 <- data.table()
for (row in 1:nrow(error_missing_onset)){
  current <- dt_onset[SubNr == error_missing_onset$SubNr[row] & BlockNr == error_missing_onset$BlockNr[row] & TrialNr == error_missing_onset$TrialNr[row]]
  diff <- abs(nrow(dt_ideal) - nrow(current))
  if (diff < 5){
    # insert NA row
    current <- insert_na(current, dt_ideal)
    print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
    print("----- Correction check -----")
    current <- edit(current, dt_ideal)
    decision <- menu(c("y", "other"), title = "Save the current data?")
    if (decision == 1){
      error_missing_onset$CorrectionNr[row] <- diff
      dt_correct_onset_3 <- rbind(dt_correct_onset_3, current[, -c(5:6)])
    } else if (decision == 2){
      error_missing_onset$CorrectionNr[row] <- readline(prompt = "Reason?: ")
    }
  } else {
    error_missing_onset$CorrectionNr[row] <- "Check individually"
  }
}

# export csv
fwrite(dt_correct_onset_3, file = here("filtered", "dt_correct_onset_3.txt"))

# substituted notes
error_sub_onset <- dt_error_onset[startsWith(Reason, "Substituted")]
dt_correct_onset_4 <- data.table()
for (row in 1:nrow(error_sub_onset)){
  current <- dt_onset[SubNr == error_sub_onset$SubNr[row] & BlockNr == error_sub_onset$BlockNr[row] & TrialNr == error_sub_onset$TrialNr[row]]
  decision = 2 
  correction = 0 # # of correction for reporting stats
  while (decision == 2){
    print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
    print("----- First check -----")
    current <- edit(current, dt_ideal)
    print("----- Correction check -----")
    edit(current, dt_ideal)
    decision <- menu(c("y", "n", "other"), title = "Save the current data? (to continue, enter 'n')")
    if (decision == 1){
      correction = correction + 1
      error_sub_onset$CorrectionNr[row] <- correction
      dt_correct_onset_4 <- rbind(dt_correct_onset_4, current[, -c(5:6)])
    } else if (decision == 3) {
      error_sub_onset$CorrectionNr[row] <- readline(prompt = "Reason?: ")
    } else if (decision == 2){
      correction = correction + 1
      print("----- Continue correction -----")
    }
  }
}

# export csv
fwrite(dt_correct_onset_4, file = here("filtered", "dt_correct_onset_4.txt"))

# combine error
error_onset <- rbind(error_extra_onset, error_missing_onset, error_sub_onset)

### individual corrections
error_ind_onset <- error_onset[startsWith(CorrectionNr, "Check")]

# 1: SubNr 2, BlockNr 3, TrialNr 6
current_1 <- dt_onset[SubNr == error_ind_onset$SubNr[1] & BlockNr == error_ind_onset$BlockNr[1] & TrialNr == error_ind_onset$TrialNr[1]]
# check a plot
edit(current_1, dt_ideal)
# decision: exclude - did not follow sheet music
error_ind_onset$CorrectionNr[1] <- "Exclude"

# 2: SubNr 4, BlockNr 3, TrialNr 1
current_2 <- dt_onset[SubNr == error_ind_onset$SubNr[2] & BlockNr == error_ind_onset$BlockNr[2] & TrialNr == error_ind_onset$TrialNr[2]]
# check a plot
edit(current_2, dt_ideal)
# decision: removed the first attempt and add
current_2 <- current_2[-c(1:18), ]
# check a plot
edit(current_2, dt_ideal)
current_2$RowNr <- c(1:72)
error_ind_onset$CorrectionNr[2] <- 1

# 3: SubNr 10, BlockNr 1, TrialNr 6
current_3 <- dt_onset[SubNr == error_ind_onset$SubNr[3] & BlockNr == error_ind_onset$BlockNr[3] & TrialNr == error_ind_onset$TrialNr[3]]
# check a plot
edit(current_3, dt_ideal)
# decision: insert NA and remove extra notes and add
# insert one NA at the first mistake (RowNr 8)
current_3 <- add_row(current_3, .before = 8)
current_3[8,] <- current_3[7, ]
current_3$Pitch[8] <- dt_ideal$Pitch[8]
current_3$TimeStamp[8] <- NA
current_3$Velocity[8] <- NA
# remove 2 extra notes
current_3 <- edit(current_3, dt_ideal)
current_3$RowNr <- c(1:72)
current_3 <- current_3[,-c(5:6)]
error_ind_onset$CorrectionNr[3] <- "Removal: 2, Insert NA: 1"

# 4: SubNr 11, BlockNr 1, TrialNr 1
current_4 <- dt_onset[SubNr == error_ind_onset$SubNr[4] & BlockNr == error_ind_onset$BlockNr[4] & TrialNr == error_ind_onset$TrialNr[4]]
# decision: exclude - did not follow sheet music 
error_ind_onset$CorrectionNr[4] <- "Exclude"

# 5: SubNr 7, BlockNr 1, TrialNr 6
current_5 <- dt_onset[SubNr == error_ind_onset$SubNr[5] & BlockNr == error_ind_onset$BlockNr[5] & TrialNr == error_ind_onset$TrialNr[5]]
# check a plot
edit(current_5, dt_ideal)
# decision: exclude - did not follow sheet music 
error_ind_onset$CorrectionNr[5] <- "Exclude"

# add from individual corrections
dt_correct_onset_5 <- rbind(current_2, current_3)

# export csv
fwrite(dt_correct_onset_5, file = here("filtered", "dt_correct_onset_5.txt"))

# combine all
dt_correct_onset <- rbind(dt_correct_onset_1, dt_correct_onset_2, dt_correct_onset_3, dt_correct_onset_4, dt_correct_onset_5)

# combine error
error_onset <- rbind(error_ind_onset, error_extra_onset, error_missing_onset, error_sub_onset)
error_onset$Duplicate <- duplicated(error_onset[,1:3])
error_onset_all <- error_onset[Duplicate == FALSE]

# export csv
fwrite(dt_correct_onset, file = here("filtered", "dt_correct_onset.txt"))
fwrite(error_onset_all, file = here("filtered", "error_onset.txt"))

####################################
# OFFSET
####################################
# 1. Detect error trials
dt_error_offset <- checker(dt_offset, dt_ideal)

# mark imperfect performances
dt_offset$Error <- 0
for (error in 1:nrow(dt_error_offset)){
  dt_offset[SubNr == dt_error_offset$SubNr[error] & BlockNr == dt_error_offset$BlockNr[error] & TrialNr == dt_error_offset$TrialNr[error]]$Error <- 1
}

dt_correct_offset_1 <- dt_offset[Error == 0]
dt_correct_offset_1$RowNr <- rep(1:72, nrow(dt_correct_offset_1)/72)
setcolorder(dt_correct_offset_1, c(15, 1:14))

# export csv
fwrite(dt_correct_offset_1, file = here("filtered", "dt_correct_offset_1.txt"))

# 2. Manual pitch error removal
dt_error_offset$CorrectionNr <- NA

# extra notes
error_extra_offset <- dt_error_offset[Reason == "Extra Notes"]
dt_correct_offset_2 <- data.table()
for (row in 1:nrow(error_extra_offset)){
  current <- dt_offset[SubNr == error_extra_offset$SubNr[row] & BlockNr == error_extra_offset$BlockNr[row] & TrialNr == error_extra_offset$TrialNr[row]]
  decision = 2 
  correction = 0 # # of correction for reporting stats
  while (decision == 2){
    print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
    print("----- First check -----")
    current <- edit(current, dt_ideal)
    print("----- Correction check -----")
    edit(current, dt_ideal)
    decision <- menu(c("y", "n", "other"), title = "Save the current data? (to continue, enter 'n')")
    if (decision == 1){
      correction = correction + 1
      error_extra_offset$CorrectionNr[row] <- correction
      dt_correct_offset_2 <- rbind(dt_correct_offset_2, current[, -c(5:6)])
    } else if (decision == 3){
      error_extra_offset$CorrectionNr[row] <- readline(prompt = "Reason?: ")
    } else if (decision == 2){
      correction = correction + 1
      print("----- Continue correction -----")
    }
  }
}

# export csv
fwrite(dt_correct_offset_2, file = here("filtered", "dt_correct_offset_2.txt"))

# missing notes
error_missing_offset <- dt_error_offset[Reason == "Missing Notes"]
dt_correct_offset_3 <- data.table()
for (row in 1:nrow(error_missing_offset)){
  current <- dt_offset[SubNr == error_missing_offset$SubNr[row] & BlockNr == error_missing_offset$BlockNr[row] & TrialNr == error_missing_offset$TrialNr[row]]
  diff <- abs(nrow(dt_ideal) - nrow(current))
  if (diff < 5){
    # insert NA row
    current <- insert_na(current, dt_ideal)
    print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
    print("----- Correction check -----")
    current <- edit(current, dt_ideal)
    decision <- menu(c("y", "other"), title = "Save the current data?")
    if (decision == 1){
      error_missing_offset$CorrectionNr[row] <- diff
      dt_correct_offset_3 <- rbind(dt_correct_offset_3, current[, -c(5:6)])
    } else if (decision == 2){
      error_missing_offset$CorrectionNr[row] <- readline(prompt = "Reason?: ")
    }
  } else {
    error_missing_offset$CorrectionNr[row] <- "Check individually"
  }
}

# export csv
fwrite(dt_correct_offset_3, file = here("filtered", "dt_correct_offset_3.txt"))

# substituted notes
error_sub_offset <- dt_error_offset[startsWith(Reason, "Substituted")]
dt_correct_offset_4 <- data.table()
for (row in 30:nrow(error_sub_offset)){
  current <- dt_offset[SubNr == error_sub_offset$SubNr[row] & BlockNr == error_sub_offset$BlockNr[row] & TrialNr == error_sub_offset$TrialNr[row]]
  decision = 2 
  correction = 0 # # of correction for reporting stats
  while (decision == 2){
    print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
    print("----- First check -----")
    current <- edit(current, dt_ideal)
    print("----- Correction check -----")
    edit(current, dt_ideal)
    decision <- menu(c("y", "n", "other"), title = "Save the current data? (to continue, enter 'n')")
    if (decision == 1){
      correction = correction + 1
      error_sub_offset$CorrectionNr[row] <- correction
      dt_correct_offset_4 <- rbind(dt_correct_offset_4, current[, -c(5:6)])
    } else if (decision == 3) {
      error_sub_offset$CorrectionNr[row] <- readline(prompt = "Reason?: ")
    } else if (decision == 2){
      correction = correction + 1
      print("----- Continue correction -----")
    }
  }
}

# export csv
fwrite(dt_correct_offset_4, file = here("filtered", "dt_correct_offset_4.txt"))

# combine error
error_offset <- rbind(error_extra_offset, error_missing_offset, error_sub_offset)

### individual corrections
error_ind_offset <- error_offset[startsWith(CorrectionNr, "Check")]

# 1: SubNr 1, BlockNr 2, TrialNr 3
current_1 <- dt_offset[SubNr == error_ind_offset$SubNr[1] & BlockNr == error_ind_offset$BlockNr[1] & TrialNr == error_ind_offset$TrialNr[1]]
# check a plot
edit(current_1, dt_ideal)
# decision: remove one extra note and correct 4 points and add
current_1 <- edit(current_1, dt_ideal) # remove one extra note
current_1 <- edit(current_1, dt_ideal) # correct 4 points
current_1 <- current_1[,-c(5:6)]
current_1$RowNr <- c(1:72)

error_ind_offset$CorrectionNr[1] <- "Removal: 1, Replacement to NA: 4"

# 2: SubNr 1, BlockNr 4, TrialNr 3
current_2 <- dt_offset[SubNr == error_ind_offset$SubNr[2] & BlockNr == error_ind_offset$BlockNr[2] & TrialNr == error_ind_offset$TrialNr[2]]
# check a plot
edit(current_2, dt_ideal)
# decision: correct 5 points and remove one extra note and add
current_2 <- edit(current_2, dt_ideal) # correct 5 points
current_2 <- edit(current_2, dt_ideal) # remove one extra note
current_2 <- current_2[,-c(5:6)]
# check a plot
edit(current_2, dt_ideal)
current_2$RowNr <- c(1:72)
error_ind_offset$CorrectionNr[2] <- 1

# 3: SubNr 2, BlockNr 3, TrialNr 6
current_3 <- dt_offset[SubNr == error_ind_offset$SubNr[3] & BlockNr == error_ind_offset$BlockNr[3] & TrialNr == error_ind_offset$TrialNr[3]]
# check a plot
edit(current_3, dt_ideal)
# decision: exclude - did not follow sheet music
error_ind_offset$CorrectionNr[3] <- "Exclude"

# 4: SubNr 4, BlockNr 3, TrialNr 1
current_4 <- dt_offset[SubNr == error_ind_offset$SubNr[4] & BlockNr == error_ind_offset$BlockNr[4] & TrialNr == error_ind_offset$TrialNr[4]]
# check a plot
edit(current_4, dt_ideal)
# decision: removed the first attempt and add
current_4 <- current_4[-c(1:18), ]
# check a plot
edit(current_4, dt_ideal)
current_4$RowNr <- c(1:72)
error_ind_offset$CorrectionNr[4] <- 0

# 5: SubNr 5, BlockNr 1, TrialNr 1
current_5 <- dt_offset[SubNr == error_ind_offset$SubNr[5] & BlockNr == error_ind_offset$BlockNr[5] & TrialNr == error_ind_offset$TrialNr[5]]
# check a plot
edit(current_5, dt_ideal)
# correct remove one extra note
current_5 <- edit(current_5, dt_ideal) # remove one extra note
current_5 <- current_5[,-c(5:6)]
current_5$RowNr <- c(1:72)
# save correctionNr
error_ind_offset$CorrectionNr[5] <- 1

# 6: SubNr 8, BlockNr 4, TrialNr 2
current_6 <- dt_offset[SubNr == error_ind_offset$SubNr[6] & BlockNr == error_ind_offset$BlockNr[6] & TrialNr == error_ind_offset$TrialNr[6]]
# check a plot
edit(current_6, dt_ideal)
# decision: exclude - correct 2 points and remove one extra note and add
current_6 <- edit(current_6, dt_ideal) # correct 2 points
current_6 <- edit(current_6, dt_ideal) # remove one extra note
current_6 <- current_6[,-c(5:6)]
# check a plot
edit(current_6, dt_ideal)
current_6$RowNr <- c(1:72)
error_ind_offset$CorrectionNr[6] <- "Removal: 1, Replacement to NA: 2"

# 7: SubNr 10, BlockNr 1, TrialNr 6
current_7 <- dt_offset[SubNr == error_ind_offset$SubNr[7] & BlockNr == error_ind_offset$BlockNr[7] & TrialNr == error_ind_offset$TrialNr[7]]
# check a plot
edit(current_7, dt_ideal)
# decision: insert NA and remove extra notes and add
# insert one NA at the first mistake (RowNr 8)
current_7 <- add_row(current_7, .before = 8)
current_7[8,] <- current_7[7, ]
current_7$Pitch[8] <- dt_ideal$Pitch[8]
current_7$TimeStamp[8] <- ""
current_7$Velocity[8] <- ""
# remove 2 extra notes
current_7 <- edit(current_7, dt_ideal)
current_7$RowNr <- c(1:72)
current_7 <- current_7[,-c(5:6)]
error_ind_onset$CorrectionNr[7] <- "Removal: 2, Insert NA: 1"

# 8: SubNr 15, BlockNr 3, TrialNr 5
current_8 <- dt_offset[SubNr == error_ind_offset$SubNr[8] & BlockNr == error_ind_offset$BlockNr[8] & TrialNr == error_ind_offset$TrialNr[8]]
# check a plot
edit(current_8, dt_ideal)
# decision: correct 2 points and remove one extra note and add
current_8 <- edit(current_8, dt_ideal)
current_8$RowNr <- c(1:72)
current_8 <- current_8[,-c(5:6)]
error_ind_offset$CorrectionNr[8] <- "Removal: 1, Replacement to NA: 2"

# 9: SubNr 11, BlockNr 1, TrialNr 1
current_9 <- dt_offset[SubNr == error_ind_offset$SubNr[9] & BlockNr == error_ind_offset$BlockNr[9] & TrialNr == error_ind_offset$TrialNr[9]]
# decision: exclude - did not follow sheet music
error_ind_offset$CorrectionNr[9] <- "Exclude"

# add from individual corrections
dt_correct_offset_5 <-rbind(current_1, current_2, current_4, current_5, current_6, current_7, current_8)

# export csv
fwrite(dt_correct_offset_5, file = here("filtered", "dt_correct_offset_5.txt"))

# combine all
dt_correct_offset <- rbind(dt_correct_offset_1, dt_correct_offset_2, dt_correct_offset_3, dt_correct_offset_4, dt_correct_offset_5)

# combine error
error_offset <- rbind(error_ind_offset, error_extra_offset, error_missing_offset, error_sub_offset)
error_offset$Duplicate <- duplicated(error_offset[,1:3])
error_offset_all <- error_offset[Duplicate == FALSE]

# export csv
fwrite(dt_correct_offset, file = here("filtered", "dt_correct_offset.txt"))
fwrite(error_offset_all, file = here("filtered", "error_offset.txt"))

####################################
# Checking
####################################
rm(list=ls(all=T)) # clear all

# read txt files
dt_correct_onset <- fread(file = here("filtered", "dt_correct_onset.txt"))
dt_correct_offset <- fread(file = here("filtered", "dt_correct_offset.txt"))

# check if duplicates

# read functions
source(here("function.R"))

# read a text file for ideal performance
dt_ideal <- read.table(here("ideal.txt"))
colnames(dt_ideal) <- "Pitch"
dt_ideal$RowNr <- 1:nrow(dt_ideal)
setcolorder(dt_ideal, c(2, 1))

# check whether data look okay
print("--- ONSET ---")
print(checker(dt_correct_onset, dt_ideal))
print("--- OFFSET ---")
print(checker(dt_correct_offset, dt_ideal))

onset_missing <- checker(dt_correct_onset, dt_ideal)
offset_missing <- checker(dt_correct_offset, dt_ideal)
onset_missing$OnOff <- "Onset"
offset_missing$OnOff <- "Offset"

fwrite(rbind(onset_missing, offset_missing), file = here("filtered", "missingTrials.txt"))

# Missing Trial - fine / if there is other than Missing Trial, look at data again
