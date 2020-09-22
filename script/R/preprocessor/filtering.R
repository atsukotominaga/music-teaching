#!/usr/local/bin/R
#rm(list=ls(all=T)) # clear all

# created 24/08/2020
# create a folder if not exists
if (!file.exists("filtered")){
  dir.create("filtered")
}

# read functions
source("./function.R")

# read a text file for ideal performance
dt_ideal <- read.table("./ideal.txt")
colnames(dt_ideal) <- "Pitch"
dt_ideal$RowNr <- 1:nrow(dt_ideal)
setcolorder(dt_ideal, c(2, 1))

# create a list of data file names
lf <- list.files("./raw_data", pattern = "txt")

# create raw_data - merge all data files into one
raw_data <- data.table()
for (i in 1:length(lf)){
  data_i <- read.table(file.path("./raw_data", lf[i]), header = F, sep = " ", dec = ".")
  raw_data <- rbind(raw_data, data_i)
}

# add column namesls
colnames(raw_data) <- c("NoteNr", "TimeStamp", "Pitch", "Velocity", "Key_OnOff", "Device", "SubNr", "BlockNr", "TrialNr", "Skill", "Condition", "Image")

# clean raw_data
raw_data$NoteNr <- as.numeric(gsub(",", "", raw_data$NoteNr))
raw_data$Image <- gsub(";", "", raw_data$Image)

# sort by SubNr, BlockNr, TrialNr
raw_data <- raw_data[order(raw_data$SubNr, raw_data$BlockNr, raw_data$TrialNr),]
setcolorder(raw_data, c(13, 1:12))

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

# export csv
fwrite(dt_error_onset, file = "./filtered/error_onset.txt")

# mark imperfect performances
dt_onset$Error <- 0
for (error in 1:nrow(dt_error_onset)){
  dt_onset[SubNr == dt_error_onset$SubNr[error] & BlockNr == dt_error_onset$BlockNr[error] & TrialNr == dt_error_onset$TrialNr[error]]$Error <- 1
}

dt_correct_onset_1 <- dt_onset[Error == 0]
dt_correct_onset_1$RowNr <- rep(1:67, nrow(dt_correct_onset_1)/67)
setcolorder(dt_correct_onset_1, c(14, 1:13))

# export csv
fwrite(dt_correct_onset_1, file = "./filtered/dt_correct_onset_1.txt")

# 2. Manual pitch error removal
dt_error_onset$CorrectionNr <- NA

# extra notes
error_extra <- dt_error_onset[Reason == "Extra Notes"]
dt_correct_onset_2 <- data.table()
for (row in 1:nrow(error_extra)){
  current <- dt_onset[SubNr == error_extra$SubNr[row] & BlockNr == error_extra$BlockNr[row] & TrialNr == error_extra$TrialNr[row]]
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

# export csv
fwrite(dt_correct_onset_2, file = "./filtered/dt_correct_onset_2.txt")

# missing notes
error_missing <- dt_error_onset[Reason == "Missing Notes"]
dt_correct_onset_3 <- data.table()
for (row in 1:nrow(error_missing)){
  current <- dt_onset[SubNr == error_missing$SubNr[row] & BlockNr == error_missing$BlockNr[row] & TrialNr == error_missing$TrialNr[row]]
  diff <- abs(nrow(dt_ideal) - nrow(current))
  if (diff < 5){
    # insert NA row
    current <- insert_na(current, dt_ideal)
    print(sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(current$SubNr), unique(current$BlockNr), unique(current$TrialNr)))
    print("----- Correction check -----")
    current <- edit(current, dt_ideal)
    decision <- menu(c("y", "other"), title = "Save the current data?")
    if (decision == 1){
      error_missing$CorrectionNr[row] <- diff
      dt_correct_onset_3 <- rbind(dt_correct_onset_3, current[, -c(5:6)])
    } else if (decision == 2){
      error_missing$CorrectionNr[row] <- readline(prompt = "Reason?: ")
    }
  } else {
    error_missing$CorrectionNr[row] <- "Check individually"
  }
}

# export csv
fwrite(dt_correct_onset_3, file = "./filtered/dt_correct_onset_3.txt")

# substituted notes
error_sub <- dt_error_onset[startsWith(Reason, "Substituted")]
dt_correct_onset_4 <- data.table()
for (row in 1:nrow(error_sub)){
  current <- dt_onset[SubNr == error_sub$SubNr[row] & BlockNr == error_sub$BlockNr[row] & TrialNr == error_sub$TrialNr[row]]
  current <- edit(current, dt_ideal)
  decision <- menu(c("y", "other"), title = "Save the current data?")
  if (decision == 1){
    error_sub$CorrectionNr[row] <- 1
    dt_correct_onset_4 <- rbind(dt_correct_onset_4, current[, -c(5:6)])
  } else if (decision == 2){
    error_sub$CorrectionNr[row] <- readline(prompt = "Reason?: ")
  }
}

# export csv
fwrite(dt_correct_onset_3, file = "./filtered/dt_correct_onset_4.txt")

# combine error
error_onset <- rbind(error_extra, error_missing, error_sub)

# export csv
fwrite(error_onset, file = "./filtered/error_onset.txt")

### individual corrections
error_ind <- error_onset[startsWith(CorrectionNr, "Check") | startsWith(CorrectionNr, "Octave")]

# 1
current_1 <- dt_onset[SubNr == error_ind$SubNr[1] & BlockNr == error_ind$BlockNr[1] & TrialNr == error_ind$TrialNr[1]]
# check a plot
edit(current_1, dt_ideal)
# decision: exclude - did not follow sheet music

# 2
current_2 <- dt_onset[SubNr == error_ind$SubNr[2] & BlockNr == error_ind$BlockNr[2] & TrialNr == error_ind$TrialNr[2]]
# check a plot
edit(current_2, dt_ideal)
# decision: removed the first attempt and add - played twice
current_2 <- insert_na(current_2[-c(1:73), ], dt_ideal)
# check a plot
edit(current_2, dt_ideal)
current_2$RowNr <- c(1:67)

# 3
current_3 <- dt_onset[SubNr == error_ind$SubNr[3] & BlockNr == error_ind$BlockNr[3] & TrialNr == error_ind$TrialNr[3]]
# decision: exclude - did not finish till the end

# 4
current_4 <- dt_onset[SubNr == error_ind$SubNr[4] & BlockNr == error_ind$BlockNr[4] & TrialNr == error_ind$TrialNr[4]]
# decision: exclude - did not follow sheet music

# 5
current_5 <- dt_onset[SubNr == error_ind$SubNr[5] & BlockNr == error_ind$BlockNr[5] & TrialNr == error_ind$TrialNr[5]]
# decision: exclude - did not finish till the end

# 6
current_6 <- dt_onset[SubNr == error_ind$SubNr[6] & BlockNr == error_ind$BlockNr[6] & TrialNr == error_ind$TrialNr[6]]
# decision: exclude - did not finish till the end

# 7
current_7 <- dt_onset[SubNr == error_ind$SubNr[7] & BlockNr == errorå_ind$BlockNr[7] & TrialNr == error_ind$TrialNr[7]]
# check a plot
current_7 <- insert_na(current_7, dt_ideal)
edit(current_7, dt_ideal)
# decision: exclude - did not follow sheet music

# 8 - 16
# decision: exclude - Octave difference

# add from individual corrections
dt_correct_onset_5 <- current_2

# combine all
dt_correct_onset <- rbind(dt_correct_onset_1, dt_correct_onset_2, dt_correct_onset_3, dt_correct_onset_4, dt_correct_onset_5)

# export csv
fwrite(dt_correct_onset_5, file = "./filtered/dt_correct_onset_5.txt")
fwrite(dt_correct_onset, file = "./filtered/dt_correct_onset.txt")

####################################
# OFFSET
####################################






####################################
# Combine ONSET/OFFSET
####################################







