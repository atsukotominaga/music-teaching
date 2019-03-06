#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 30/01/2019
# This script organises raw data and removes pitch errors from it.
# GitHub repo (private): https://github.com/atsukotominaga/expertpiano/tree/master/script/R 

####################################
#  Requirements
####################################
# !!! Set working directory to file source location !!!

# Install and load required packages
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}

# Create necessary folders if not exist
# csv
if (!file.exists("csv")){
  dir.create("csv")
}

####################################
# Reading & Clearning data
####################################
# Create a list of data file names
lf <- list.files("./data", pattern = "txt")

# Create raw_data - merge all data files into one
raw_data <- data.frame()
for (i in 1:length(lf)){
  data_i <- read.csv(file.path("./data", lf[i]), header = F, sep = " ", dec = ".")
  raw_data <- rbind(raw_data, data_i)
  }

# Add column namesls
colnames(raw_data) <- c("NoteNr", "TimeStamp", "Pitch", "Velocity", "Key_OnOff", "Device", 
                        "SubNr", "BlockNr", "TrialNr", "Skill", "Condition", "Image")

# Clean raw_data
raw_data$NoteNr <- as.numeric(gsub(",", "", raw_data$NoteNr))
raw_data$Image <- gsub(";", "", raw_data$Image)

# Sort by SubNr, BlockNr, TrialNr
raw_data <- raw_data[order(raw_data$SubNr, raw_data$BlockNr, raw_data$TrialNr),]
raw_data$RowNr <- c(1:nrow(raw_data))
raw_data <- raw_data[c(13, 1:12)]

# Correct labelling (due to a labelling error of the original study / see detail: TBC)
# articulation
df_a <- raw_data %>% dplyr::filter(grepl("stim_a", Image))
  # Correct wrong labelling
df_a$Skill <- "articulation"

# Correct labelling (due to a labelling error of the original study / see detail: TBC)
# dynamics
df_d <- raw_data %>% dplyr::filter(grepl("stim_d", Image))
  # Correct wrong labelling
df_d$Skill <- "dynamics"

# Bind data frames for articulation and dynamics
df_all <- rbind(df_a, df_d)

# Sort by RowNr
df_all <- df_all[order(df_all$RowNr),]

####################################
# Check BlockNr
####################################
# Whether each participants completed all conditions
for (subnr in unique(df_all$SubNr)){
  df_current <- df_all %>% dplyr::filter(SubNr == subnr)
  write(sprintf("----- SubNr %i -----", subnr), file = "./missingBlock.txt", append = TRUE) # Export the results as a txt file
  print(sprintf("----- SubNr %i -----", subnr))
  if (all.equal(unique(df_current$BlockNr), c(1:4))){
    write("No missing block", file = "./missingBlock.txt", append = TRUE)
    write(unique(df_current$BlockNr), file = "./missingBlock.txt", append = TRUE)
    print("No missing block")
    print(unique(df_current$BlockNr))
    } else {
      write("There will be missing blocks", file = "./missingBlock.txt", append = TRUE)
      write(unique(df_current$BlockNr), file = "./missingBlock.txt", append = TRUE)
      print("There will be missing blocks")
      print(unique(df_current$BlockNr))
  }
}

####################################
# Detect pitch errors
####################################
# Only MIDI onsets and offsets
df_note <- df_all %>% dplyr::filter(Key_OnOff != 10)

# MIDI onsets and offsets
df_onset <- df_note %>% dplyr::filter(Key_OnOff == 1)
df_offset <- df_note %>% dplyr::filter(Key_OnOff == 0)

# Read ideal performance
df_ideal <- read.csv("./ideal.csv")

# Find pitch errors and missing data
ls_error <- list() # List - SubNr/BlockNr/TrialNr for pitch errors
ls_miss <- list() # List - SubNr/BlockNr/TrialNr for missing data
for (subnr in unique(df_onset$SubNr)){
  write(sprintf("----- SubNr %i -----", subnr), file = "./error.txt", append = TRUE) # Export the results as a txt file
  print(sprintf("----- SubNr %i -----", subnr))
  for (block in unique(df_onset$BlockNr)){
    for (trial in unique(df_onset$TrialNr)){
      # Extract each trial for each participant
      current_onset <- df_onset %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      current_offset <- df_offset %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      if (nrow(current_onset) != 0){ # if data_current is NOT empty
        # NoteNr (both onsets and offsets) is 67 - detect pitch errors
        if (length(current_onset$NoteNr) == length(df_ideal$NoteNr) & length(current_offset$NoteNr) == length(df_ideal$NoteNr)){
          counter = 0
          for (note in 1:length(df_ideal$NoteNr)){
            # Onset errors
            if (current_onset[note,]$Pitch != df_ideal[note,]$IdealPerformance){
              while (counter == 0){
                ls_error <- c(ls_error, list(c(subnr, block, trial)))
                write(sprintf("Pitch Error (onset) - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note), file = "./error.txt", append = TRUE)
                print(sprintf("Pitch Error (onset) - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note))
                counter = counter + 1
              }
              # Offset errors
            } else if (current_offset[note,]$Pitch != df_ideal[note,]$IdealPerformance){
              while (counter == 0){
                ls_error <- c(ls_error, list(c(subnr, block, trial)))
                write(sprintf("Pitch Error (offset) - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note), file = "./error.txt", append = TRUE)
                print(sprintf("Pitch Error (offset) - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note))
                counter = counter + 1
              }
            }
          }
          # NoteNr (both onsets and offsets) is NOT 67 - discard the current trial
        } else {
          ls_error <- c(ls_error, list(c(subnr, block, trial)))
          write(sprintf("NoteNr Error - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial, note), file = "./error.txt", append = TRUE)
          print(sprintf("NoteNr Error - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial))
        }
      } else if (nrow(current_onset) == 0){
        ls_miss <- c(ls_miss, list(c(subnr, block, trial)))
        write(sprintf("Missing - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial), file = "./error.txt", append = TRUE)
        print(sprintf("Missing - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial))
      }
    }
  }
}

# Create data with pitch errors
df_error <- data.frame()
for (error in 1:length(ls_error)){
  # # of errors of onsets and offsets
  df_error <- rbind(df_error, df_note %>%
                      dplyr::filter(SubNr == ls_error[[error]][1] & BlockNr == ls_error[[error]][2] & TrialNr == ls_error[[error]][3]))
}

# Append ls_miss to ls_error
ls_remove <- rbind(ls_error, ls_miss)
# Remove duplication if exists
ls_remove <- unique(ls_remove)

# Calculate error rate
df_er <- data.frame()
for (subnr in unique(df_note$SubNr)){
  error = 0
  for (row in 1:length(ls_error)){
    if (subnr  == ls_error[[row]][1]){
      error = error + 1
    }
  }
  df_er <- rbind(df_er, c(subnr, error, error/32))
}
colnames(df_er) <- c("SubNr", "N", "ErrorRate")

# Determine excluded participants
df_er$Exclude <- "include"
df_er$Exclude[df_er$ErrorRate > 0.1] <- "exclude"

# Descriptive stats for error rate
desc_er <- aggregate(ErrorRate~Exclude, data = df_er, 
                            FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
print(desc_er)
# Export error rate
write.table(desc_er, file = "./errorRate.txt", row.names = F)

# Mark pitch errors for data_all
df_note$Error <- 0
for (error in 1:length(ls_error)){
  df_note$Error[df_note$SubNr == ls_error[[error]][1] & df_note$BlockNr == ls_error[[error]][2] & df_note$TrialNr == ls_error[[error]][3]] <- 1
}

# Create data without pitch errors
data_analysis <- df_note %>% dplyr::filter(Error != 1)

####################################
# Export csv files
####################################
# Export a csv file for df_all
write.csv(df_all, file = "./csv/data_all.csv", row.names = F)

# Create data only containing metronome sounds
df_metro <- df_all %>% dplyr::filter(Key_OnOff == 10)
# Export a csv file for data_metro
write.csv(df_metro, file = "./csv/data_metro.csv", row.names = F)

# Export a csv file for data_error
write.csv(df_error, file = "./csv/data_error.csv", row.names = F)

# Export a csv file for data_error_rate
write.csv(df_er, file = "./csv/data_errorRate.csv", row.names = F)

# Export a csv file for data_analysis
write.csv(data_analysis, file = "./csv/data_analysis.csv", row.names = F)

# Data for each participant
for (i in unique(data_analysis$SubNr)){
  data_i <- data_analysis %>% dplyr::filter(SubNr == i)
  var_name <- paste("data_", toString(i), sep = "")
  assign(var_name, data_i)
  # Export csv files for each participant
  write.csv(data_i, file = paste("./csv/", var_name, ".csv", sep = ""))
}