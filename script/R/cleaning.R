#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 14/12/2018
# This script removes pitch errors from raw data.
# GitHub repo (private): https://github.com/atsukotominaga/expertpiano/tree/master/script/R 

# Install and load required packages
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}

####################################
# Reading & Clearning data
####################################
# Create a list of data files
lf <- list.files('./data', pattern = 'txt')

# Create raw_data - merge all data files into one
raw_data <- data.frame()
for (i in 1:length(lf)){
  data_i <- read.csv(file.path('./data', lf[i]), header = F, sep = " ", dec = '.')
  raw_data <- rbind(raw_data, data_i)
}

# Add column names
colnames(raw_data) <- c('NoteNr', 'TimeStamp', 'Pitch', 'Velocity', 'Key_OnOff', 'Device', 
                        'SubNr', 'BlockNr', 'TrialNr', 'Skill', 'Condition', 'Image')

# Clean raw_data
raw_data$NoteNr <- as.numeric(gsub(",", "", raw_data$NoteNr))
raw_data$Image <- gsub(";", "", raw_data$Image)

# Sort by SubNr, BlockNr, TrialNr
raw_data <- raw_data[order(raw_data$SubNr, raw_data$BlockNr, raw_data$TrialNr),]
raw_data$RowNr <- c(1:nrow(raw_data))
raw_data <- raw_data[c(13, 1:12)]

####################################
# Detect pitch errors
####################################
# Only MIDI onsets and offsets
df_all <- raw_data %>% dplyr::filter(Key_OnOff != 10)

# Only MIDI onsets
df_onset <- raw_data %>% dplyr::filter(Key_OnOff == 1)

# Read ideal performance
df_ideal <- read.csv('./ideal.csv')

# Find pitch errors and missing data
ls_error <- list() # List - SubNr/BlockNr/TrialNr for pitch errors
ls_miss <- list() # List - SubNr/BlockNr/TrialNr for missing data
for (i in unique(df_onset$SubNr)){
  print(sprintf('----- SubNr %i -----', i))
  for (j in unique(df_onset$BlockNr)){
    for (k in unique(df_onset$TrialNr)){
      # Extract each trial for each participant
      df_current <- df_onset %>% dplyr::filter(SubNr == i & BlockNr == j & TrialNr == k)
      if (nrow(df_current) != 0){ # if data_current is NOT empty
        # NoteNr is 51 - detect pitch errors
        if (length(df_current$NoteNr) == length(df_ideal$NoteNr)){
          for (note in 1:length(df_current$NoteNr)){
            if (df_current[note,]$Pitch != df_ideal[note,]$IdealPerformance){
              ls_error <- c(ls_error, list(c(i, j, k)))
              print(sprintf('Error - SubNr/BlockNr/TrialNr: %i/%i/%i', i, j, k))
            }
          }
        # NoteNr is NOT 51 - discard the current trial
        } else {
          ls_error <- c(ls_error, list(c(i, j, k)))
          print(sprintf('Error - SubNr/BlockNr/TrialNr: %i/%i/%i', i, j, k))
        }
      } else if (nrow(df_current) == 0){
        ls_miss <- c(ls_miss, list(c(i, j, k)))
        print(sprintf('Missing - SubNr/BlockNr/TrialNr: %i/%i/%i', i, j, k))
      }
    }
  }
}

# Remove duplicate data
ls_error <- unique(ls_error)

# Create data with pitch errors
df_error <- data.frame()
for (i in 1:length(ls_error)){
  # # of errors of onsets and offsets
  df_error <- rbind(df_error, df_all %>%
                        dplyr::filter(SubNr == ls_error[[i]][1] & BlockNr == ls_error[[i]][2] & TrialNr == ls_error[[i]][3]))
  }

# Mark pitch errors for data_all
df_all$Error <- 0
for (i in 1:length(ls_error)){
  df_all$Error[df_all$SubNr == ls_error[[i]][1] & df_all$BlockNr == ls_error[[i]][2] & df_all$TrialNr == ls_error[[i]][3]] <- 1
}

# Create data without pitch errors
data_analysis <- df_all %>% dplyr::filter(Error != 1)

####################################
# Export csv files
####################################
# Export a csv file for raw_data
write.csv(raw_data, file = './raw_data.csv', row.names = F)
# Create data only containing metronome sounds

# Export a csv file for data_metro
df_metro <- raw_data %>% dplyr::filter(Key_OnOff == 10)
write.csv(df_metro, file = './data_metro.csv', row.names = F)

# Export a csv file for data_error
write.csv(df_error, file = './data_error.csv', row.names = F)

# Export a csv file for data_analysis
write.csv(data_analysis, file = './data_analysis.csv', row.names = F)

# # Data for each participant
# for (i in unique(data_analysis$SubNr)){
#   data_i <- data_analysis %>% dplyr::filter(SubNr == i)
#   var_name <- paste('data_', toString(i), sep = "")
#   assign(var_name, data_i)
#   # Export csv files for each participant
#   write.csv(data_i, file = paste('./', var_name, '.csv', sep = ''))
# }