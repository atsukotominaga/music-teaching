#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment
#ctrl+shift+C - comment out a block of code

# Install and load required packages
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}

####################################
# Reading & Clearning data
####################################
# Create the list of data files
lf <- list.files('./data', pattern = 'txt')

# Merge all data files into one
raw_data <- data.frame()

for (i in 1:length(lf)){
  data_i <- read.csv(file.path('./data', lf[i]), header = F, sep = " ", dec = '.') #Change the path after the script has been written
  raw_data <- rbind(raw_data, data_i)
}

# Add column names
colnames(raw_data) <- c('NoteNr', 'TimeStamp', 'Pitch', 'Velocity', 'Key_OnOff', 'Device', 'SubNr', 'BlockNr', 'TrialNr', 'Skill', 'Condition', 'Image')

# Clearning raw_data
raw_data$NoteNr <- as.numeric(gsub(",", "", raw_data$NoteNr))
raw_data$Image <- gsub(";", "", raw_data$Image)

# Sorting by SubNr, BlockNr, TrialNr
raw_data <- raw_data[order(raw_data$SubNr, raw_data$BlockNr, raw_data$TrialNr),]
raw_data$Order <- c(1:nrow(raw_data))

# Export a csv file for raw_data
write.csv(raw_data, file = './raw_data.csv', row.names = F)

# Create data without metronome sounds
data_all <- raw_data %>%
  dplyr::filter(Key_OnOff != 10)

# Export a csv file for data_all (without a metronome)
write.csv(data_all, file = './data.csv', row.names = F)

####################################
# Metronome
####################################
# Create data only containing metronome sounds
data_metro <- raw_data %>%
  dplyr::filter(Key_OnOff == 10)

# Export a csv file for data_metro
write.csv(data_metro, file = './data_metro.csv', row.names = F)

####################################
# Remove pitch errors
####################################
# Only MIDI onsets
data_onset <- data_all %>%
  dplyr::filter(Key_OnOff == 1)

# Read ideal_performance
data_ideal <- read.csv('./ideal.csv')

# Create lists about trials containing error / missing
ls_error <- list()
ls_miss <- list()

# Mark pitch errors for onsets
mark_error <- data.frame()
ls_error <- list()
print('Detect pitch errors and missing data')
for (i in unique(data_all$SubNr)){
  for (m in unique(data_all$BlockNr)){
    for (n in unique(data_all$TrialNr)){
      # Extract each trial for each participant
      data_current <- data_onset %>%
        dplyr::filter(SubNr == i & BlockNr == m & TrialNr == n)
      if (nrow(data_current) != 0){ # if data_current is NOT empty
        # Create an error column
        data_current$Error <- 0
        # NoteNr is 51 - detect pitch errors
        if (length(data_current$NoteNr) == length(data_ideal$NoteNr)){
          for (note in 1:length(data_current$NoteNr)){
            if (data_current[note,]$Pitch != data_ideal[note,]$IdealPerformance){
              data_current[note,]$Error <- 1
              mark_error <- rbind(mark_error, data_current[note,])
              ls_error <- c(ls_error, list(c(i, m, n)))
              print(sprintf("Error - SubNr/BlockNr/TrialNr/Order: %i/%i/%i", i, m, n))
            } else {
              mark_error <- rbind(mark_error, data_current[note,])
            }
          }
        # NoteNr is NOT 51 - discard the current trial
        } else {
          data_current$Error <- 1
          mark_error <- rbind(mark_error, data_current)
          ls_error <- c(ls_error, list(c(i, m, n)))
          print(sprintf("Error - SubNr/BlockNr/TrialNr/Order: %i/%i/%i", i, m, n))
        }
      } else if (nrow(data_current) == 0){
        ls_miss <- c(ls_miss, list(c(i, m, n)))
        print(sprintf("Missing data - SubNr/BlockNr/TrialNr/Order: %i/%i/%i", i, m, n))
      }
    }
  }
}

# Create data with pitch errors
data_error <- data.frame()
data_error_c <- data.frame()
for (i in 1:length(ls_error)){
  # # of errors of onsets and offsets
  data_error <- rbind(data_error, data_all %>%
                            dplyr::filter(SubNr == ls_error[[i]][1] & BlockNr == ls_error[[i]][2] & TrialNr == ls_error[[i]][3]))
  # # of errors of onsets
  data_error_c <- rbind(data_error_c, mark_error %>%
                            dplyr::filter(SubNr == ls_error[[i]][1] & BlockNr == ls_error[[i]][2] & TrialNr == ls_error[[i]][3]))
}

# Check whether # of notes in data_error is twice as large as # of notes in data_error_c
if (nrow(data_error) != nrow(data_error_c)*2){
  print('check # of notes in data_error and data_error_c')
}

# Export a csv file for data_error
write.csv(data_error, file = './data_error.csv', row.names = F)

# Mark pitch errors for data
data_all$Error <- 0
for (i in 1:length(data_all$NoteNr)){
  for (m in 1:length(ls_error)){
    if (data_all[i,]$SubNr == ls_error[[m]][1] & data_all[i,]$BlockNr == ls_error[[m]][2] & data_all[i,]$TrialNr == ls_error[[m]][3]){
      data_all[i,]$Error <- 1
    }
  }
}

# Create data without pitch errors
data_analysis <- data_all %>%
  dplyr::filter(Error != 1)

# Export a csv file for data_analysis
write.csv(data_analysis, file = './data_analysis.csv', row.names = F)

# Data for each participant
for (i in unique(raw_data$SubNr)){
  data_i <- data_analysis %>%
    dplyr::filter(SubNr == i)
  var_name <- paste('data_', toString(i), sep = "")
  assign(var_name, data_i)
  # Export csv files for each participant
  write.csv(data_i, file = paste('./', var_name, '.csv', sep = ''))
}