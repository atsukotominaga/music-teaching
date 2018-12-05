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
data_all <- data.frame()
for (i in 1:length(lf)){
  data_i <- read.csv(file.path('./data', lf[i]), header = F, sep = " ", dec = '.')
  data_all <- rbind(data_all, data_i)
}

# Add column names
colnames(data_all) <- c('NoteNr', 'TimeStamp', 'Pitch', 'Velocity', 'Key_OnOff', 'Device', 'SubNr', 'BlockNr', 'TrialNr', 'Skill', 'Condition', 'Image')

# Clearning data_all
data_all$NoteNr <- as.numeric(gsub(",", "", data_all$NoteNr))
data_all$Image <- gsub(";", "", data_all$Image)

# Sorting by SubNr, BlockNr, TrialNr
data_all <- data_all[order(data_all$SubNr, data_all$BlockNr, data_all$TrialNr),]
data_all$Order <- c(1:nrow(data_all))

# Export a csv file for data_all
write.csv(data_all, file = './data_all.csv', row.names = F)

# Create data without metronome sounds
data <- data_all %>%
  dplyr::filter(Key_OnOff != 10)

# Export a csv file for data (without a metronome)
write.csv(data, file = './data.csv', row.names = F)

# # Data for each participant
# for (i in unique(data_all$SubNr)){
#   data_i <- data_all %>%
#     dplyr::filter(SubNr == i)
#   var_name <- paste('data_', toString(i), sep = "")
#   assign(var_name, data_i)
#   # Export csv files for each participant
#   write.csv(data_i, file = paste('./', var_name, '.csv', sep = ''))
# }

####################################
# Metronome
####################################
# Create data only containing metronome sounds
data_metro <- data_all %>%
  dplyr::filter(Key_OnOff == 10)

# Export a csv file for data_metro
write.csv(data_metro, file = './data_metro.csv', row.names = F)

####################################
# Remove pitch errors
####################################
# Only MIDI onsets
data_onset <- data %>%
  dplyr::filter(Key_OnOff == 1)

# Read ideal_performance
data_ideal <- read.csv('./ideal.csv')

# Create lists about trials containing error / missing
ls_error <- list()
ls_miss <- list()

# Mark pitch errors for only onsets
data_error <- data.frame()
ls_error <- list()
print('Detect pitch errors and missing data')
for (i in unique(data$SubNr)){
  for (m in unique(data$BlockNr)){
    for (n in unique(data$TrialNr)){
      # Extract each trial for each participant
      data_current <- data_onset %>%
        dplyr::filter(SubNr == i & BlockNr == m & TrialNr == n)
      if (nrow(data_current) != 0){ # if data_current is not empty
        # Create an error column
        data_current$Error <- 0
        # NoteNr is 51 - detect pitch errors
        if (length(data_current$NoteNr) == length(data_ideal$NoteNr)){
          for (note in 1:length(data_current$NoteNr)){
            if (data_current[note,]$Pitch != data_ideal[note,]$IdealPerformance){
              data_current[note,]$Error <- 1
              data_error <- rbind(data_error, data_current[note,])
              ls_error <- c(ls_error, list(c(i, m, n)))
              print(sprintf("Error - SubNr/BlockNr/TrialNr: %i/%i/%i", i, m, n))
            } else {
              data_error <- rbind(data_error, data_current[note,])
            }
          }
        # NoteNr is NOT 51 - discard the current trial
        } else {
          data_current$Error <- 1
          data_error <- rbind(data_error, data_current)
          ls_error <- c(ls_error, list(c(i, m, n)))
          print(sprintf("Error - SubNr/BlockNr/TrialNr: %i/%i/%i", i, m, n))
        }
      } else if (nrow(data_current) == 0){
        ls_miss <- c(ls_miss, list(c(i, m, n)))
        print(sprintf("Missing data - SubNr/BlockNr/TrialNr: %i/%i/%i", i, m, n))
      }
    }
  }
}

# Mark pitch errors for data

# Create data without pitch errors
  
# Create data with pitch errors
data_for_error <- data.frame()
data_for_error_check <- data.frame()
for (i in 1:length(ls_error)){
  
  data_for_error <- rbind(data_for_error, data %>%
                            dplyr::filter(SubNr == ls_error[[i]][1] & BlockNr == ls_error[[i]][2] & TrialNr == ls_error[[i]][3]))
  data_for_error_check <- rbind(data_for_error_check, data_error %>%
                            dplyr::filter(SubNr == ls_error[[i]][1] & BlockNr == ls_error[[i]][2] & TrialNr == ls_error[[i]][3]))
}

# Check whether # of notes in data_for_error is twice as large as # of notes in data_for_error_check
if (nrow(data_for_error) != nrow(data_for_error_check)*2){
  print('check # of notes in data_for_error and data_for_error_check')
}

# Export a csv file for data_all (with a metronome)
write.csv(data_for_error, file = './data_error.csv', row.names = F)