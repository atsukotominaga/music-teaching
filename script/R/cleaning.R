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
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("wesanderson")) {install.packages("wesanderson"); require("wesanderson")}

# Create necessary folders if not exist
# filtered - all of the outputs will be stored in this folder
if (!file.exists("1_filtered")){
  dir.create("1_filtered")
}

####################################
# Reading & Clearning data
####################################
# Create a list of data file names
lf <- list.files("./raw_data", pattern = "txt")

# Create raw_data - merge all data files into one
raw_data <- data.frame()
for (i in 1:length(lf)){
  data_i <- read.csv(file.path("./raw_data", lf[i]), header = F, sep = " ", dec = ".")
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

# Assign info for the order of the condition
# teaching first
raw_data$Order[raw_data$BlockNr == 1 & raw_data$Condition == "teaching"] <- "teaching-first"
raw_data$Order[raw_data$BlockNr == 2 & raw_data$Condition == "teaching"] <- "teaching-first"
raw_data$Order[raw_data$BlockNr == 3 & raw_data$Condition == "performing"] <- "teaching-first"
raw_data$Order[raw_data$BlockNr == 4 & raw_data$Condition == "performing"] <- "teaching-first"
# perforing first
raw_data$Order[raw_data$BlockNr == 1 & raw_data$Condition == "performing"] <- "performing-first"
raw_data$Order[raw_data$BlockNr == 2 & raw_data$Condition == "performing"] <- "performing-first"
raw_data$Order[raw_data$BlockNr == 3 & raw_data$Condition == "teaching"] <- "performing-first"
raw_data$Order[raw_data$BlockNr == 4 & raw_data$Condition == "teaching"] <- "performing-first"

### Optional
# Correct labelling (due to a labelling error of the original study / see detail: TBC)
# articulation
df_a <- raw_data %>% dplyr::filter(grepl("stim_a", Image))
  # Correct wrong labelling
df_a$Skill <- "articulation"

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
  write(sprintf("----- SubNr %i -----", subnr), file = "./1_filtered/missingBlock.txt", append = TRUE) # Export the results as a txt file
  print(sprintf("----- SubNr %i -----", subnr))
  if (all.equal(unique(df_current$BlockNr), c(1:4))){
    write("No missing block", file = "./1_filtered/missingBlock.txt", append = TRUE)
    write(unique(df_current$BlockNr), file = "./1_filtered/missingBlock.txt", append = TRUE)
    print("No missing block")
    print(unique(df_current$BlockNr))
    } else {
      write("There will be missing blocks", file = "./1_filtered/missingBlock.txt", append = TRUE)
      write(unique(df_current$BlockNr), file = "./1_filtered/missingBlock.txt", append = TRUE)
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
  write(sprintf("----- SubNr %i -----", subnr), file = "./1_filtered/errorMessage.txt", append = TRUE) # Export the results as a txt file
  print(sprintf("----- SubNr %i -----", subnr))
  for (block in unique(df_onset$BlockNr)){
    for (trial in unique(df_onset$TrialNr)){
      # Extract each trial for each participant
      current_onset <- df_onset %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      current_offset <- df_offset %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      if (nrow(current_onset) != 0){ # if data_current is NOT empty
        # NoteNr (both onsets and offsets) is 67 - detect pitch errors
        if (length(current_onset$NoteNr) == length(df_ideal$NoteNr) & length(current_offset$NoteNr) == length(df_ideal$NoteNr)){
          counter = 0 # set counter so that the following loop will terminate once it finds one error in a given trial
          for (note in 1:length(df_ideal$NoteNr)){
            # Onset errors
            if (current_onset[note,]$Pitch != df_ideal[note,]$IdealPerformance){
              while (counter == 0){
                ls_error <- c(ls_error, list(c(subnr, block, trial)))
                write(sprintf("Pitch Error (onset) - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note), file = "./1_filtered/errorMessage.txt", append = TRUE)
                print(sprintf("Pitch Error (onset) - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note))
                counter = counter + 1
              }
              # Offset errors
            } else if (current_offset[note,]$Pitch != df_ideal[note,]$IdealPerformance){
              while (counter == 0){
                ls_error <- c(ls_error, list(c(subnr, block, trial)))
                write(sprintf("Pitch Error (offset) - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note), file = "./1_filtered/errorMessage.txt", append = TRUE)
                print(sprintf("Pitch Error (offset) - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note))
                counter = counter + 1
              }
            }
          }
          # NoteNr (either onsets or offsets) is NOT 67 - discard the current trial
        } else {
          ls_error <- c(ls_error, list(c(subnr, block, trial)))
          write(sprintf("NoteNr Error - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial, note), file = "./1_filtered/errorMessage.txt", append = TRUE)
          print(sprintf("NoteNr Error - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial))
        }
      } else if (nrow(current_onset) == 0){ # if data_current is empty
        ls_miss <- c(ls_miss, list(c(subnr, block, trial)))
        write(sprintf("Missing - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial), file = "./1_filtered/errorMessage.txt", append = TRUE)
        print(sprintf("Missing - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial))
      }
    }
  }
}

# Create a list of removed trials
ls_remove <- c(ls_error, ls_miss)
# Remove duplication if exists
ls_remove <- unique(ls_remove)
# Create a data frame of removed trials
df_removed <- t(data.frame(ls_remove))
colnames(df_removed) <- c("SubNr", "BlockNr", "TrialNr")
rownames(df_removed) <- c(1:nrow(df_removed))

# Calculate error rate
df_errorRate <- data.frame()
for (subnr in unique(df_note$SubNr)){
  error = 0
  for (row in 1:length(ls_remove)){
    if (subnr  == ls_remove[[row]][1]){
      error = error + 1
    }
  }
  df_errorRate <- rbind(df_errorRate, c(subnr, error, error/32))
}
colnames(df_errorRate) <- c("SubNr", "N", "ErrorRate")

####################################
# Exclude participants
####################################
# Determine excluded participants - Less than 10%
df_errorRate$LessThan10 <- "include"
df_errorRate$LessThan10[df_errorRate$ErrorRate > 0.1] <- "exclude"

# Determine excluded participants - Within mean + 2SD
mean <- mean(df_errorRate$N)/32
sd <- sd(df_errorRate$N)/32
df_errorRate$SD <- "include"
df_errorRate$SD[df_errorRate$ErrorRate > mean+2*sd] <- "exclude"

# Determine excluded participants - 75% percentile
percentile75 <- quantile(df_errorRate$ErrorRate, .75)
df_errorRate$Percentile <- "include"
df_errorRate$Percentile[df_errorRate$ErrorRate > percentile75] <- "exclude"

# Descriptive stats for error rate
desc_errorRate_LessThan10 <- aggregate(ErrorRate~LessThan10, data = df_errorRate, FUN = function(x){c(N = length(x), mean = mean(x), median = median(x), sd = sd(x))})
desc_errorRate_LessThan10$Criterion <- "LessThan10"
desc_errorRate_SD <- aggregate(ErrorRate~SD, data = df_errorRate, FUN = function(x){c(N = length(x), mean = mean(x), median = median(x), sd = sd(x))})
desc_errorRate_SD$Criterion <- "Within +2SD"
desc_errorRate_Percentile <- aggregate(ErrorRate~Percentile, data = df_errorRate, FUN = function(x){c(N = length(x), mean = mean(x), median = median(x), sd = sd(x))})
desc_errorRate_Percentile$Criterion <- "75% Percentile"
desc_errorRate <- as.data.frame(rbind(desc_errorRate_LessThan10[,2:3], desc_errorRate_SD[,2:3], desc_errorRate_Percentile[,2:3]))
# Change the order of colnames
desc_errorRate <- desc_errorRate[c(2,1)]
print(desc_errorRate)
# Export error rate
write.csv(desc_errorRate, file = "./1_filtered/errorRate.csv", row.names = TRUE)

# Mark pitch errors for data_all
df_note$Error <- 0
for (error in 1:length(ls_error)){
  df_note$Error[df_note$SubNr == ls_remove[[error]][1] & df_note$BlockNr == ls_remove[[error]][2] & df_note$TrialNr == ls_remove[[error]][3]] <- 1
}

# Create data without pitch errors
df_analysis <- df_note %>% dplyr::filter(Error != 1)

# Create data only with pitch errors
df_error <- df_note %>% dplyr::filter(Error == 1)

####################################
# Histograms
####################################
hist_error_LessThan10 <- ggplot(df_errorRate, aes(x = ErrorRate, fill = LessThan10)) + geom_histogram(position = "identity", binwidth = .01) +
  geom_vline(aes(xintercept = mean(ErrorRate)), color = "black", linetype = "dashed", size = .5) + # mean line
  annotate(geom = "text", x = mean(df_errorRate$ErrorRate)+.05, y = 9, label="Mean", color = "black") +
  geom_vline(aes(xintercept = median(ErrorRate)), color = "black", linetype = "dotted", size = .5) + # median line
  annotate(geom = "text", x = median(df_errorRate$ErrorRate)-.06, y = 9, label = "Median", color = "black") +
  labs(x = "Error Rate", y = "Count", title = "Error Rate Histogram") +
  scale_y_continuous(breaks=seq(0, 10, 1)) +
  scale_fill_manual(values=wes_palette(n = 2, name = "GrandBudapest2")) +
  theme_classic()

hist_error_SD <- ggplot(df_errorRate, aes(x = ErrorRate, fill = SD)) + geom_histogram(position = "identity", binwidth = .01) +
  geom_vline(aes(xintercept = mean(ErrorRate)), color = "black", linetype = "dashed", size = .5) + # mean line
  annotate(geom = "text", x = mean(df_errorRate$ErrorRate)+.05, y = 9, label="Mean", color = "black") +
  geom_vline(aes(xintercept = median(ErrorRate)), color = "black", linetype = "dotted", size = .5) + # median line
  annotate(geom = "text", x = median(df_errorRate$ErrorRate)-.06, y = 9, label = "Median", color = "black") +
  labs(x = "Error Rate", y = "Count", title = "Error Rate Histogram") +
  scale_y_continuous(breaks=seq(0, 10, 1)) +
  scale_fill_manual(values=wes_palette(n = 2, name = "GrandBudapest2")) +
  theme_classic()

hist_error_Percentile <- ggplot(df_errorRate, aes(x = ErrorRate, fill = Percentile)) + geom_histogram(binwidth = .01) +
  geom_vline(aes(xintercept = mean(ErrorRate)), color = "black", linetype = "dashed", size = .5) + # mean line
  annotate(geom = "text", x = mean(df_errorRate$ErrorRate)+.05, y = 9, label="Mean", color = "black") +
  geom_vline(aes(xintercept = median(ErrorRate)), color = "black", linetype = "dotted", size = .5) + # median line
  annotate(geom = "text", x = median(df_errorRate$ErrorRate)-.06, y = 9, label = "Median", color = "black") +
  labs(x = "Error Rate", y = "Count", title = "Error Rate Histogram") +
  scale_y_continuous(breaks=seq(0, 10, 1)) +
  scale_fill_manual(values=wes_palette(n = 2, name = "GrandBudapest2")) +
  theme_classic()

# Export png files
ggsave("./1_filtered/hist_error_LessThan10.png", plot = hist_error_LessThan10, dpi = 600, width = 5, height = 5)
ggsave("./1_filtered/hist_error_SD.png", plot = hist_error_SD, dpi = 600, width = 5, height = 5)
ggsave("./1_filtered/hist_error_Percentile.png", plot = hist_error_Percentile, dpi = 600, width = 5, height = 5)

####################################
# Export csv files
####################################
# Export a csv file for df_all
write.csv(df_all, file = "./1_filtered/data_all.csv", row.names = F)

# Create data only containing metronome sounds
df_metro <- df_all %>% dplyr::filter(Key_OnOff == 10)
# Export a csv file for df_metro
write.csv(df_metro, file = "./1_filtered/data_metro.csv", row.names = F)

# Export a csv file for df_removed
write.csv(df_removed, file = "./1_filtered/data_removed.csv", row.names = F)

# Export a csv file for df_error
write.csv(df_error, file = "./1_filtered/data_error.csv", row.names = F)

# Export a csv file for df_errorRate
write.csv(df_errorRate, file = "./1_filtered/data_errorRate.csv", row.names = F)

# Export a csv file for data_analysis
write.csv(df_analysis, file = "./1_filtered/data_analysis.csv", row.names = F)

# # Data for each participant
# for (i in unique(data_analysis$SubNr)){
#   data_i <- data_analysis %>% dplyr::filter(SubNr == i)
#   var_name <- paste("data_", toString(i), sep = "")
#   assign(var_name, data_i)
#   # Export csv files for each participant
#   write.csv(data_i, file = paste("./1_filtered/", var_name, ".csv", sep = ""))
# }