#### FUNCTIONS #####
### define pitch remove function (pitch_remover)
# data: df_onset/df_offset

pitch_remover <- function(data){
  ls_error <- list()
  ls_miss <- list()
  for (subnr in unique(df_all$SubNr)){
    print(sprintf("---SubNr %i---", subnr))
    for (block in c(1:4)){
      for (trial in c(1:8)){
        current <- data %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
        if (nrow(current) != 0){ #if current data is not empty
          if (length(df_ideal$Pitch) != length(current$NoteNr)){ #if # of onsets is not equal to ideal performance
            ls_error <- c(ls_error, list(c(subnr, block, trial)))
            print(sprintf("Onset-NoteNr error - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial))
          } else if (length(df_ideal$Pitch) == length(current$Pitch)) { #if # of onsets and offsets are correct
            counter = 0 #set a counter so that the following loop will terminate once it detects one pitch error in a trial
            for (note in 1:length(df_ideal$Pitch)){
              # detect onset error
              if (current[note,]$Pitch != df_ideal$Pitch[note]){
                while (counter == 0){
                  ls_error <- c(ls_error, list(c(subnr, block, trial)))
                  print(sprintf("Onset-Pitch error - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note))
                  counter = counter + 1
                }
              }
            }
          }
        } else { #if current data is emtpy"
          ls_miss <- c(ls_miss, list(c(subnr, block, trial)))
          print(sprintf("Missing - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial))
        }
      }
    }
  }
  # create a list of removed trials
  ls_removed <- c(ls_error, ls_miss)
  # remove duplication if exists
  ls_removed <- unique(ls_removed)
  return(ls_removed)
}

### check error categories
# removed: df_removed_onset/df_removed_offset
# data: data of the current trial
# ideal: df_ideal

check <- function(removed, data, ideal){
  counter = 0
  # define column names
  if (nrow(data) > length(ideal$Pitch) & nrow(data) < length(ideal$Pitch)*1.05){ # if there are more notes than ideal
    removed$errorType[i] <- "More"
    for (note in 1:length(ideal$Pitch)){
      # detect onset error
      if (data[note,]$Pitch != ideal$Pitch[note]){
        while (counter == 0) {
          removed$errorRowNr[i] <- note
          print(sprintf("More: Onset-Pitch error - RowNr: %i", note))
          counter = counter + 1
        } 
      }
    }
  } else if (nrow(data) > length(ideal$Pitch)*0.95 & nrow(data) < length(ideal$Pitch)){ # if there are less note than ideal
    removed$errorType[i] <- "Less"
    for (note in 1:nrow(data)){
      # detect onset error
      if (data[note,]$Pitch != ideal$Pitch[note]){
        while (counter == 0) {
          removed$errorRowNr[i] <- note
          print(sprintf("Less: Onset-Pitch error - RowNr: %i", note))
          counter = counter + 1
        }
      }
    }
  } else if (nrow(data) == length(ideal$Pitch)){ # there are equal notes to ideal
    for (note in 1:nrow(data)){
      removed$errorType[i] <- "Equal"
      # detect onset error
      if (data[note,]$Pitch != ideal$Pitch[note]){
        while (counter == 0) {
          removed$errorRowNr[i] <- note
          print(sprintf("Equal: Onset-Pitch error - RowNr: %i", note))
          counter = counter + 1
        }
      }
    } 
  } else if (nrow(data) >= length(ideal$Pitch)*1.05){ # error more than 5%
    removed$errorType[i] <- "Exclude"
    removed$errorRowNr[i] <- "Exclude the trial (NoteNr more than 5%)"
    print("Exclude the trial (NoteNr more than 5%)")
  } else if (nrow(data) <= length(ideal$Pitch)*0.95){ # error less than 5%
    removed$errorType[i] <- "Exclude"
    removed$errorRowNr[i] <- "Exclude the trial (NoteNr less than 5%)"
    print("Exclude the trial (NoteNr less than 5%)")
  } else { # other problems
    removed$errorType[i] <- "Other"
    removed$errorRowNr[i] <- "Check individually"
  }
  return(removed)
}

# calculate the length diff
diff_length <- function(vector1, vector2){
  if(length(vector1) > length(vector2)){
    return(length(vector1) - length(vector2))
  } else {
    return(length(vector2) - length(vector1))
  }
}

# fill the diff with NA
fill_by_na <- function(vector, num){ # num should be the output of diff_length
  return(c(vector, rep(NA, num)))
}

# manual removal
manual <- function(removed, data){
  data$RowNr <- c(1:nrow(data))
  length_diff <- diff_length(data$Pitch, df_ideal$Pitch)
  data$Ideal <- fill_by_na(df_ideal$Pitch, length_diff)
  data$Diff[data$Pitch == data$Ideal] <- 0
  data$Diff[data$Pitch != data$Ideal] <- "!!!!!"
  # sort the order of columns
  data <- data[c("RowNr", "NoteNr", "TimeStamp", "Pitch", "Ideal", "Diff", "Velocity", "Key_OnOff", "Device", "Tempo", "SubNr", "BlockNr", "TrialNr", "Skill", "Condition", "Image")]
  graph <- ggplot() +
    geom_line(data = data, aes(x = RowNr, y = Pitch), colour = "#F8766D") +
    geom_line(data = data, aes(x = RowNr, y = Ideal), colour = "#00BFC4") +
    geom_point(data = data, aes(x = RowNr, y = Pitch), colour = "#F8766D") +
    geom_point(data = data, aes(x = RowNr, y = Ideal), colour = "#00BFC4") +
    scale_x_continuous("RowNr", data$RowNr) +
    coord_fixed(ratio = 1/4) +
    labs(title = sprintf("SubNr: %i, BlockNr: %i, TrialNr: %i", removed[i,1], removed[i,2], removed[i,3]), y = "Pitch")
  print(graph)
  corrected <- editData(data, viewer = "pane")
  return(corrected)
}
