##### FUNCTIONS #####
### define pitch remove function (pitch_remover)
# data: data of all trials (df_onset/df_offset)
# ideal: df_ideal
pitch_remover <- function(data, ideal){
  ls_removed <- list()
  for (subnr in unique(data$SubNr)){ #set # of participants
    print(sprintf("---SubNr %i---", subnr))
    for (block in c(1:4)){
      for (trial in c(1:8)){
        current <- data %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
        if (nrow(current) != 0){ #if current data is not empty
          if (length(ideal$Pitch) != length(current$NoteNr)){ #if # of onsets/offsets is not equal to ideal performance
            ls_removed <- c(ls_removed, list(c(subnr, block, trial, "NoteNr error")))
            print(sprintf("NoteNr error - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial))
          } else if (length(ideal$Pitch) == length(current$Pitch)) { #if # of onsets and offsets are correct
            counter = 0 #set a counter so that the following loop will terminate once it detects one pitch error in a trial
            for (note in 1:length(ideal$Pitch)){
              # detect onset error
              if (current[note,]$Pitch != ideal$Pitch[note]){
                while (counter == 0){
                  ls_removed <- c(ls_removed, list(c(subnr, block, trial, "Pitch error")))
                  print(sprintf("Pitch error - SubNr/BlockNr/TrialNr/NoteNr: %i/%i/%i/%i", subnr, block, trial, note))
                  counter = counter + 1
                }
              }
            }
          }
        } else { #if current data is emtpy"
          ls_removed <- c(ls_removed, list(c(subnr, block, trial, "Missing")))
          print(sprintf("Missing - SubNr/BlockNr/TrialNr: %i/%i/%i", subnr, block, trial))
        }
      }
    }
  }
  return(ls_removed)
}

### check error categories
# removed: info of removed trials (df_removed_onset/df_removed_offset)
# data: data of the current trial
# ideal: df_ideal
check <- function(removed, data, ideal){
  counter = 0
  # define column names
  if (nrow(data) > length(ideal$Pitch) & nrow(data) < length(ideal$Pitch)*1.1){ # if there are more notes than ideal
    removed$errorType[i] <- "More"
    for (note in 1:length(ideal$Pitch)){
      # detect onset error
      if (data[note,]$Pitch != ideal$Pitch[note]){
        while (counter == 0) {
          removed$errorRowNr[i] <- note
          print(sprintf("More: RowNr: %i", note))
          counter = counter + 1
        } 
      }
    }
  } else if (nrow(data) > length(ideal$Pitch)*0.9 & nrow(data) < length(ideal$Pitch)){ # if there are less note than ideal
    removed$errorType[i] <- "Less"
    for (note in 1:nrow(data)){
      # detect onset error
      if (data[note,]$Pitch != ideal$Pitch[note]){
        while (counter == 0) {
          removed$errorRowNr[i] <- note
          print(sprintf("Less: RowNr: %i", note))
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
          print(sprintf("Equal: RowNr: %i", note))
          counter = counter + 1
        }
      }
    } 
  } else if (nrow(data) >= length(ideal$Pitch)*1.05){ # error more than 5%
    removed$errorType[i] <- "Check"
    removed$errorRowNr[i] <- "NoteNr more than 10%"
    print("Check - NoteNr more than 10%")
  } else if (nrow(data) <= length(ideal$Pitch)*0.95){ # error less than 5%
    removed$errorType[i] <- "Check"
    removed$errorRowNr[i] <- "NoteNr less than 10%"
    print("Check - NoteNr less than 10%")
  } else { # other problems
    removed$errorType[i] <- "Other"
    removed$errorRowNr[i] <- "Check individually"
  }
  return(removed)
}

### manual removal
# removed: removed_more/removed_less
# data: data of the current trial
# ideal: df_ideal
manual <- function(removed, data, ideal){
  data$RowNr <- c(1:nrow(data))
  length_diff <- abs(length(data$Pitch) - length(ideal$Pitch))
  data$Ideal <- c(ideal$Pitch, rep(NA, length_diff))
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
    labs(title = sprintf("SubNr: %s, BlockNr: %s, TrialNr: %s", unique(data$SubNr), unique(data$BlockNr), unique(data$TrialNr)), y = "Pitch")
  print(graph)
  corrected <- editData(data, viewer = "pane")
  return(corrected)
}

insert_na <- function(data, ideal){
  # insert NA row
  while (nrow(data) < nrow(ideal)){
    counter = 0
    for (note in 1:nrow(data)){
      if (data$Pitch[note] != ideal$Pitch[note]){
        while (counter == 0){
          data <- add_row(data, .before = note)
          data$Pitch[note] <- ideal$Pitch[note]
          data[c("Key_OnOff", "Device", "Tempo", "SubNr", "BlockNr", "TrialNr", "Skill", "Condition", "Image")][note,] <- data[c("Key_OnOff", "Device", "Tempo", "SubNr", "BlockNr", "TrialNr", "Skill", "Condition", "Image")][note-1,]
          counter = counter + 1
        }
      }
    }
  }
  return(data)
}
