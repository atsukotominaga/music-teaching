# install and load required packages
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
if (!require("tibble")) {install.packages("tibble"); require("tibble")}
if (!require("editData")) {install.packages("editData"); require("editData")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}

# ggplot settings
theme_set(theme_classic())

### check whether a trial contains pitch errors
# data: data of the current trial
# ideal: dt_ideal
checker <- function(data, ideal){
  dt_errors <- data.table() # return data of erroneous trials - SubNr/BlockNr/TrialNr/Reason
  for (subject in unique(data$SubNr)){
    for(block in c(1:4)){
      for (trial in c(1:8)){
        current <- data[SubNr == subject & BlockNr == block & TrialNr == trial]
        if (nrow(current) != 0){ # current data not empty
          if (nrow(current) != nrow(ideal)){ # extra/missing note
            if (nrow(current) > nrow(ideal)){
              dt_errors <- rbind(dt_errors, data.table(subject, block, trial, "Extra Notes")) 
            } else if (nrow(current) < nrow(ideal)){
              dt_errors <- rbind(dt_errors, data.table(subject, block, trial, "Missing Notes")) 
            }
          } else if (nrow(current) == nrow(ideal)){ # substituted note
            for (note in 1:nrow(ideal)){
              if (current$Pitch[note] != ideal$Pitch[note]){
                dt_errors <- rbind(dt_errors, data.table(subject, block, trial, paste("Substituted Notes - RowNr ", as.character(note), sep = "")))
                break
              }
            }
          }
        } else if (nrow(current) == 0){ # current data empty
          dt_errors <- rbind(dt_errors, data.table(subject, block, trial, paste("Missing Trial")))
        }
      }
    }
  }
  colnames(dt_errors) <- c("SubNr", "BlockNr", "TrialNr", "Reason")
  return(dt_errors)
}

### edit data
# data: data of the current trial
# ideal: dt_ideal
edit <- function(data, ideal){
  data$RowNr <- c(1:nrow(data))
  length_diff <- abs(nrow(data) - nrow(ideal))
  data$Ideal <- c(ideal$Pitch, rep(NA, length_diff))
  data$Diff <- "NA"
  data[Pitch != Ideal]$Diff <- "DIFFERENT"
  # sort the order of columns
  setcolorder(data, c("RowNr", "NoteNr", "TimeStamp", "Pitch", "Ideal", "Diff", "Velocity", "Key_OnOff", "Device", "SubNr", "BlockNr", "TrialNr", "Skill", "Condition", "Image", "Error"))
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
  return(data.table(corrected)) # convert to data.table
}

### insert NA
# data: data of the current trial
# ideal: dt_ideal
insert_na <- function(data, ideal){
  # insert NA row
  while (nrow(data) < nrow(ideal)){
    for (note in 1:nrow(data)){
      if (note != 66){
        if (data$Pitch[note] != ideal$Pitch[note]){
          data <- add_row(data, .before = note)
          data[note] <- data[note-1]
          data$TimeStamp[note] <- NA
          data$Velocity[note] <- NA
          data$Pitch[note] <- ideal$Pitch[note]
          break
        }
      } else if (note == 66){
        if (data$Pitch[note] != ideal$Pitch[note]){
          data <- add_row(data, .after = note)
          data[note+1] <- data[note]
          # NA66
          data$TimeStamp[note] <- NA
          data$Velocity[note] <- NA
          data$Pitch[note] <- ideal$Pitch[note]
          # NA67
          data$TimeStamp[note+1] <- NA
          data$Velocity[note+1] <- NA
          data$Pitch[note+1] <- ideal$Pitch[note+1]
          break
        } else if (data$Pitch[note] == ideal$Pitch[note]){
          data <- add_row(data, .after = note)
          data[note+1] <- data[note]
          data$TimeStamp[note+1] <- NA
          data$Velocity[note+1] <- NA
          data$Pitch[note+1] <- ideal$Pitch[note+1]
          break
        }
      }
    }
  }
  return(data)
}
