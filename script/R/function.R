# check whether there are pitch errors

checker <- function(data, ideal){
  dt_errors <- data.table() # return data of erroneous trials - SubNr/BlockNr/TrialNr/Reason
  colnames(dt_errors) <- c("SubNr", "BlockNr", "TrialNr", "Reason")
  for (subject in unique(data$SubNr)){
    print(sprintf("---Subject %i---", subject)){
      for (block in c(1:4)){
        for (trial in c(1:8)){
          current <- data[SubNr == subject & BlockNr == block & TrialNr == trial]
          if (nrow(current) != 0){ # if the current trial is not empty
            if (nrow(ideal) != nrow(current)){ # if there is an extra/missing note
              dt_error <- rbind(dt_error, data.table(subject, block, trial, "NoteNr error"))
            } else if (nrow(ideal) == nrow(current)){ # if there is a wrong substituted note
                boo = TRUE # terminate a loop once the first error was detected
                for (note in 1:nrow(ideal)){
                  # detect error
                  if (current$Pitch[note] != ideal$Pitch[note]){
                    dt_error <- rbind(dt_error, data.table(subject, block, trial, paste("Pitch error - RowNr ", as.character(note), sep = "")))
                    boo = FALSE
                    if (boo == FALSE){
                      break
                    }
                  }
                }
            }
          } else if (nrow(current) == 0){ # if the current trial is empty
            dt_error <- rbind(dt_error, data.table(subject, block, trial, "Missing trial"))
          }
        }
      }
    }
  } return(df_error) # return erroneous trials
}
