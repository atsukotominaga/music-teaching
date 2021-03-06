#!/usr/local/bin/R
# rm(list=ls(all=TRUE)) # clear Grobal Environment

# Documentation
# Created: 09/06/19
# This script creates a condfile.csv

# Create a basic structure
df <- data.frame(
  SubNr = c(1:20),
  Cond1 = rep(c("teaching", "performing"), 10),
  Cond2 = rep(c("performing", "teaching"), 10),
  Inst1 = rep(c("inst_t", "inst_p"), 10),
  Inst2 = rep(c("inst_p", "inst_t"), 10),
  Skill1 = rep(c("articulation", "dynamics"), each = 2, 5),
  Skill2 = rep(c("dynamics", "articulation"), each = 2, 5),
  Image1 = rep(c("stim_a", "stim_d"), each = 2, 5),
  Image2 = rep(c("stim_d", "stim_a"), each = 2, 5)
)

# Add , and ; so that MaxMSP can recognise this file as csv
df$SubNr <- paste(df$SubNr, ",", sep = "")
df$Image2 <- paste(df$Image2, ";", sep = "")

# Add more participants - 15/07/19
df_add01 <- rbind(df[3,], df[6,], df[7,])
df_add01$SubNr <- c("21,", "22,", "23,")
df <- rbind(df, df_add01)

# Export df to csv
write.table(df, file = "./condfile.csv", row.names = FALSE, quote = FALSE, sep = " ")
