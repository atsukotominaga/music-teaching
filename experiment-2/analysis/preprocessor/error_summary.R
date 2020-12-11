## ----setup, include = FALSE---------------------------------------------------
# packages
# data manipulation
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
# plot
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}

# function
source("function.R")

# read a text file for ideal performance
dt_ideal_2 <- read.table("ideal.txt")
colnames(dt_ideal_2) <- "Pitch"
dt_ideal_2$RowNr <- 1:nrow(dt_ideal_2)
setcolorder(dt_ideal_2, c(2, 1))
# 
dt_onset_2 <- fread(file = "filtered/dt_correct_onset.txt", sep = ",")
dt_offset_2 <- fread(file = "filtered/dt_correct_offset.txt", sep = ",")
error_onset_2 <- fread(file = "filtered/error_onset.txt", sep = ",")
error_offset_2 <- fread(file = "filtered/error_offset.txt", sep = ",")


## ----onset-all, echo = FALSE--------------------------------------------------
error_onset_all_2 <- error_onset_2[CorrectionNr != "Exclude"]

# separate errors depending on reasons
# SubNr 10, BlockNr 1, TrialNr 6
error_onset_all_2 <- rbind(error_onset_all_2, error_onset_all_2[2])
error_onset_all_2$Reason[2] <- "Extra Notes"
error_onset_all_2$CorrectionNr[2] <- 2
error_onset_all_2$Reason[nrow(error_onset_all_2)] <- "Substituted"
error_onset_all_2$CorrectionNr[nrow(error_onset_all_2)] <- 1

error_onset_all_2


## ----onset-summary, echo = FALSE----------------------------------------------
error_onset_all_2[startsWith(Reason, "Substituted")]$Reason <- "Substituted"
error_onset_summary_2 <- error_onset_all_2[, .(N = .N, Sum = sum(as.numeric(CorrectionNr))), by = .(Reason)]
error_onset_summary_2


## ----na-onset-all, echo = FALSE-----------------------------------------------
missing_onset_2 <- checker(dt_onset_2, dt_ideal_2)
missing_onset_2


## ----na-onset-summary, echo = FALSE-------------------------------------------
missing_onset_summary_2 <- missing_onset_2[, .(N = .N), by = .(Reason)]
missing_onset_summary_2


## ----offset-all, echo = FALSE-------------------------------------------------
error_offset_all_2 <- error_offset_2[CorrectionNr != "Exclude",]

# separate errors depending on reasons
# SubNr 1, BlockNr 2, TrialNr 3
error_offset_all_2 <- rbind(error_offset_all_2, error_offset_all_2[1])
error_offset_all_2$Reason[1] <- "Extra Notes"
error_offset_all_2$CorrectionNr[1] <- 1
error_offset_all_2$Reason[nrow(error_offset_all_2)] <- "Substituted"
error_offset_all_2$CorrectionNr[nrow(error_offset_all_2)] <- 4
# SubNr 8, BlockNr 4, TrialNr 2
error_offset_all_2 <- rbind(error_offset_all_2, error_offset_all_2[5])
error_offset_all_2$Reason[5] <- "Extra Notes"
error_offset_all_2$CorrectionNr[5] <- 1
error_offset_all_2$Reason[nrow(error_offset_all_2)] <- "Substituted"
error_offset_all_2$CorrectionNr[nrow(error_offset_all_2)] <- 2
# SubNr 10, BlockNr 1, TrialNr 6
error_offset_all_2 <- rbind(error_offset_all_2, error_offset_all_2[6])
error_offset_all_2$Reason[6] <- "Extra Notes"
error_offset_all_2$CorrectionNr[6] <- 2
error_offset_all_2$Reason[nrow(error_offset_all_2)] <- "Substituted"
error_offset_all_2$CorrectionNr[nrow(error_offset_all_2)] <- 1
# SubNr 15, BlockNr 3, TrialNr 5
error_offset_all_2 <- rbind(error_offset_all_2, error_offset_all_2[7])
error_offset_all_2$Reason[7] <- "Extra Notes"
error_offset_all_2$CorrectionNr[7] <- 1
error_offset_all_2$Reason[nrow(error_offset_all_2)] <- "Substituted"
error_offset_all_2$CorrectionNr[nrow(error_offset_all_2)] <- 2

error_offset_all_2


## ----offset-summary, echo = FALSE---------------------------------------------
error_offset_all_2[startsWith(Reason, "Substituted")]$Reason <- "Substituted"
error_offset_summary_2 <- error_offset_all_2[, .(N = .N, Sum = sum(as.numeric(CorrectionNr))), by = .(Reason)]
error_offset_summary_2


## ----na-offset-all, echo = FALSE----------------------------------------------
missing_offset_2 <- checker(dt_offset_2, dt_ideal_2)
missing_offset_2


## ----na-offset-summary, echo = FALSE------------------------------------------
missing_offset_summary_2 <- missing_offset_2[, .(N = .N), by = .(Reason)]
missing_offset_summary_2


## ----export, include = FALSE--------------------------------------------------
knitr::purl("error_summary.Rmd")

