## ----setup, include = FALSE--------------------------------------------------------
# packages
# data manipulation
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
# plot
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("here")) {install.packages("here"); require("here")}

# set working directory
here::i_am("error_summary.Rmd")

# function
source(here("function.R"))

# read a text file for ideal performance
dt_ideal <- read.table(here("ideal.txt"))
colnames(dt_ideal) <- "Pitch"
dt_ideal$RowNr <- 1:nrow(dt_ideal)
setcolorder(dt_ideal, c(2, 1))
# 
dt_onset <- fread(file = here("filtered", "dt_correct_onset.txt"), sep = ",")
dt_offset <- fread(file = here("filtered", "dt_correct_offset.txt"), sep = ",")
error_onset <- fread(file = here("filtered", "error_onset.txt"), sep = ",")
error_offset <- fread(file = here("filtered", "error_offset.txt"), sep = ",")


## ----onset-all, echo = FALSE-------------------------------------------------------
error_onset_all <- error_onset[CorrectionNr != "Exclude"]
error_onset_all


## ----onset-summary, echo = FALSE---------------------------------------------------
error_onset_all[startsWith(Reason, "Substituted")]$Reason <- "Substituted"
error_onset_summary <- error_onset_all[, .(N = .N, Sum = sum(as.numeric(CorrectionNr))), by = .(Reason)]
error_onset_summary


## ----na-onset-all, echo = FALSE----------------------------------------------------
missing_onset <- checker(dt_onset, dt_ideal)
missing_onset


## ----na-onset-summary, echo = FALSE------------------------------------------------
missing_onset_summary <- missing_onset[, .(N = .N), by = .(Reason)]
missing_onset_summary


## ----offset-all, echo = FALSE------------------------------------------------------
error_offset_all <- error_offset[CorrectionNr != "Exclude",]

# separate errors depending on reasons
# SubNr 11, BlockNr 4, TrialNr 8
error_offset_all <- rbind(error_offset_all, error_offset_all[4])
error_offset_all$Reason[4] <- "Extra Notes"
error_offset_all$CorrectionNr[4] <- 1
error_offset_all$Reason[nrow(error_offset_all)] <- "Substituted"
error_offset_all$CorrectionNr[nrow(error_offset_all)] <- 2
# SubNr 27, BlockNr 3, TrialNr 6
error_offset_all <- rbind(error_offset_all, error_offset_all[5])
error_offset_all$Reason[5] <- "Extra Notes"
error_offset_all$CorrectionNr[5] <- 1
error_offset_all$Reason[nrow(error_offset_all)] <- "Substituted"
error_offset_all$CorrectionNr[nrow(error_offset_all)] <- 6

error_offset_all


## ----offset-summary, echo = FALSE--------------------------------------------------
error_offset_all[startsWith(Reason, "Substituted")]$Reason <- "Substituted"
error_offset_summary <- error_offset_all[, .(N = .N, Sum = sum(as.numeric(CorrectionNr))), by = .(Reason)]
error_offset_summary


## ----na-offset-all, echo = FALSE---------------------------------------------------
missing_offset <- checker(dt_offset, dt_ideal)
missing_offset


## ----na-offset-summary, echo = FALSE-----------------------------------------------
missing_offset_summary <- missing_offset[, .(N = .N), by = .(Reason)]
missing_offset_summary


## ----export, include = FALSE-------------------------------------------------------
knitr::purl("error_summary.Rmd")

