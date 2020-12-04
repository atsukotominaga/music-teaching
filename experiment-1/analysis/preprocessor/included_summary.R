## ----setup, include = FALSE---------------------------------------------------------
# packages
# data manipulation
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
# plot
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("here")) {install.packages("here"); require("here")}

# set working directory
here::i_am("included_summary.Rmd")

# read txt files
dt_ioi <- fread(file = here("trimmed", "data_ioi_1.txt"), sep = ",")
dt_kot <- fread(file = here("trimmed", "data_kot.txt"), sep = ",")
dt_vel <- fread(file = here("trimmed", "data_vel.txt"), sep = ",")
dt_vel_diff <- fread(file = here("trimmed", "data_vel_diff.txt"), sep = ",")


## ----ioi, echo = FALSE--------------------------------------------------------------
dt_ioi


## ----ioi-summary, echo = FALSE------------------------------------------------------
dt_ioi[, .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = .(SubNr, Condition, Skill)]


## ----kot, echo = FALSE--------------------------------------------------------------
dt_kot[SubNr != 1]


## ----kot-summary, echo = FALSE------------------------------------------------------
dt_kot[SubNr != 1, .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, Subcomponent)]


## ----vel, echo = FALSE--------------------------------------------------------------
dt_vel[SubNr != 1]


## ----vel-summary, echo = FALSE------------------------------------------------------
dt_vel[SubNr != 1, .(N = .N, Mean = mean(Velocity), SD = sd(Velocity)), by = .(SubNr, Condition, Skill, Subcomponent)]


## ----vel-diff, echo = FALSE---------------------------------------------------------
dt_vel_diff[SubNr != 1]


## ----vel-diff-summary, echo = FALSE-------------------------------------------------
dt_vel_diff[SubNr != 1, .(N = .N, Mean = mean(Diff), SD = sd(Diff)), by = .(SubNr, Condition, Skill, Subcomponent)]


## ----export-csv, echo = TRUE--------------------------------------------------------
fwrite(dt_ioi[SubNr != 1 & SubNr != 3 & SubNr != 8 & SubNr != 14 & SubNr != 16], file = here("trimmed", "data_analysis_ioi.txt"), sep = ",")
fwrite(dt_kot[SubNr != 1 & SubNr != 3 & SubNr != 8 & SubNr != 14 & SubNr != 16], file = here("trimmed", "data_analysis_kot.txt"), sep = ",")
fwrite(dt_vel[SubNr != 1 & SubNr != 3 & SubNr != 8 & SubNr != 14 & SubNr != 16], file = here("trimmed", "data_analysis_vel.txt"), sep = ",")
fwrite(dt_vel_diff[SubNr != 1 & SubNr != 3 & SubNr != 8 & SubNr != 14 & SubNr != 16], file = here("trimmed", "data_analysis_vel_diff.txt"), sep = ",")


## ----export, include = FALSE--------------------------------------------------------
knitr::purl("included_summary.Rmd")

