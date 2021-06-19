## ----setup, include = FALSE---------------------------------
# packages
# data manipulation
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
# plot
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}

# read txt files
dt_ioi <- fread(file = "./trimmed/data_ioi_1.txt", sep = ",")
dt_kor <- fread(file = "./trimmed/data_kor.txt", sep = ",")
dt_kot <- fread(file = "./trimmed/data_kot.txt", sep = ",")
dt_vel <- fread(file = "./trimmed/data_vel.txt", sep = ",")
dt_vel_diff <- fread(file = "./trimmed/data_vel_diff.txt", sep = ",")


## ----ioi, echo = FALSE--------------------------------------
dt_ioi


## ----ioi-summary, echo = FALSE------------------------------
dt_ioi[, .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = .(SubNr, Condition, Skill)]


## ----kor, echo = FALSE--------------------------------------
dt_kor


## ----kor-summary, echo = FALSE------------------------------
dt_kor[, .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, Subcomponent)]


## ----kot, echo = FALSE--------------------------------------
dt_kot


## ----kot-summary, echo = FALSE------------------------------
dt_kot[, .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, Subcomponent)]


## ----vel, echo = FALSE--------------------------------------
dt_vel


## ----vel-summary, echo = FALSE------------------------------
dt_vel[, .(N = .N, Mean = mean(Velocity), SD = sd(Velocity)), by = .(SubNr, Condition, Skill, Subcomponent)]


## ----vel-diff, echo = FALSE---------------------------------
dt_vel_diff


## ----vel-diff-summary, echo = FALSE-------------------------
dt_vel_diff[, .(N = .N, Mean = mean(Diff), SD = sd(Diff)), by = .(SubNr, Condition, Skill, Subcomponent)]


## ----export-csv, echo = TRUE--------------------------------
fwrite(dt_ioi[SubNr != 2 & SubNr != 3], file = "./trimmed/data_analysis_ioi.txt", sep = ",")
fwrite(dt_kor[SubNr != 2 & SubNr != 3], file = "./trimmed/data_analysis_kor.txt", sep = ",")
fwrite(dt_kot[SubNr != 2 & SubNr != 3], file = "./trimmed/data_analysis_kot.txt", sep = ",")
fwrite(dt_vel[SubNr != 2 & SubNr != 3], file = "./trimmed/data_analysis_vel.txt", sep = ",")
fwrite(dt_vel_diff[SubNr != 2 & SubNr != 3], file = "./trimmed/data_analysis_vel_diff.txt", sep = ",")


## ----export, include = FALSE--------------------------------
knitr::purl("included_summary.Rmd")

