## ----setup, include = FALSE---------------------------------------------------
# packages
if (!require("here")) {install.packages("here"): require("here")}
# data manipulation
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
# plot
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}
# # statistics
# if (!require("afex")) {install.packages("afex"); require("afex")}
# if (!require("rstatix")) {install.packages("rstatix"); require("rstatix")}

# read info about teaching experience
source(here("experiment-1/analysis/demographics", "questionnaire.R"), chdir = TRUE)


## ----file, include = FALSE----------------------------------------------------
filename_ioi = "../preprocessor/trimmed/data_analysis_ioi.txt"


## ----extract, include = FALSE-------------------------------------------------
dt_ioi <- fread(filename_ioi, header = T, sep = ",", dec = ".") # read a trimmed csv

# SubNr as a factor
dt_ioi$SubNr <- as.factor(dt_ioi$SubNr)

# Include only articulation
dt_ioi_art <- dt_ioi[Skill == "articulation"]
# Include only dynamics
dt_ioi_dyn <- dt_ioi[Skill == "dynamics"]


## ----ioi-art, echo = FALSE----------------------------------------------------
# For each individual
ioi_art_trial <- dt_ioi_art[, .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)]
setorder(ioi_art_trial, "SubNr", "Condition", "Skill")

ioi_art <- ioi_art_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill)]

# add teaching info
ioi_art$Teaching <- rep(dt_included$TeachingPiano, each = 2)
ioi_art$TeachingYears <- rep(dt_included$TeachingPianoYears, each = 2)
ioi_art


## ----ioi-art-all, echo = FALSE------------------------------------------------
# Group mean
ioi_art_all <- ioi_art[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N), Median = median(Mean), IQR = IQR(Mean)), by = .(Condition, Skill, Teaching)]
ioi_art_all


## ----ioi-art-all-box, echo = FALSE--------------------------------------------
ioi_art_plot <- data.table(subject = ioi_art[Condition == "teaching"]$SubNr, teaching = ioi_art[Condition == "teaching"]$Mean, performing = ioi_art[Condition == "performing"]$Mean, teachingexp = dt_included$TeachingPiano, teachingyears = dt_included$TeachingPianoYears)

ggpaired(ioi_art_plot, cond1 = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "IOIs (ms)", title = "IOI: Articulation") + geom_hline(yintercept = 188, linetype = "dashed")


## ----ioi-dyn, echo = FALSE----------------------------------------------------
# For each individual
ioi_dyn_trial <- dt_ioi_dyn[, .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)]
setorder(ioi_dyn_trial, "SubNr", "Condition", "Skill")

ioi_dyn <- ioi_dyn_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill)]

# add teaching info
ioi_dyn$Teaching <- rep(dt_included$TeachingPiano, each = 2)
ioi_dyn$TeachingYears <- rep(dt_included$TeachingPianoYears, each = 2)
ioi_dyn


## ----ioi-dyn-all, echo = FALSE------------------------------------------------
# Group mean
ioi_dyn_all <- ioi_dyn[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N), Median = median(Mean), IQR = IQR(Mean)), by = .(Condition, Skill, Teaching)]
ioi_dyn_all


## ----ioi-dyn-all-box, echo = FALSE--------------------------------------------
ioi_dyn_plot <- data.table(subject = ioi_dyn[Condition == "teaching"]$SubNr, teaching = ioi_dyn[Condition == "teaching"]$Mean, performing = ioi_dyn[Condition == "performing"]$Mean, teachingexp = dt_included$TeachingPiano, teachingyears = dt_included$TeachingPianoYears)

ggpaired(ioi_dyn_plot, cond1 = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "IOIs (ms)", title = "IOI: Dynamics") + geom_hline(yintercept = 188, linetype = "dashed")


## ----export, include = FALSE--------------------------------------------------
knitr::purl("teaching_ioi.Rmd")

