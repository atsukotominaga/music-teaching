## ----setup, include = FALSE---------------------------------------------------
# packages
if (!require("here")) {install.packages("here"): require("here")}
# data manipulation
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
# plot
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}
# # statistic
# if (!require("afex")) {install.packages("afex"); require("afex")}
# if (!require("emmeans")) {install.packages("emmeans"); require("emmeans")}

# read info about teaching experience
source(here("experiment-1/analysis/demographics", "questionnaire.R"), chdir = TRUE)


## ----file, include = FALSE----------------------------------------------------
filename_vel = "../preprocessor/trimmed/data_analysis_vel.txt"
filename_vel_diff = "../preprocessor/trimmed/data_analysis_vel_diff.txt"


## ----extract, include = FALSE-------------------------------------------------
dt_vel <- fread(filename_vel, header = T, sep = ",", dec = ".") # read a trimmed txt
dt_vel_diff <- fread(filename_vel_diff, header = T, sep = ",", dec = ".") # read a trimmed txt

# SubNr as a factor
dt_vel$SubNr <- as.factor(dt_vel$SubNr)
dt_vel_diff$SubNr <- as.factor(dt_vel_diff$SubNr)

# Include only articulation
dt_vel_art <- dt_vel[Skill == "articulation"]
dt_vel_diff_art <- dt_vel_diff[Skill == "articulation"]

# Include only dynamics
dt_vel_dyn <- dt_vel[Skill == "dynamics"]
dt_vel_diff_dyn <- dt_vel_diff[Skill == "dynamics"]


## ----vel-dyn, echo = FALSE----------------------------------------------------
# For each individual
vel_dyn_trial <- dt_vel_dyn[, .(N = .N, Mean = mean(Velocity), SD = sd(Velocity)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr, Subcomponent)]
setorder(vel_dyn_trial, "SubNr", "Condition", "Skill", "Subcomponent")

vel_dyn <- vel_dyn_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent)]

# add teaching info
vel_dyn$Teaching <- rep(dt_included$TeachingPiano, each = 4)
vel_dyn$TeachingYears <- rep(dt_included$TeachingPianoYears, each = 4)
vel_dyn


## ----vel-dyn-all, echo = FALSE------------------------------------------------
# Group mean
vel_dyn_all <- vel_dyn[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, Teaching)]
vel_dyn_all


## ----vel-dyn-all-box, echo = FALSE--------------------------------------------
vel_dyn_plot <- data.table(subject = vel_dyn[Condition == "teaching"]$SubNr, teaching = vel_dyn[Condition == "teaching"]$Mean, performing = vel_dyn[Condition == "performing"]$Mean, subcomponent = vel_dyn[Condition == "teaching"]$Subcomponent, teachingexp = rep(dt_included$TeachingPiano, each = 2), teachingyears = rep(dt_included$TeachingPianoYears, each = 2))

ggpaired(vel_dyn_plot[subcomponent == "Forte"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KV (0-127)", title = "KV: Dynamics-Forte")

ggpaired(vel_dyn_plot[subcomponent == "Piano"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KV (0-127)", title = "KV: Dynamics-Piano")


## ----vel-diff-dyn, echo = FALSE-----------------------------------------------
# For each individual
vel_diff_dyn_trial <- dt_vel_diff_dyn[Subcomponent == "FtoP" | Subcomponent == "PtoF", .(N = .N, Mean = mean(Diff), SD = sd(Diff)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr, Subcomponent)]

vel_diff_dyn <- vel_diff_dyn_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent)]
setorder(vel_diff_dyn_trial, "SubNr", "Condition", "Skill", "Subcomponent")

vel_diff_dyn <- vel_diff_dyn_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent)]

# add teaching info
vel_diff_dyn$Teaching <- rep(dt_included$TeachingPiano, each = 4)
vel_diff_dyn$TeachingYears <- rep(dt_included$TeachingPianoYears, each = 4)
vel_diff_dyn


## ----vel-diff-dyn-all, echo = FALSE-------------------------------------------
# Group mean
vel_diff_dyn_all <- vel_diff_dyn[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, Teaching)]
vel_diff_dyn_all


## ----vel-diff-dyn-all-box, echo = FALSE---------------------------------------
vel_diff_dyn_plot <- data.table(subject = vel_diff_dyn[Condition == "teaching"]$SubNr, teaching = vel_diff_dyn[Condition == "teaching"]$Mean, performing = vel_diff_dyn[Condition == "performing"]$Mean, subcomponent = vel_diff_dyn[Condition == "teaching"]$Subcomponent, teachingexp = rep(dt_included$TeachingPiano, each = 2), teachingyears = rep(dt_included$TeachingPianoYears, each = 2))

ggpaired(vel_diff_dyn_plot[subcomponent == "FtoP"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "Difference", title = "KV-Diff: Dynamics-FtoP")

ggpaired(vel_diff_dyn_plot[subcomponent == "PtoF"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "Difference", title = "KV-Diff: Dynamics-PtoF")


## ----vel-art, echo = FALSE----------------------------------------------------
# For each individual
vel_art_trial <- dt_vel_art[, .(N = .N, Mean = mean(Velocity), SD = sd(Velocity)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr, Subcomponent)]

vel_art <- vel_art_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent)]
setorder(vel_art, "SubNr", "Condition", "Skill", "Subcomponent")

# add teaching info
vel_art$Teaching <- rep(dt_included$TeachingPiano, each = 4)
vel_art$TeachingYears <- rep(dt_included$TeachingPianoYears, each = 4)

vel_art


## ----vel-art-all, echo = FALSE------------------------------------------------
# Group mean
vel_art_all <- vel_art[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, Teaching)]
vel_art_all


## ----vel-art-all-box, echo = FALSE--------------------------------------------
vel_art_plot <- data.table(subject = vel_art[Condition == "teaching"]$SubNr, teaching = vel_art[Condition == "teaching"]$Mean, performing = vel_art[Condition == "performing"]$Mean, subcomponent = vel_art[Condition == "teaching"]$Subcomponent, teachingexp = rep(dt_included$TeachingPiano, each = 2), teachingyears = rep(dt_included$TeachingPianoYears, each = 2))

ggpaired(vel_art_plot[subcomponent == "Legato"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KV (0-127)", title = "KV: Articulation-Legato")

ggpaired(vel_art_plot[subcomponent == "Staccato"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KV (0-127)", title = "KV: Articulation-Staccato")


## ----vel-diff-art, echo = FALSE-----------------------------------------------
# For each individual
vel_diff_art_trial <- dt_vel_diff_art[Subcomponent == "LtoS" | Subcomponent == "StoL", .(N = .N, Mean = mean(Diff), SD = sd(Diff)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr, Subcomponent)]
setorder(vel_diff_art_trial, "SubNr", "Condition", "Skill", "Subcomponent")

vel_diff_art <- vel_diff_art_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent)]

# add teaching info
vel_diff_art$Teaching <- rep(dt_included$TeachingPiano, each = 4)
vel_diff_art$TeachingYears <- rep(dt_included$TeachingPianoYears, each = 4)
vel_diff_art


## ----vel-diff-art-all, echo = FALSE-------------------------------------------
# Group mean
vel_diff_art_all <- vel_diff_art[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, Teaching)]
vel_diff_art_all


## ----vel-diff-art-all-box, echo = FALSE---------------------------------------
vel_diff_art_plot <- data.table(subject = vel_diff_art[Condition == "teaching"]$SubNr, teaching = vel_diff_art[Condition == "teaching"]$Mean, performing = vel_diff_art[Condition == "performing"]$Mean, subcomponent = vel_diff_art[Condition == "teaching"]$Subcomponent, teachingexp = rep(dt_included$TeachingPiano, each = 2), teachingyears = rep(dt_included$TeachingPianoYears, each = 2))

ggpaired(vel_diff_art_plot[subcomponent == "LtoS"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "Difference", title = "KV-Diff: Articulation-LtoS")

ggpaired(vel_diff_art_plot[subcomponent == "StoL"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "Difference", title = "KV-Diff: Articulation-StoL")


## ----export, include = FALSE--------------------------------------------------
knitr::purl("teaching_dyn.Rmd")

