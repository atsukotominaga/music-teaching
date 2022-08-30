## ----setup, include = FALSE-----------------------------------------------------------------------------------
# packages
if (!require("here")) {install.packages("here"): require("here")}
# data manipulation
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
# plot
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}
# # statistics
# if (!require("afex")) {install.packages("afex"); require("afex")}
# if (!require("emmeans")) {install.packages("emmeans"); require("emmeans")}

# read info about teaching experience
source(here("experiment-2/analysis/demographics", "questionnaire.R"), chdir = TRUE)


## ----file, include = FALSE------------------------------------------------------------------------------------
filename_kor = "../preprocessor/trimmed/data_analysis_kor.txt"
filename_kot = "../preprocessor/trimmed/data_analysis_kot.txt"


## ----extract, include = FALSE---------------------------------------------------------------------------------
dt_kor <- fread(filename_kor, header = T, sep = ",", dec = ".") # read a trimmed txt
dt_kot <- fread(filename_kot, header = T, sep = ",", dec = ".") # read a trimmed txt

# SubNr as a factor
dt_kor$SubNr <- as.factor(dt_kor$SubNr)
dt_kot$SubNr <- as.factor(dt_kot$SubNr)

# Include only articulation
dt_kor_art <- dt_kor[Skill == "articulation"]
dt_kot_art <- dt_kot[Skill == "articulation"]

# Include only dynamics
dt_kor_dyn <- dt_kor[Skill == "dynamics"]
dt_kot_dyn <- dt_kot[Skill == "dynamics"]


## ----kot-art, echo = FALSE------------------------------------------------------------------------------------
# For each individual
kot_art_trial <- dt_kot_art[Subcomponent == "Legato" | Subcomponent == "Staccato", .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr, Subcomponent)]

kot_art <- kot_art_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent)]
setorder(kot_art, "SubNr", "Condition", "Skill", "Subcomponent")

# add teaching info
kot_art$Teaching <- rep(dt_2_included$TeachingPiano, each = 4)
kot_art$TeachingYears <- rep(dt_2_included$TeachingPianoYears, each = 4)

kot_art


## ----kot-art-all, echo = FALSE--------------------------------------------------------------------------------
# Group mean
kot_art_all <- kot_art[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, Teaching)]
kot_art_all


## ----kot-art-all-box, echo = FALSE----------------------------------------------------------------------------
kot_art_plot <- data.table(subject = kot_art[Condition == "teaching"]$SubNr, teaching = kot_art[Condition == "teaching"]$Mean, performing = kot_art[Condition == "performing"]$Mean, subcomponent = kot_art[Condition == "teaching"]$Subcomponent, teachingexp = rep(dt_2_included$TeachingPiano, each = 2), teachingyears = rep(dt_2_included$TeachingPianoYears, each = 2))

ggpaired(kot_art_plot[subcomponent == "Legato"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KOT (ms)", title = "KOT: Articulation-Legato")

ggpaired(kot_art_plot[subcomponent == "Staccato"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KOT (ms)", title = "KOT: Articulation-Staccato")


## ----kor-art, echo = FALSE------------------------------------------------------------------------------------
# For each individual
kor_art_trial <- dt_kor_art[Subcomponent == "Legato" | Subcomponent == "Staccato", .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr, Subcomponent)]

kor_art <- kor_art_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent)]
setorder(kor_art, "SubNr", "Condition", "Skill", "Subcomponent")

# add teaching info
kor_art$Teaching <- rep(dt_2_included$TeachingPiano, each = 4)
kor_art$TeachingYears <- rep(dt_2_included$TeachingPianoYears, each = 4)

kor_art


## ----kor-art-all, echo = FALSE--------------------------------------------------------------------------------
# Group mean
kor_art_all <- kor_art[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, Teaching)]
kor_art_all


## ----kor-art-all-box, echo = FALSE----------------------------------------------------------------------------
kor_art_plot <- data.table(subject = kor_art[Condition == "teaching"]$SubNr, teaching = kor_art[Condition == "teaching"]$Mean, performing = kor_art[Condition == "performing"]$Mean, subcomponent = kor_art[Condition == "teaching"]$Subcomponent, teachingexp = rep(dt_2_included$TeachingPiano, each = 2), teachingyears = rep(dt_2_included$TeachingPianoYears, each = 2))

ggpaired(kor_art_plot[subcomponent == "Legato"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KOR", title = "KOR: Articulation-Legato")

ggpaired(kor_art_plot[subcomponent == "Staccato"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KOR", title = "KOR: Articulation-Staccato")


## ----kot-dyn, echo = FALSE------------------------------------------------------------------------------------
# For each individual
kot_dyn_trial <- dt_kot_dyn[Subcomponent == "Forte" | Subcomponent == "Piano", .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr, Subcomponent)]

kot_dyn <- kot_dyn_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent)]
setorder(kot_dyn, "SubNr", "Condition", "Skill", "Subcomponent")

# add teaching info
kot_dyn$Teaching <- rep(dt_2_included$TeachingPiano, each = 4)
kot_dyn$TeachingYears <- rep(dt_2_included$TeachingPianoYears, each = 4)

kot_dyn


## ----kot-dyn-all, echo = FALSE--------------------------------------------------------------------------------
# Group mean
kot_dyn_all <- kot_dyn[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, Teaching)]
kot_dyn_all


## ----kot-dyn-all-box, echo = FALSE----------------------------------------------------------------------------
kot_dyn_plot <- data.table(subject = kot_dyn[Condition == "teaching"]$SubNr, teaching = kot_dyn[Condition == "teaching"]$Mean, performing = kot_dyn[Condition == "performing"]$Mean, subcomponent = kot_dyn[Condition == "teaching"]$Subcomponent, teachingexp = rep(dt_2_included$TeachingPiano, each = 2), teachingyears = rep(dt_2_included$TeachingPianoYears, each = 2))

ggpaired(kot_dyn_plot[subcomponent == "Forte"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KOT (ms)", title = "KOT: Dynamics-Forte")

ggpaired(kot_dyn_plot[subcomponent == "Piano"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KOT (ms)", title = "KOT: Dynamics-Piano")


## ----kor-dyn, echo = FALSE------------------------------------------------------------------------------------
# For each individual
kor_dyn_trial <- dt_kor_dyn[Subcomponent == "Forte" | Subcomponent == "Piano", .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr, Subcomponent)]

kor_dyn <- kor_dyn_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent)]
setorder(kor_dyn, "SubNr", "Condition", "Skill", "Subcomponent")

# add teaching info
kor_dyn$Teaching <- rep(dt_2_included$TeachingPiano, each = 4)
kor_dyn$TeachingYears <- rep(dt_2_included$TeachingPianoYears, each = 4)

kor_dyn


## ----kor-dyn-all, echo = FALSE--------------------------------------------------------------------------------
# Group mean
kor_dyn_all <- kor_dyn[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, Teaching)]
kor_dyn_all


## ----kor-dyn-all-box, echo = FALSE----------------------------------------------------------------------------
kor_dyn_plot <- data.table(subject = kor_dyn[Condition == "teaching"]$SubNr, teaching = kor_dyn[Condition == "teaching"]$Mean, performing = kor_dyn[Condition == "performing"]$Mean, subcomponent = kor_dyn[Condition == "teaching"]$Subcomponent, teachingexp = rep(dt_2_included$TeachingPiano, each = 2), teachingyears = rep(dt_2_included$TeachingPianoYears, each = 2))

ggpaired(kor_dyn_plot[subcomponent == "Forte"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KOR", title = "KOR: Dynamics-Forte")

ggpaired(kor_dyn_plot[subcomponent == "Piano"], cond = "performing", cond2 = "teaching", color = "condition", facet.by = "teachingexp", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KOR", title = "KOR: Dynamics-Piano")


## ----export, include = FALSE----------------------------------------------------------------------------------
knitr::purl("teaching_art.Rmd")

