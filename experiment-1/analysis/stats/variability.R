## ----setup, include = FALSE---------------------------------------------------
# packages
# data manipulation
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
# plot
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}
# statistics
if (!require("afex")) {install.packages("afex"); require("afex")}
if (!require("rstatix")) {install.packages("rstatix"); require("rstatix")}

filename_ioi = "../preprocessor/trimmed/data_analysis_ioi.txt"
dt_ioi <- fread(filename_ioi, header = T, sep = ",", dec = ".")

# SubNr as a factor
dt_ioi$SubNr <- as.factor(dt_ioi$SubNr)

# Include only articulation
dt_ioi_art <- dt_ioi[Skill == "articulation"]
# per participant
ioi_art_seq_each <- dt_ioi_art[, .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = .(SubNr, Condition, Skill, Interval)]

# Include only dynamics
dt_ioi_dyn <- dt_ioi[Skill == "dynamics"]
# per participant
ioi_dyn_seq_each <- dt_ioi_dyn[, .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = .(SubNr, Condition, Skill, Interval)]


## ---- echo = FALSE, fig.width = 6, fig.height = 1.2---------------------------
# Plot the whole sequence
ggline(ioi_art_seq_each, x = "Interval", y = "Mean", add = "mean_se", shape = "Condition", color = "Condition", xlab = "Interval", ylab = "IOIs (ms)", title = "IOIs: Articulation") +
  geom_hline(yintercept = 188, linetype = "dotted") + # target tempo
  geom_vline(xintercept = c(8, 24, 49, 16, 41, 57), linetype = "twodash") + # transition points
  scale_x_continuous(breaks = seq(1,66,1))

ggline(ioi_dyn_seq_each, x = "Interval", y = "Mean", add = "mean_se", shape = "Condition", color = "Condition", xlab = "Interval", ylab = "IOIs (ms)", title = "IOIs: Dynamics") +
  geom_hline(yintercept = 188, linetype = "dotted") + # target tempo
  geom_vline(xintercept = c(8, 24, 49, 16, 41, 57), linetype = "twodash") + # transition points
  scale_x_continuous(breaks = seq(1,66,1))


## ---- echo = FALSE------------------------------------------------------------
ioi_cv_trial <- dt_ioi[, .(N = .N, Mean = mean(IOI), SD = sd(IOI), CV = sd(IOI)/mean(IOI)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)]
ioi_cv_subject <- ioi_cv_trial[, .(N = .N, MeanCV = mean(CV), SD = sd(CV)), by = .(SubNr, Condition, Skill)]
ioi_cv_subject <- ioi_cv_subject[order(SubNr, Condition, Skill)] # ordering

ggplot(ioi_cv_subject, aes(x = Condition, y = MeanCV, color = Condition)) +
  geom_boxplot() +
  geom_line(aes(group = SubNr), size = 0.4, color = "gray") +
  geom_point(aes(color = Condition)) +
  facet_grid(. ~ Skill) +
  theme_pubr()


## ---- echo = FALSE------------------------------------------------------------
# descriptive stats
ioi_cv_subject[, .(.N, MeanCV = mean(MeanCV), SDCV = sd(MeanCV)), by = .(Condition, Skill)]

aov_ez(id = "SubNr", dv = "MeanCV",
       within = c("Condition", "Skill"),
       data = ioi_cv_subject)


## ---- echo = FALSE------------------------------------------------------------
# descriptive stats
ioi_cv_subject[SubNr != 2, .(.N, MeanCV = mean(MeanCV), SDCV = sd(MeanCV)), by = .(Condition, Skill)]

aov_ez(id = "SubNr", dv = "MeanCV",
       within = c("Condition", "Skill"),
       data = ioi_cv_subject[SubNr != 2])


## ---- echo = FALSE------------------------------------------------------------
ioi_tra <- dt_ioi[Subcomponent == "LtoS" | Subcomponent == "StoL" | Subcomponent == "FtoP" | Subcomponent == "PtoF", .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = .(SubNr, Condition, Skill)]
ioi_tra <- ioi_tra[order(SubNr, Condition, Skill)] # ordering

ggplot(ioi_tra, aes(x = Condition, y = Mean, color = Condition)) +
  geom_boxplot() +
  geom_line(aes(group = SubNr), size = 0.4, color = "gray") +
  geom_point(aes(color = Condition)) +
  facet_grid(. ~ Skill) +
  theme_pubr()


## ---- echo = FALSE------------------------------------------------------------
# descriptive stats
ioi_tra[, .(.N, MeanCV = mean(Mean), SD = sd(Mean)), by = .(Condition, Skill)]

aov_ez(id = "SubNr", dv = "Mean",
       within = c("Condition", "Skill"),
       data = ioi_tra)


## ---- echo = FALSE------------------------------------------------------------
dt_ioi$Transition <- "No"
dt_ioi[Subcomponent == "LtoS" | Subcomponent == "StoL" | Subcomponent == "FtoP" | Subcomponent == "PtoF"]$Transition <- "Yes"

# transition points vs. others
ioi_tra_others <- dt_ioi[, .(.N, Mean = mean(IOI), SD = sd(IOI)), by = .(SubNr, Condition, Skill, Transition)]
ioi_tra_others <- ioi_tra_others[order(SubNr, Condition, Skill)] # ordering

# descriptive stats
ioi_tra_others[, .(.N, Mean = mean(Mean), SD = sd(Mean)), by = .(Condition, Skill, Transition)]

ggplot(ioi_tra_others, aes(x = Condition, y = Mean, color = Condition)) +
  geom_boxplot() +
  geom_line(aes(group = SubNr), size = 0.4, color = "gray") +
  geom_point(aes(color = Condition)) +
  facet_grid(Skill ~ Transition) +
  theme_pubr()


## ---- echo = FALSE------------------------------------------------------------
aov_ez(id = "SubNr", dv = "Mean",
       within = c("Condition", "Skill", "Transition"),
       data = ioi_tra_others)


## ----export, include = FALSE--------------------------------------------------
knitr::purl("variability.Rmd")

