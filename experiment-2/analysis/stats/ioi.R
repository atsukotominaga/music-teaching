## ----setup, include = FALSE------------------------------------------------------
# packages
# data manipulation
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
# plot
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}
# statistics
if (!require("afex")) {install.packages("afex"); require("afex")}


## ----file, include = FALSE-------------------------------------------------------
filename_ioi = "../preprocessor/trimmed/data_analysis_ioi.txt"


## ----extract, include = FALSE----------------------------------------------------
dt_ioi <- fread(filename_ioi, header = T, sep = ",", dec = ".") # read a trimmed csv

# SubNr as a factor
dt_ioi$SubNr <- as.factor(dt_ioi$SubNr)

# Include only articulation
dt_ioi_art <- dt_ioi[Skill == "articulation"]
# Include only dynamics
dt_ioi_dyn <- dt_ioi[Skill == "dynamics"]


## ----ioi-art, echo = FALSE-------------------------------------------------------
# For each individual
ioi_art <- dt_ioi_art[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill)]
ioi_art


## ----ioi-art-box,  echo = FALSE--------------------------------------------------
ggboxplot(dt_ioi_art[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)], "SubNr", "Mean", color = "Condition", palette = "aaas", xlab = "SubNr", ylab = "Normalised IOIs", title = "IOI: Articulation") + 
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  geom_hline(yintercept = 1, linetype = "dashed")


## ----ioi-art-all, echo = FALSE---------------------------------------------------
# Group mean
ioi_art_all <- ioi_art[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill)]
ioi_art_all


## ----ioi-art-all-box, echo = FALSE-----------------------------------------------
ioi_art_plot <- data.table(subject = ioi_art[Condition == "teaching"]$SubNr, teaching = ioi_art[Condition == "teaching"]$Mean, performing = ioi_art[Condition == "performing"]$Mean)

ggpaired(ioi_art_plot, cond1 = "performing", cond2 = "teaching", color = "condition", line.size = 0.3, line.color = "gray", palette = "aaas", xlab = "Condition", ylab = "Normalised IOIs", title = "IOI: Articulation") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_pubr(base_size = 20, legend = "none", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----normality1, echo = FALSE----------------------------------------------------
# Normality check
ioi_art_diff <- ioi_art[Condition == "teaching"]$Mean-ioi_art[Condition == "performing"]$Mean
ioi_art_norm <- shapiro.test(ioi_art_diff)
ioi_art_norm


## ----paired-art, echo = FALSE----------------------------------------------------
ioi_art_ttest <- t.test(ioi_art[Condition == "teaching"]$Mean, ioi_art[Condition == "performing"]$Mean, paired = TRUE, alternative = "two.sided")
ioi_art_ttest


## ----wilcoxon-art, echo = FALSE--------------------------------------------------
ioi_art_wilcoxon <- wilcox.test(Mean ~ Condition, data = ioi_art, paired = TRUE, alternative = "two.sided")
ioi_art_wilcoxon


## ----ioi-art-ch, echo = FALSE----------------------------------------------------
# Define phrase boundaries
dt_ioi_art$Boundary <- "No"
dt_ioi_art[Subcomponent == "LtoS" | Subcomponent == "StoL"]$Boundary <- "Yes"

# For each individual
ioi_art_ch <- dt_ioi_art[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill, Boundary)]
ioi_art_ch


## ----ioi-art-ch-box, echo = FALSE------------------------------------------------
ggboxplot(dt_ioi_art[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr, Boundary)], "SubNr", "Mean", color = "Condition", palette = "aaas", xlab = "SubNr", ylab = "Normalised IOIs", title = "IOI: Articulation") + 
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  facet_grid(Boundary ~ .) +
  geom_hline(yintercept = 1, linetype = "dashed")


## ----ioi-art-ch-all, echo = FALSE------------------------------------------------
# Group mean
ioi_art_ch_all <- ioi_art_ch[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(nrow(ioi_art_ch)/2)), by = .(Condition, Skill, Boundary)]
ioi_art_ch_all


## ----ioi-art-ch-all-box, echo = FALSE--------------------------------------------
ioi_art_ch_plot <- data.table(subject = ioi_art_ch[Condition == "teaching"]$SubNr, teaching = ioi_art_ch[Condition == "teaching"]$Mean, performing = ioi_art_ch[Condition == "performing"]$Mean, boundary = ioi_art_ch[Condition == "teaching"]$Boundary)

ggpaired(ioi_art_ch_plot, cond1 = "performing", cond2 = "teaching", color = "condition", facet.by = "boundary", line.size = 0.3, line.color = "gray", palette = "aaas", xlab = "Condition", ylab = "Normalised IOIs", title = "IOI: Articulation") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_pubr(base_size = 20, legend = "none", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----anova-art-boundary, echo = FALSE--------------------------------------------
ioi_art_ch_aov <- aov_car(Mean ~ Condition*Boundary + Error(SubNr/(Condition*Boundary)), data = ioi_art_ch)
summary(ioi_art_ch_aov)


## ----anova-art-boundary-2, echo = FALSE------------------------------------------
ioi_art_ch_aov_2 <- aov(Mean ~ Condition*Boundary + Error(SubNr/(Condition*Boundary)), data = ioi_art_ch)
summary(ioi_art_ch_aov_2)


## ----ioi-art-trial, echo = FALSE-------------------------------------------------
# For each individual
ioi_art_trial <- dt_ioi_art[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill, TrialNr)]
ioi_art_trial


## ----ioi-art-trial-line, echo = FALSE, fig.height = 4----------------------------
ggline(dt_ioi_art[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)], x = "TrialNr", y = "Mean", shape = "Condition", color = "Condition", palette = "aaas", xlab = "TrialNr", ylab = "Normalised IOIs", title = "IOI: Articulation") +
  facet_wrap(SubNr ~ .) +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1)) +
  geom_hline(yintercept = 1, linetype = "dashed")


## ----ioi-art-trial-all, echo = FALSE---------------------------------------------
# Group mean
ioi_art_trial_all <- ioi_art_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, TrialNr)]
ioi_art_trial_all


## ----ioi-art-trial-all-line, echo = FALSE----------------------------------------
ggline(ioi_art_trial, x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", palette = "aaas", xlab = "Trial", ylab = "Normalised IOIs", title = "IOI: Articulation") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_pubr(base_size = 20, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----ioi-dyn, echo = FALSE-------------------------------------------------------
# For each individual
ioi_dyn <- dt_ioi_dyn[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill)]
ioi_dyn


## ----ioi-dyn-box,  echo = FALSE--------------------------------------------------
ggboxplot(dt_ioi_dyn[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)], "SubNr", "Mean", color = "Condition", palette = "aaas", xlab = "SubNr", ylab = "Normalised IOIs", title = "IOI: Dynamics") + 
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  geom_hline(yintercept = 1, linetype = "dashed")


## ----ioi-dyn-all, echo = FALSE---------------------------------------------------
# Group mean
ioi_dyn_all <- ioi_dyn[, .(N = .N, Mean = mean(Mean), SD = sd(Mean),  SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill)]
ioi_dyn_all


## ----ioi-dyn-all-box, echo = FALSE-----------------------------------------------
ioi_dyn_plot <- data.table(subject = ioi_dyn[Condition == "teaching"]$SubNr, teaching = ioi_dyn[Condition == "teaching"]$Mean, performing = ioi_dyn[Condition == "performing"]$Mean)

ggpaired(ioi_dyn_plot, cond1 = "performing", cond2 = "teaching", color = "condition", line.size = 0.3, line.color = "gray", palette = "aaas", xlab = "Condition", ylab = "Normalised IOIs", title = "IOI: Dynamics") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_pubr(base_size = 20, legend = "none", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----normality2, echo = FALSE----------------------------------------------------
# Normality check
ioi_dyn_diff <- ioi_dyn[Condition == "teaching"]$Mean-ioi_dyn[Condition == "performing"]$Mean
ioi_dyn_norm <- shapiro.test(ioi_dyn_diff)
ioi_dyn_norm


## ----paired-dyn, echo = FALSE----------------------------------------------------
ioi_dyn_ttest <- t.test(ioi_dyn[Condition == "teaching"]$Mean, ioi_dyn[Condition == "performing"]$Mean, paired = TRUE, alternative = "two.sided")
ioi_dyn_ttest


## ----wilcoxon-dyn, echo = FALSE--------------------------------------------------
ioi_dyn_wilcoxon <- wilcox.test(Mean ~ Condition, data = ioi_dyn, paired = TRUE, alternative = "two.sided")
ioi_dyn_wilcoxon


## ----ioi-dyn-ch, echo = FALSE----------------------------------------------------
# Define phrase boundaries
dt_ioi_dyn$Boundary <- "No"
dt_ioi_dyn[Subcomponent == "FtoP" | Subcomponent == "PtoF"]$Boundary <- "Yes"

# For each individual
ioi_dyn_ch <- dt_ioi_dyn[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill, Boundary)]
ioi_dyn_ch


## ----ioi-dyn-ch-box, echo = FALSE------------------------------------------------
ggboxplot(dt_ioi_dyn[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr, Boundary)], "SubNr", "Mean", color = "Condition", palette = "aaas", xlab = "SubNr", ylab = "Normalised IOIs", title = "IOI: Dynamics") + 
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  facet_grid(Boundary ~ .) +
  geom_hline(yintercept = 1, linetype = "dashed")


## ----ioi-dyn-ch-all, echo = FALSE------------------------------------------------
# Group mean
ioi_dyn_ch_all <- ioi_dyn_ch[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Boundary)]
ioi_dyn_ch_all


## ----ioi-dyn-ch-all-box, echo = FALSE--------------------------------------------
ioi_dyn_ch_plot <- data.table(subject = ioi_dyn_ch[Condition == "teaching"]$SubNr, teaching = ioi_dyn_ch[Condition == "teaching"]$Mean, performing = ioi_dyn_ch[Condition == "performing"]$Mean, boundary = ioi_dyn_ch[Condition == "teaching"]$Boundary)

ggpaired(ioi_dyn_ch_plot, cond1 = "performing", cond2 = "teaching", color = "condition", facet.by = "boundary", line.size = 0.3, line.color = "gray", palette = "aaas", xlab = "Condition", ylab = "Normalised IOIs", title = "IOI: Dynamics") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_pubr(base_size = 20, legend = "none", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----anova-dyn-boundary, echo = FALSE--------------------------------------------
ioi_dyn_ch_aov <- aov_car(Mean ~ Condition*Boundary + Error(SubNr/(Condition*Boundary)), data = ioi_dyn_ch)
summary(ioi_dyn_ch_aov)


## ----anova-dyn-boundary2, echo = FALSE-------------------------------------------
ioi_dyn_ch_aov_2 <- aov(Mean ~ Condition*Boundary + Error(SubNr/(Condition*Boundary)), data = ioi_dyn_ch)
summary(ioi_dyn_ch_aov_2)


## ----ioi-dyn-trial, echo = FALSE-------------------------------------------------
# For each individual
ioi_dyn_trial <- dt_ioi_dyn[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill, TrialNr)]
ioi_dyn_trial


## ----ioi-dyn-trial-line, echo = FALSE, fig.height = 4----------------------------
ggline(dt_ioi_dyn[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)], x = "TrialNr", y = "Mean", shape = "Condition", color = "Condition", palette = "aaas", xlab = "TrialNr", ylab = "Normalised IOIs", title = "IOI: Dynamics") +
  facet_wrap(SubNr ~ .) +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1)) +
  geom_hline(yintercept = 1, linetype = "dashed")


## ----ioi-dyn-trial-all, echo = FALSE---------------------------------------------
# Group mean
ioi_dyn_trial_all <- ioi_dyn_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean),  SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, TrialNr)]
ioi_dyn_trial_all


## ----ioi-dyn-trial-all-line, echo = FALSE----------------------------------------
ggline(ioi_dyn_trial, x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", palette = "aaas", xlab = "Trial", ylab = "Normalised IOIs", title = "IOI: Dynamics") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_pubr(base_size = 20, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----seq-art, fig.width = 9, fig.height = 3, echo = FALSE------------------------
# For each individual
ioi_art_seq <- dt_ioi_art[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill, Interval)]

ggline(ioi_art_seq, x = "Interval", y = "Mean", add = "mean_se", position = position_dodge(.2), linetype = "Condition", shape = "Condition", color = "Condition", palette = "aaas", xlab = "Interval", ylab = "Normalised IOIs", title = "IOI: Articulation") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,71,1))


## ----seq-dyn, fig.width = 9, fig.height = 3, echo = FALSE------------------------
# For each individual
ioi_dyn_seq <- dt_ioi_dyn[, .(N = .N, Mean = mean(normIOI), SD = sd(normIOI)), by = .(SubNr, Condition, Skill, Interval)]

ggline(ioi_dyn_seq, x = "Interval", y = "Mean", add = "mean_se", position = position_dodge(.2), linetype = "Condition", shape = "Condition", color = "Condition", palette = "aaas", xlab = "Interval", ylab = "Normalised IOIs", title = "IOI: Dynamics") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
    scale_x_continuous(breaks = seq(1,71,1))


## ----export, include = FALSE-----------------------------------------------------
knitr::purl("ioi.Rmd")

