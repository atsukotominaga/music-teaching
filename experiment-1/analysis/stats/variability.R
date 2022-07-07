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
ioi_cv <- ioi_cv_subject[, .(N = .N, MeanCV = mean(MeanCV), MedianCV = median(MeanCV), SD = sd(MeanCV), IQR = IQR(MeanCV)), by = .(Condition, Skill)]
ioi_cv

ggplot(ioi_cv_subject, aes(x = Condition, y = MeanCV, color = Condition)) +
  geom_boxplot() +
  geom_line(aes(group = SubNr), size = 0.4, color = "gray") +
  geom_point(aes(color = Condition)) +
  labs(y = "CVs") +
  facet_grid(. ~ Skill) +
  theme_pubr()


## ---- echo = FALSE------------------------------------------------------------
# normality check
diff <- ioi_cv_subject[Condition == "teaching" & Skill == "articulation"]$MeanCV-ioi_cv_subject[Condition == "performing" & Skill == "articulation"]$MeanCV
ioi_art_cv_norm <- shapiro.test(diff)
ioi_art_cv_norm

# wilcoxon test
ioi_art_cv_wilcoxon <- wilcox.test(MeanCV ~ Condition, data = ioi_cv_subject[Skill == "articulation"], paired = TRUE, alternative = "two.sided")
ioi_art_cv_wilcoxon

# effect size
wilcoxeff_art_cv <- ioi_cv_subject[Skill == "articulation"] %>% wilcox_effsize(MeanCV ~ Condition, paired = TRUE)
wilcoxeff_art_cv


## ---- echo = FALSE------------------------------------------------------------
# normality check
diff2 <- ioi_cv_subject[Condition == "teaching" & Skill == "dynamics"]$MeanCV-ioi_cv_subject[Condition == "performing" & Skill == "dynamics"]$MeanCV
ioi_dyn_cv_norm <- shapiro.test(diff2)
ioi_dyn_cv_norm

# wilcoxon test
ioi_dyn_cv_wilcoxon <- wilcox.test(MeanCV ~ Condition, data = ioi_cv_subject[Skill == "dynamics"], paired = TRUE, alternative = "two.sided")
ioi_dyn_cv_wilcoxon

# effect size
wilcoxeff_dyn_cv <- ioi_cv_subject[Skill == "dynamics"] %>% wilcox_effsize(MeanCV ~ Condition, paired = TRUE)
wilcoxeff_dyn_cv


## ---- echo = FALSE------------------------------------------------------------
ioi_tra_trial <- dt_ioi[Subcomponent == "LtoS" | Subcomponent == "StoL" | Subcomponent == "FtoP" | Subcomponent == "PtoF", .(N = .N, Mean = mean(IOI), SD = sd(IOI)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)]
ioi_tra_subject <- ioi_tra_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill)]
ioi_tra_subject <- ioi_tra_subject[order(SubNr, Condition, Skill)] # ordering
ioi_tra <- ioi_tra_subject[, .(N = .N, Mean = mean(Mean), Median = median(Mean), SD = sd(Mean), IQR = IQR(Mean)), by = .(Condition, Skill)]
ioi_tra

ggplot(ioi_tra_subject, aes(x = Condition, y = Mean, color = Condition)) +
  geom_boxplot() +
  geom_line(aes(group = SubNr), size = 0.4, color = "gray") +
  geom_point(aes(color = Condition)) +
  labs(y = "IOIs (ms)") +
  facet_grid(. ~ Skill) +
  theme_pubr()


## ---- echo = FALSE------------------------------------------------------------
# normality check
diff3 <- ioi_tra_subject[Condition == "teaching" & Skill == "articulation"]$Mean-ioi_tra_subject[Condition == "performing" & Skill == "articulation"]$Mean
ioi_art_tra_norm <- shapiro.test(diff3)
ioi_art_tra_norm

# wilcoxon test
ioi_art_tra_wilcoxon <- wilcox.test(Mean ~ Condition, data = ioi_tra_subject[Skill == "articulation"], paired = TRUE, alternative = "two.sided")
ioi_art_tra_wilcoxon

# effect size
wilcoxeff_art_tra <- ioi_tra_subject[Skill == "articulation"] %>% wilcox_effsize(Mean ~ Condition, paired = TRUE)
wilcoxeff_art_tra


## ---- echo = FALSE------------------------------------------------------------
# normality check
diff4 <- ioi_tra_subject[Condition == "teaching" & Skill == "dynamics"]$Mean-ioi_tra_subject[Condition == "performing" & Skill == "dynamics"]$Mean
ioi_dyn_tra_norm <- shapiro.test(diff4)
ioi_dyn_tra_norm

# t test
ioi_dyn_tra_t_test <- t.test(Mean ~ Condition, data = ioi_tra_subject[Skill == "dynamics"], paired = TRUE, alternative = "two.sided")
ioi_dyn_tra_t_test

# wilcoxon test
ioi_dyn_tra_wilcoxon <- wilcox.test(Mean ~ Condition, data = ioi_tra_subject[Skill == "dynamics"], paired = TRUE, alternative = "two.sided")
ioi_dyn_tra_wilcoxon

# effect size
wilcoxeff_dyn_tra <- ioi_tra_subject[Skill == "dynamics"] %>% wilcox_effsize(Mean ~ Condition, paired = TRUE)
wilcoxeff_dyn_tra


## ----export, include = FALSE--------------------------------------------------
knitr::purl("variability.Rmd")

