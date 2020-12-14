## ----setup, include = FALSE---------------------------------------------------
# packages
# data manipulation
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
# plot
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}
# statistics
if (!require("afex")) {install.packages("afex"); require("afex")}
if (!require("emmeans")) {install.packages("emmeans"); require("emmeans")}


## ----file, include = FALSE----------------------------------------------------
filename_kor = "../preprocessor/trimmed/data_analysis_kor.txt"
filename_kot = "../preprocessor/trimmed/data_analysis_kot.txt"


## ----extract, include = FALSE-------------------------------------------------
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


## ----kor-art, echo = FALSE----------------------------------------------------
# For each individual
kor_art <- dt_kor_art[Subcomponent == "Legato" | Subcomponent == "Staccato", .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, Subcomponent)]
kor_art


## ----kor-art-box,  echo = FALSE-----------------------------------------------
ggboxplot(dt_kor_art[Subcomponent == "Legato" | Subcomponent == "Staccato", .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, Subcomponent, BlockNr, TrialNr)], "SubNr", "Mean", color = "Condition", palette = "aaas", xlab = "SubNr", ylab = "KOR", title = "KOR: Articulation") +
  facet_grid(Subcomponent ~ .) +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----kor-art-all, echo = FALSE------------------------------------------------
# Group mean
kor_art_all <- kor_art[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent)]
kor_art_all


## ----kor-art-all-scatter, echo = FALSE----------------------------------------
ggerrorplot(kor_art, x = "Condition", y = "Mean", add = "jitter", add.params = list(color = "darkgray"), error.plot = "errorbar", color = "Condition", facet.by = "Subcomponent", palette = "aaas", xlab = "Condition", ylab = "KOR", title = "KOR: Articulation") +
  theme_pubr(base_size = 20, legend = "none", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----kor-art-all-box, echo = FALSE--------------------------------------------
kor_art_plot <- data.table(subject = kor_art[Condition == "teaching"]$SubNr, teaching = kor_art[Condition == "teaching"]$Mean, performing = kor_art[Condition == "performing"]$Mean, subcomponent = kor_art[Condition == "teaching"]$Subcomponent)

ggpaired(kor_art_plot, cond = "performing", cond2 = "teaching", color = "condition", facet.by = "subcomponent", line.size = 0.3, line.color = "gray", palette = "aaas", xlab = "Condition", ylab = "KOR", title = "KOR: Articulation") +
  theme_pubr(base_size = 20, legend = "none", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----anova-art-kor-1, echo = FALSE--------------------------------------------
kor_art_aov <- aov_car(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = kor_art)
summary(kor_art_aov)


## ----anova-art-kor-2, echo = FALSE--------------------------------------------
kor_art_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = kor_art)
summary(kor_art_aov_2)    


## ----post-hoc-2, echo = FALSE-------------------------------------------------
test(pairs(emmeans(kor_art_aov,~Condition|Subcomponent)), joint = TRUE)
pairs(emmeans(kor_art_aov,~Condition|Subcomponent), adjust = "tukey")


## ----kor-art-trial, echo = FALSE----------------------------------------------
# For each individual
kor_art_trial <- dt_kor_art[Subcomponent == "Legato" | Subcomponent == "Staccato", .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, Subcomponent, TrialNr)]
kor_art_trial


## ----kor-art-trial-line-legato, echo = FALSE, fig.height = 4------------------
ggline(dt_kor_art[Subcomponent == "Legato", .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", palette = "aaas", xlab = "TrialNr", ylab = "KOR", title = "KOR: Articulation/Legato") +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----kor-art-trial-line-staccato, echo = FALSE, fig.height = 4----------------
ggline(dt_kor_art[Subcomponent == "Staccato", .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", palette = "aaas", xlab = "TrialNr", ylab = "KOR", title = "KOR: Articulation/Staccato") +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----kor-art-trial-all, echo = FALSE------------------------------------------
# Group mean
kor_art_trial_all <- kor_art_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM =  sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, TrialNr)]
kor_art_trial_all


## ----kor-art-trial-all-line-legato, echo = FALSE------------------------------
ggline(kor_art_trial[Subcomponent == "Legato"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", palette = "aaas", xlab = "Trial", ylab = "KOR", title = "KOR: Articulation/Legato") +
  theme_pubr(base_size = 20, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----kor-art-trial-all-line-staccato, echo = FALSE----------------------------
ggline(kor_art_trial[Subcomponent == "Staccato"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", palette = "aaas", xlab = "Trial", ylab = "KOR", title = "KOR: Articulation/Legato") +
  theme_pubr(base_size = 20, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----kot-art, echo = FALSE----------------------------------------------------
# For each individual
kot_art <- dt_kot_art[Subcomponent == "Legato" | Subcomponent == "Staccato", .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, Subcomponent)]
kot_art


## ----kot-art-box,  echo = FALSE-----------------------------------------------
ggboxplot(dt_kot_art[Subcomponent == "Legato" | Subcomponent == "Staccato", .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, Subcomponent, BlockNr, TrialNr)], "SubNr", "Mean", color = "Condition", palette = "aaas", xlab = "SubNr", ylab = "KOT (ms)", title = "KOT: Articulation") +
  facet_grid(Subcomponent ~ .) +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----kot-art-all, echo = FALSE------------------------------------------------
# Group mean
kot_art_all <- kot_art[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent)]
kot_art_all


## ----kot-art-all-scatter, echo = FALSE----------------------------------------
ggerrorplot(kot_art, x = "Condition", y = "Mean", add = "jitter", add.params = list(color = "darkgray"), error.plot = "errorbar", color = "Condition", facet.by = "Subcomponent", palette = "aaas", xlab = "Condition", ylab = "KOT (ms)", title = "KOT: Articulation") +
  theme_pubr(base_size = 20, legend = "none", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----kot-art-all-box, echo = FALSE--------------------------------------------
kot_art_plot <- data.table(subject = kot_art[Condition == "teaching"]$SubNr, teaching = kot_art[Condition == "teaching"]$Mean, performing = kot_art[Condition == "performing"]$Mean, subcomponent = kot_art[Condition == "teaching"]$Subcomponent)

ggpaired(kot_art_plot, cond = "performing", cond2 = "teaching", color = "condition", facet.by = "subcomponent", line.size = 0.3, line.color = "gray", palette = "aaas", xlab = "Condition", ylab = "KOT (ms)", title = "KOT: Articulation") +
  theme_pubr(base_size = 20, legend = "none", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----anova-art-kot-1, echo = FALSE--------------------------------------------
kot_art_aov <- aov_car(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = kot_art)
summary(kot_art_aov)


## ----anova-art-kot-2, echo = FALSE--------------------------------------------
kot_art_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = kot_art)
summary(kot_art_aov_2)


## ----post-hoc-1, echo = FALSE-------------------------------------------------
test(pairs(emmeans(kot_art_aov,~Condition|Subcomponent)), joint = TRUE)
pairs(emmeans(kot_art_aov,~Condition|Subcomponent), adjust = "tukey")


## ----kot-art-trial, echo = FALSE----------------------------------------------
# For each individual
kot_art_trial <- dt_kot_art[Subcomponent == "Legato" | Subcomponent == "Staccato", .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, Subcomponent, TrialNr)]
kot_art_trial


## ----kot-art-trial-line-legato, echo = FALSE, fig.height = 4------------------
ggline(dt_kot_art[Subcomponent == "Legato", .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", palette = "aaas", xlab = "TrialNr", ylab = "KOT (ms)", title = "KOT: Articulation/Legato") +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----kot-art-trial-line-staccato, echo = FALSE, fig.height = 4----------------
ggline(dt_kot_art[Subcomponent == "Staccato", .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", palette = "aaas", xlab = "TrialNr", ylab = "KOT (ms)", title = "KOT: Articulation/Staccato") +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----kot-art-trial-all, echo = FALSE------------------------------------------
# Group mean
kot_art_trial_all <- kot_art_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, TrialNr)]
kot_art_trial_all


## ----kot-art-trial-all-line-legato, echo = FALSE------------------------------
ggline(kot_art_trial[Subcomponent == "Legato"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", palette = "aaas", xlab = "Trial", ylab = "KOT (ms)", title = "KOT: Articulation/Legato") +
  scale_x_continuous(breaks = seq(1,8,1)) +
  theme_pubr(base_size = 20, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----kot-art-trial-all-line-staccato, echo = FALSE----------------------------
ggline(kot_art_trial[Subcomponent == "Staccato"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", palette = "aaas", xlab = "Trial", ylab = "KOT (ms)", title = "KOT: Articulation/Staccato") +
  scale_x_continuous(breaks = seq(1,8,1)) +
  theme_pubr(base_size = 20, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----kor-dyn, echo = FALSE----------------------------------------------------
# For each individual
kor_dyn <- dt_kor_dyn[Subcomponent == "Forte" | Subcomponent == "Piano", .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, Subcomponent)]
kor_dyn


## ----kor-dyn-box,  echo = FALSE-----------------------------------------------
ggboxplot(dt_kor_dyn[Subcomponent == "Forte" | Subcomponent == "Piano", .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, Subcomponent, BlockNr, TrialNr)], "SubNr", "Mean", color = "Condition", palette = "aaas", xlab = "SubNr", ylab = "KOR", title = "KOR: Dynamics") +
  facet_grid(Subcomponent ~ .) +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----kor-dyn-all, echo = FALSE------------------------------------------------
# Group mean
kor_dyn_all <- kor_dyn[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent)]
kor_dyn_all


## ----kor-dyn-all-scatter, echo = FALSE----------------------------------------
ggerrorplot(kor_dyn, x = "Condition", y = "Mean", add = "jitter", add.params = list(color = "darkgray"), error.plot = "errorbar", color = "Condition", facet.by = "Subcomponent", palette = "aaas", xlab = "Condition", ylab = "KOR", title = "KOR: Dynamics") +
  theme_pubr(base_size = 20, legend = "none", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----kor-dyn-all-box, echo = FALSE--------------------------------------------
kor_dyn_plot <- data.table(subject = kor_dyn[Condition == "teaching"]$SubNr, teaching = kor_dyn[Condition == "teaching"]$Mean, performing = kor_dyn[Condition == "performing"]$Mean, subcomponent = kor_dyn[Condition == "teaching"]$Subcomponent)

ggpaired(kor_dyn_plot, cond = "performing", cond2 = "teaching", color = "condition", facet.by = "subcomponent", line.size = 0.3, line.color = "gray", palette = "aaas", xlab = "Condition", ylab = "KOR", title = "KOR: Dynamics") +
  theme_pubr(base_size = 20, legend = "none", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----anova-dyn-kor-1, echo = FALSE--------------------------------------------
kor_dyn_aov <- aov_car(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = kor_dyn)
summary(kor_dyn_aov)


## ----anova-dyn-kor-2, echo = FALSE--------------------------------------------
kor_dyn_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = kor_dyn)
summary(kor_dyn_aov_2)


## ----kor-dyn-trial, echo = FALSE----------------------------------------------
# For each individual
kor_dyn_trial <- dt_kor_dyn[Subcomponent == "Forte" | Subcomponent == "Piano", .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, Subcomponent, TrialNr)]
kor_dyn_trial


## ----kor-dyn-trial-line-forte, echo = FALSE, fig.height = 4-------------------
ggline(dt_kor_dyn[Subcomponent == "Forte", .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", palette = "aaas", xlab = "TrialNr", ylab = "KOR", title = "KOR: Dynamics/Forte") +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----kor-dyn-trial-line-piano, echo = FALSE, fig.height = 4-------------------
ggline(dt_kor_dyn[Subcomponent == "Piano", .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", palette = "aaas", xlab = "TrialNr", ylab = "KOR", title = "KOR: Dynamics/Piano") +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----kor-dyn-trial-all, echo = FALSE------------------------------------------
# Group mean
kor_dyn_trial_all <- kor_dyn_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent,TrialNr)]
kor_dyn_trial_all


## ----kor-dyn-trial-all-line-forte, echo = FALSE-------------------------------
ggline(kor_dyn_trial[Subcomponent == "Forte"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", palette = "aaas", xlab = "Trial", ylab = "KOR", title = "KOR: Dynamics/Forte") +
  theme_pubr(base_size = 20, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----kor_dyn_trial_all_line_piano, echo = FALSE-------------------------------
ggline(kor_dyn_trial[Subcomponent == "Piano"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", palette = "aaas", xlab = "Trial", ylab = "KOR", title = "KOR: Dynamics/Piano") +
  theme_pubr(base_size = 20, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----kot-dyn, echo = FALSE----------------------------------------------------
# For each individual
kot_dyn <- dt_kot_dyn[Subcomponent == "Forte" | Subcomponent == "Piano", .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, Subcomponent)]
kot_dyn


## ----kot-dyn-box,  echo = FALSE-----------------------------------------------
ggboxplot(dt_kot_dyn[Subcomponent == "Forte" | Subcomponent == "Piano", .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, Subcomponent, BlockNr, TrialNr)], "SubNr", "Mean", color = "Condition", palette = "aaas", xlab = "SubNr", ylab = "KOT (ms)", title = "KOT: Dynamics") +
  facet_grid(Subcomponent ~ .) +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----kot-dyn-all, echo = FALSE------------------------------------------------
# Group mean
kot_dyn_all <- kot_dyn[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent)]
kot_dyn_all


## ----kot-dyn-all-scatter, echo = FALSE----------------------------------------
ggerrorplot(kot_dyn, x = "Condition", y = "Mean", add = "jitter", add.params = list(color = "darkgray"), error.plot = "errorbar", color = "Condition", facet.by = "Subcomponent", palette = "aaas", xlab = "Condition", ylab = "KOT (ms)", title = "KOT: Dynamics") +
  theme_pubr(base_size = 20, legend = "none", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----kot-dyn-all-box, echo = FALSE--------------------------------------------
kot_dyn_plot <- data.table(subject = kot_dyn[Condition == "teaching"]$SubNr, teaching = kot_dyn[Condition == "teaching"]$Mean, performing = kot_dyn[Condition == "performing"]$Mean, subcomponent = kot_dyn[Condition == "teaching"]$Subcomponent)

ggpaired(kot_dyn_plot, cond = "performing", cond2 = "teaching", color = "condition", facet.by = "subcomponent", line.size = 0.3, line.color = "gray", palette = "aaas", xlab = "Condition", ylab = "KOT (ms)", title = "KOT: Dynamics") +
  theme_pubr(base_size = 20, legend = "none", base_family = "Helvetica Neue LT Std 57 Condensed")


## ----anova-dyn-kot-1, echo = FALSE--------------------------------------------
kot_dyn_aov <- aov_car(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = kot_dyn)
summary(kot_dyn_aov)


## ----anova-dyn-kot-2, echo = FALSE--------------------------------------------
kot_dyn_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = kot_dyn)
summary(kot_dyn_aov_2)


## ----kot-dyn-trial, echo = FALSE----------------------------------------------
# For each individual
kot_dyn_trial <- dt_kot_dyn[, .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, Subcomponent, TrialNr)]
kot_dyn_trial


## ----kot-dyn-trial-line-forte, echo = FALSE, fig.height = 4-------------------
ggline(dt_kot_dyn[Subcomponent == "Forte", .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", palette = "aaas", xlab = "TrialNr", ylab = "KOT (ms)", title = "KOT: Dynamics/Forte") +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----kot-dyn-trial-line-piano, echo = FALSE, fig.height = 4-------------------
ggline(dt_kot_dyn[Subcomponent == "Piano", .(N = .N, Mean = mean(KOT), SD = sd(KOT)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr)], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", palette = "aaas", xlab = "TrialNr", ylab = "KOT (ms)", title = "KOT: Dynamics/Piano") +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----kot-dyn-trial-all, echo = FALSE------------------------------------------
# Group mean
kot_dyn_trial_all <- kot_dyn_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM =  sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, TrialNr)]
kot_dyn_trial_all


## ----kot-dyn-trial-all-line-forte, echo = FALSE-------------------------------
ggline(kot_dyn_trial[Subcomponent == "Forte"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", palette = "aaas", xlab = "Trial", ylab = "KOT (ms)", title = "KOT: Dynamics/Forte") +
  theme_pubr(base_size = 20, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----kot-dyn-trial-all-line-piano, echo = FALSE-------------------------------
ggline(kot_dyn_trial[Subcomponent == "Piano"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", palette = "aaas", xlab = "Trial", ylab = "KOT (ms)", title = "KOT: Dynamics/Piano") +
  theme_pubr(base_size = 20, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----seq-art, fig.width = 9, fig.height = 3, echo = FALSE---------------------
# For each individual
kor_art_seq <- dt_kor_art[, .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, Interval)]

ggline(kor_art_seq, x = "Interval", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", palette = "aaas", xlab = "Interval", ylab = "KOR", title = "KOT: Articulation") +
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,71,1))


## ----seq-dyn, fig.width = 9, fig.height = 3, echo = FALSE---------------------
# For each individual
kor_dyn_seq <- dt_kor_dyn[, .(N = .N, Mean = mean(KOR), SD = sd(KOR)), by = .(SubNr, Condition, Skill, Interval)]

ggline(kor_dyn_seq, x = "Interval", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", palette = "aaas", xlab = "Interval", ylab = "KOR", title = "KOT: Dynamics")+
  theme_pubr(base_size = 12, legend = "bottom", base_family = "Helvetica Neue LT Std 57 Condensed") +
  scale_x_continuous(breaks = seq(1,71,1)) 


## ----export, include = FALSE--------------------------------------------------
knitr::purl("articulation.Rmd")

