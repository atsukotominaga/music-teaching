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

vel_dyn <- vel_dyn_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent)]
vel_dyn


## ----vel-dyn-box,  echo = FALSE-----------------------------------------------
ggboxplot(vel_dyn_trial, "SubNr", "Mean", color = "Condition", xlab = "SubNr", ylab = "KV (0-127)", title = "KV: Dynamics") + facet_grid(Subcomponent ~ .)


## ----vel-dyn-all, echo = FALSE------------------------------------------------
# Group mean
vel_dyn_all <- vel_dyn[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent)]
vel_dyn_all


## ----vel-dyn-all-box, echo = FALSE--------------------------------------------
vel_dyn_plot <- data.table(subject = vel_dyn[Condition == "teaching"]$SubNr, teaching = vel_dyn[Condition == "teaching"]$Mean, performing = vel_dyn[Condition == "performing"]$Mean, subcomponent = vel_dyn[Condition == "teaching"]$Subcomponent)

ggpaired(vel_dyn_plot, cond = "performing", cond2 = "teaching", color = "condition", facet.by = "subcomponent", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KV (0-127)", title = "KV: Dynamics")


## ----anova-dyn-vel-1, echo = FALSE--------------------------------------------
vel_dyn_aov <- aov_car(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = vel_dyn)
summary(vel_dyn_aov)


## ----anova-dyn-vel-2, echo = FALSE--------------------------------------------
vel_dyn_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = vel_dyn)
summary(vel_dyn_aov_2)


## ----post-hoc-1, echo = FALSE-------------------------------------------------
vel_dyn_posthoc <- test(pairs(emmeans(vel_dyn_aov,~Condition|Subcomponent), adjust = "tukey"), joint = TRUE)
vel_dyn_posthoc


## ----vel-dyn-trial, echo = FALSE----------------------------------------------
# For each individual
vel_dyn_trial


## ----vel-dyn-trial-line-forte, echo = FALSE, fig.height = 4-------------------
ggline(vel_dyn_trial[Subcomponent == "Forte"], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", xlab = "TrialNr", ylab = "KV (0-127)", title = "KV: Dynamics/Forte") + scale_x_continuous(breaks = seq(1,8,1))


## ----vel-dyn-trial-line-piano, echo = FALSE, fig.height = 4-------------------
ggline(vel_dyn_trial[Subcomponent == "Piano"], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", xlab = "TrialNr", ylab = "KV (0-127)", title = "KV: Dynamics/Piano") + scale_x_continuous(breaks = seq(1,8,1))


## ----vel-dyn-trial-all, echo = FALSE------------------------------------------
# Group mean
vel_dyn_trial_all <- vel_dyn_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, TrialNr)]
vel_dyn_trial_all


## ----vel-dyn-trial-all-line-forte, echo = FALSE-------------------------------
ggline(vel_dyn_trial[Subcomponent == "Forte"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", xlab = "Trial", ylab = "KV (0-127)", title = "KV: Dynamics/Forte") + scale_x_continuous(breaks = seq(1,8,1))


## ----vel-dyn-trial-all-line-piano, echo = FALSE-------------------------------
ggline(vel_dyn_trial[Subcomponent == "Piano"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", xlab = "Trial", ylab = "KV (0-127)", title = "KV: Dynamics/Piano") + scale_x_continuous(breaks = seq(1,8,1))


## ----vel-diff-dyn, echo = FALSE-----------------------------------------------
# For each individual
vel_diff_dyn_trial <- dt_vel_diff_dyn[Subcomponent == "FtoP" | Subcomponent == "PtoF", .(N = .N, Mean = mean(Diff), SD = sd(Diff)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr, Subcomponent)]

vel_diff_dyn <- vel_diff_dyn_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent)]
vel_diff_dyn


## ----vel-diff-dyn-box,  echo = FALSE------------------------------------------
ggboxplot(vel_diff_dyn_trial, "SubNr", "Mean", color = "Condition", xlab = "SubNr", ylab = "Difference", title = "KV-Diff: Dynamics") +
  facet_grid(Subcomponent ~ .)


## ----vel-diff-dyn-all, echo = FALSE-------------------------------------------
# Group mean
vel_diff_dyn_all <- vel_diff_dyn[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent)]
vel_diff_dyn_all


## ----vel-diff-dyn-all-box, echo = FALSE---------------------------------------
vel_diff_dyn_plot <- data.table(subject = vel_diff_dyn[Condition == "teaching"]$SubNr, teaching = vel_diff_dyn[Condition == "teaching"]$Mean, performing = vel_diff_dyn[Condition == "performing"]$Mean, subcomponent = vel_diff_dyn[Condition == "teaching"]$Subcomponent)

ggpaired(vel_diff_dyn_plot, cond = "performing", cond2 = "teaching", color = "condition", facet.by = "subcomponent", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "Difference", title = "KV-Diff: Dynamics")


## ----anova-dyn-vel-diff-1, echo = FALSE---------------------------------------
vel_diff_dyn_aov <- aov_car(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), vel_diff_dyn)
summary(vel_diff_dyn_aov)


## ----anova-dyn-vel-diff-2, echo = FALSE---------------------------------------
vel_diff_dyn_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), vel_diff_dyn)
summary(vel_diff_dyn_aov_2)


## ----post-hoc-2, echo = FALSE-------------------------------------------------
vel_diff_dyn_posthoc <- test(pairs(emmeans(vel_diff_dyn_aov,~Condition|Subcomponent), adjust = "tukey"), joint = TRUE)
vel_diff_dyn_posthoc


## ----vel-diff-dyn-trial, echo = FALSE-----------------------------------------
# For each individual
vel_diff_dyn_trial


## ----vel-diff-dyn-trial-line-ftop, echo = FALSE, fig.height = 4---------------
ggline(vel_diff_dyn_trial[Subcomponent == "FtoP"], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", xlab = "TrialNr", ylab = "Difference", title = "KV-Diff: Dynamics/Forte to Piano") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----vel-diff-dyn-trial-line-ptof, echo = FALSE, fig.height = 4---------------
ggline(vel_diff_dyn_trial[Subcomponent == "PtoF"], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", xlab = "TrialNr", ylab = "Difference", title = "KV-Diff: Dynamics/Piano to Forte") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----vel-diff-dyn-trial-all, echo = FALSE-------------------------------------
# Group mean
vel_diff_dyn_trial_all <- vel_diff_dyn_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, TrialNr)]
vel_diff_dyn_trial_all


## ----vel-diff-dyn-trial-all-line-ftop, echo = FALSE---------------------------
ggline(vel_diff_dyn_trial[Subcomponent == "FtoP"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", xlab = "Trial", ylab = "Difference", title = "KV-Diff: Dynamics/Forte to Piano") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----vel-diff-dyn-trial-all-line-ptof, echo = FALSE---------------------------
ggline(vel_diff_dyn_trial[Subcomponent == "PtoF"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", xlab = "Trial", ylab = "Difference", title = "KV-Diff: Dynamics/Piano to Forte") +
  scale_x_continuous(breaks = seq(1,8,1))


## ----vel-art, echo = FALSE----------------------------------------------------
# For each individual
vel_art_trial <- dt_vel_art[, .(N = .N, Mean = mean(Velocity), SD = sd(Velocity)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr, Subcomponent)]

vel_art <- vel_art_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent)]
vel_art


## ----vel-art-box,  echo = FALSE-----------------------------------------------
ggboxplot(vel_art_trial, "SubNr", "Mean", color = "Condition", xlab = "SubNr", ylab = "KV (0-127)", title = "KV: Articulation") + facet_grid(Subcomponent ~ .)


## ----vel-art-all, echo = FALSE------------------------------------------------
# Group mean
vel_art_all <- vel_art[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent)]
vel_art_all


## ----vel-art-all-box, echo = FALSE--------------------------------------------
vel_art_plot <- data.table(subject = vel_art[Condition == "teaching"]$SubNr, teaching = vel_art[Condition == "teaching"]$Mean, performing = vel_art[Condition == "performing"]$Mean, subcomponent = vel_art[Condition == "teaching"]$Subcomponent)

ggpaired(vel_art_plot, cond = "performing", cond2 = "teaching", color = "condition", facet.by = "subcomponent", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KV (0-127)", title = "KV: Articulation")


## ----anova-art-vel-1, echo = FALSE--------------------------------------------
vel_art_aov <- aov_car(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = vel_art)
summary(vel_art_aov)


## ----anova-art-vel-2, echo = FALSE--------------------------------------------
vel_art_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = vel_art)
summary(vel_art_aov_2)


## ----post-hoc-3, echo = FALSE-------------------------------------------------
vel_art_posthoc <- test(pairs(emmeans(vel_art_aov,~Condition|Subcomponent), adjust = "tukey"), joint = TRUE)
vel_art_posthoc


## ----vel-art-trial, echo = FALSE----------------------------------------------
# For each individual
vel_art_trial


## ----vel-art-trial-line-legato, echo = FALSE, fig.height = 4------------------
ggline(vel_art_trial[Subcomponent == "Legato"], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", xlab = "TrialNr", ylab = "KV (0-127)", title = "KV: Articulation/Legato") + scale_x_continuous(breaks = seq(1,8,1))


## ----vel-art-trial-line-staccato, echo = FALSE, fig.height = 4----------------
ggline(vel_art_trial[Subcomponent == "Staccato"], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", xlab = "TrialNr", ylab = "KV (0-127)", title = "KV: Articulation/Staccato") + scale_x_continuous(breaks = seq(1,8,1))


## ----vel-art-trial-all, echo = FALSE------------------------------------------
# Group mean
vel_art_trial_all <- vel_art_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, TrialNr)]
vel_art_trial_all


## ----vel-art-trial-all-line-legato, echo = FALSE------------------------------
ggline(vel_art_trial[Subcomponent == "Legato"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", xlab = "Trial", ylab = "KV (0-127)", title = "KV: Articulation/Legato") + scale_x_continuous(breaks = seq(1,8,1))


## ----vel-art-trial-all-line-staccato, echo = FALSE----------------------------
ggline(vel_art_trial[Subcomponent == "Staccato"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", xlab = "Trial", ylab = "KV (0-127)", title = "KV: Articulation/Staccato") + scale_x_continuous(breaks = seq(1,8,1))


## ----vel-diff-art, echo = FALSE-----------------------------------------------
# For each individual
vel_diff_art_trial <- dt_vel_diff_art[Subcomponent == "LtoS" | Subcomponent == "StoL", .(N = .N, Mean = mean(Diff), SD = sd(Diff)), by = .(SubNr, Condition, Skill, BlockNr, TrialNr, Subcomponent)]

vel_diff_art <- vel_diff_art_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent)]
vel_diff_art


## ----vel-diff-art-box,  echo = FALSE------------------------------------------
ggboxplot(vel_diff_art_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean)), by = .(SubNr, Condition, Skill, Subcomponent, BlockNr, TrialNr)], "SubNr", "Mean", color = "Condition", xlab = "SubNr", ylab = "Difference", title = "KV-Diff: Articulation") + facet_grid(Subcomponent ~ .)


## ----vel-diff-art-all, echo = FALSE-------------------------------------------
# Group mean
vel_diff_art_all <- vel_diff_art[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent)]
vel_diff_art_all


## ----vel-diff-art-all-box, echo = FALSE---------------------------------------
vel_diff_art_plot <- data.table(subject = vel_diff_art[Condition == "teaching"]$SubNr, teaching = vel_diff_art[Condition == "teaching"]$Mean, performing = vel_diff_art[Condition == "performing"]$Mean, subcomponent = vel_diff_art[Condition == "teaching"]$Subcomponent)

ggpaired(vel_diff_art_plot, cond = "performing", cond2 = "teaching", color = "condition", facet.by = "subcomponent", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "KV (0-127)", title = "KV-Diff: Articulation")


## ----anova-art-vel-diff-1, echo = FALSE---------------------------------------
vel_diff_art_aov <- aov_car(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), vel_diff_art)
summary(vel_diff_art_aov)


## ----anova-art-vel-diff-2, echo = FALSE---------------------------------------
vel_diff_art_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), vel_diff_art)
summary(vel_diff_art_aov_2)


## ----post-hoc-4, echo = FALSE-------------------------------------------------
vel_diff_art_posthoc <- test(pairs(emmeans(vel_diff_art_aov,~Condition|Subcomponent), adjust = "tukey"), joint = TRUE)
vel_diff_art_posthoc


## ----vel-diff-art-trial, echo = FALSE-----------------------------------------
# For each individual
vel_diff_art_trial


## ----vel-diff-artt-rial-line-ltos, echo = FALSE, fig.height = 4---------------
ggline(vel_diff_art_trial[Subcomponent == "LtoS"], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", xlab = "TrialNr", ylab = "Difference", title = "KV-Diff: Articulation/Legato to Staccato") + scale_x_continuous(breaks = seq(1,8,1))


## ----vel-diff-art-trial-line-stol, echo = FALSE, fig.height = 4---------------
ggline(vel_diff_art_trial[Subcomponent == "StoL"], x = "TrialNr", y = "Mean", facet.by = "SubNr", shape = "Condition", color = "Condition", xlab = "TrialNr", ylab = "Difference", title = "KV-Diff: Articulation/Staccato to Legato") + scale_x_continuous(breaks = seq(1,8,1))


## ----vel-diff-art-trial-all, echo = FALSE-------------------------------------
# Group mean
vel_diff_art_trial_all <- vel_diff_art_trial[, .(N = .N, Mean = mean(Mean), SD = sd(Mean), SEM = sd(Mean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent, TrialNr)]
vel_diff_art_trial_all


## ----vel-diff-art-trial-all-line-ltos, echo = FALSE---------------------------
ggline(vel_diff_art_trial[Subcomponent == "LtoS"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", xlab = "Trial", ylab = "Difference", title = "KV-Diff: Articulation/Legato to Staccato") + scale_x_continuous(breaks=seq(1,8,1))


## ----vel-diff-art-trial-all-line-stol, echo = FALSE---------------------------
ggline(vel_diff_art_trial[Subcomponent == "StoL"], x = "TrialNr", y = "Mean", add = "mean_se", position = position_dodge(.2), shape = "Condition", color = "Condition", xlab = "Trial", ylab = "Difference", title = "KV-Diff: Articulation/Staccato to Legato") + scale_x_continuous(breaks=seq(1,8,1))


## ----seq-dyn, fig.width = 9, fig.height = 3, echo = FALSE---------------------
# For each individual
vel_dyn_seq <- dt_vel_dyn[, .(N = .N, Mean = mean(Velocity), SD = sd(Velocity)), by = .(SubNr, Condition, Skill, RowNr)]

ggline(vel_dyn_seq, x = "RowNr", y = "Mean", add = "mean_se", position = position_dodge(.2), linetype = "Condition", shape = "Condition", color = "Condition", xlab = "Note", ylab = "", title = "KV: Dynamics") + scale_x_continuous(breaks = seq(1,72,1))

vel_diff_dyn_seq <- dt_vel_diff_dyn[, .(N = .N, Mean = mean(Diff), SD = sd(Diff)), by = .(SubNr, Condition, Skill, Interval)]

ggline(vel_diff_dyn_seq, x = "Interval", y = "Mean", add = "mean_se", position = position_dodge(.2), linetype = "Condition", shape = "Condition", color = "Condition", xlab = "Interval", ylab = "", title = "KV-Diff: Dynamics") + scale_x_continuous(breaks = seq(1,71,1))


## ----seq-art, fig.width = 9, fig.height = 3, echo = FALSE---------------------
# For each individual
vel_art_seq <- dt_vel_art[, .(N = .N, Mean = mean(Velocity), SD = sd(Velocity)), by = .(SubNr, Condition, Skill, RowNr)]

ggline(vel_art_seq, x = "RowNr", y = "Mean", add = "mean_se", position = position_dodge(.2), linetype = "Condition", shape = "Condition", color = "Condition", xlab = "Note", ylab = "", title = "KV: Articulation") + scale_x_continuous(breaks = seq(1,72,1))

vel_diff_art_seq <- dt_vel_diff_art[, .(N = .N, Mean = mean(Diff), SD = sd(Diff)), by = .(SubNr, Condition, Skill, Interval)]

ggline(vel_diff_art_seq, x = "Interval", y = "Mean", add = "mean_se", position = position_dodge(.2), linetype = "Condition", shape = "Condition", color = "Condition", xlab = "Interval", ylab = "", title = "KV-Diff: Articulation") + scale_x_continuous(breaks = seq(1,71,1))


## ---- echo = FALSE------------------------------------------------------------
# normalise diff by KV range
vel_diff_dyn_trial$NormalisedDiff <- 0
for (i in unique(vel_diff_dyn_trial$SubNr)){
  for (m in unique(vel_diff_dyn_trial[SubNr == i]$BlockNr)){
    for (n in unique(vel_diff_dyn_trial[SubNr == i & BlockNr == m]$TrialNr)){
      # FtoP
      vel_diff_dyn_trial[SubNr == i & BlockNr == m & TrialNr == n & Subcomponent == "FtoP"]$NormalisedDiff <- vel_diff_dyn_trial[SubNr == i & BlockNr == m & TrialNr == n & Subcomponent == "FtoP"]$Mean/(max(vel_dyn_trial[SubNr == i & BlockNr == m & TrialNr == n]$Mean)-min(vel_dyn_trial[SubNr == i & BlockNr == m & TrialNr == n]$Mean))
      # PtoF
      vel_diff_dyn_trial[SubNr == i & BlockNr == m & TrialNr == n & Subcomponent == "PtoF"]$NormalisedDiff <- vel_diff_dyn_trial[SubNr == i & BlockNr == m & TrialNr == n & Subcomponent == "PtoF"]$Mean/(max(vel_dyn_trial[SubNr == i & BlockNr == m & TrialNr == n]$Mean)-min(vel_dyn_trial[SubNr == i & BlockNr == m & TrialNr == n]$Mean))
    }
  }
}

# each participant
vel_diff_dyn_normalised <- vel_diff_dyn_trial[, .(N = .N, NormalisedMean = mean(NormalisedDiff), SD = sd(NormalisedDiff), SEM = sd(NormalisedDiff)/sqrt(.N)), by = .(SubNr, Condition, Skill, Subcomponent)]

# group
vel_diff_dyn_normalised_all <- vel_diff_dyn_normalised[, .(N = .N, Mean = mean(NormalisedMean), SD = sd(NormalisedMean), SEM = sd(NormalisedMean)/sqrt(.N)), by = .(Condition, Skill, Subcomponent)]
vel_diff_dyn_normalised_all


## ---- echo = FALSE------------------------------------------------------------
vel_diff_dyn_plot_2 <- data.table(subject = vel_diff_dyn_normalised[Condition == "teaching"]$SubNr, teaching = vel_diff_dyn_normalised[Condition == "teaching"]$NormalisedMean, performing = vel_diff_dyn_normalised[Condition == "performing"]$NormalisedMean, subcomponent = vel_diff_dyn_normalised[Condition == "teaching"]$Subcomponent)

ggpaired(vel_diff_dyn_plot_2, cond = "performing", cond2 = "teaching", color = "condition", facet.by = "subcomponent", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "Normalised KV (0-127)", title = "KV-Diff: Dynamics")


## ---- echo = FALSE------------------------------------------------------------
# stats
vel_diff_dyn_aov_3 <- aov_car(NormalisedMean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), vel_diff_dyn_normalised)
summary(vel_diff_dyn_aov_3)


## ---- echo = FALSE------------------------------------------------------------
vel_diff_dyn$NormalisedDiff <- 0
for (i in unique(vel_diff_dyn_trial$SubNr)){
  # teaching
  # FtoP
  vel_diff_dyn[SubNr == i & Condition == "teaching" & Subcomponent == "FtoP"]$NormalisedDiff <- vel_diff_dyn[SubNr == i & Condition == "teaching" & Subcomponent == "FtoP"]$Mean/(max(vel_dyn[SubNr == i & Condition == "teaching"]$Mean)-min(vel_dyn[SubNr == i & Condition == "teaching"]$Mean))
  # PtoF
  vel_diff_dyn[SubNr == i & Condition == "teaching" & Subcomponent == "PtoF"]$NormalisedDiff <- vel_diff_dyn[SubNr == i & Condition == "teaching" & Subcomponent == "PtoF"]$Mean/(max(vel_dyn[SubNr == i & Condition == "teaching"]$Mean)-min(vel_dyn[SubNr == i & Condition == "teaching"]$Mean))
  # performing
  # FtoP
  vel_diff_dyn[SubNr == i & Condition == "performing" & Subcomponent == "FtoP"]$NormalisedDiff <- vel_diff_dyn[SubNr == i & Condition == "performing" & Subcomponent == "FtoP"]$Mean/(max(vel_dyn[SubNr == i & Condition == "performing"]$Mean)-min(vel_dyn[SubNr == i & Condition == "performing"]$Mean))
  # PtoF
  vel_diff_dyn[SubNr == i & Condition == "performing" & Subcomponent == "PtoF"]$NormalisedDiff <- vel_diff_dyn[SubNr == i & Condition == "performing" & Subcomponent == "PtoF"]$Mean/(max(vel_dyn[SubNr == i & Condition == "performing"]$Mean)-min(vel_dyn[SubNr == i & Condition == "performing"]$Mean))
}

# group
vel_diff_dyn_normalised_all_2 <- vel_diff_dyn[, .(N = .N, Mean = mean(NormalisedDiff), SD = sd(NormalisedDiff), SEM = sd(NormalisedDiff)/sqrt(.N)), by = .(Condition, Skill, Subcomponent)]
vel_diff_dyn_normalised_all_2


## ---- echo = FALSE------------------------------------------------------------
vel_diff_dyn_plot_3 <- data.table(subject = vel_diff_dyn[Condition == "teaching"]$SubNr, teaching = vel_diff_dyn[Condition == "teaching"]$NormalisedDiff, performing = vel_diff_dyn[Condition == "performing"]$NormalisedDiff, subcomponent = vel_diff_dyn[Condition == "teaching"]$Subcomponent)

ggpaired(vel_diff_dyn_plot_3, cond = "performing", cond2 = "teaching", color = "condition", facet.by = "subcomponent", line.size = 0.3, line.color = "gray", xlab = "Condition", ylab = "Normalised KV (0-127)", title = "KV-Diff: Dynamics")


## ---- echo = FALSE------------------------------------------------------------
# stats
vel_diff_dyn_aov_4 <- aov_car(NormalisedDiff ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), vel_diff_dyn)
summary(vel_diff_dyn_aov_4)


## ----export, include = FALSE--------------------------------------------------
knitr::purl("dynamics.Rmd")

