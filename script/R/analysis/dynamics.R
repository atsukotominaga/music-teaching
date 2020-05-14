## ----setup, include = FALSE-----------------------------------------------------------------------------------------------
# packages
# data manipulation
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
# plot
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("ggsignif")) {install.packages("ggsignif"); require("ggsignif")}
# statistics
if (!require("stats")) {install.packages("stats"); require("stats")}
if (!require("effsize")) {install.packages("effsize"); require("effsize")}
if (!require("ez")) {install.packages("ez"); require("ez")}

# ggplots
theme_set(theme_classic())
theme_update(text = element_text(size = 20, family = "Helvetica Neue LT Std 57 Condensed"), legend.position = "bottom")


## ----extract, include = FALSE---------------------------------------------------------------------------------------------
df_vel <- read.csv("./trimmed/data_vel.csv", header = T, sep = ",", dec = ".") # read a trimmed csv
df_vel_diff <- read.csv("./trimmed/data_vel_diff.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

# SubNr as a factor
df_vel$SubNr <- as.factor(df_vel$SubNr)
df_vel_diff$SubNr <- as.factor(df_vel_diff$SubNr)

# Include only articulation
df_vel_art <- df_vel %>% dplyr::filter(Skill == "articulation")
df_vel_diff_art <- df_vel_diff %>% dplyr::filter(Skill == "articulation")

# Include only dynamics
df_vel_dyn <- df_vel %>% dplyr::filter(Skill == "dynamics")
df_vel_diff_dyn <- df_vel_diff %>% dplyr::filter(Skill == "dynamics")


## ----vel_dyn, echo = FALSE------------------------------------------------------------------------------------------------
# For each individual
vel_dyn <- aggregate(Velocity~SubNr*Condition*Skill*Subcomponent, data = df_vel_dyn,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_dyn <- cbind(vel_dyn[,1:4], as.data.frame(vel_dyn[,5]))
# Change colnames
colnames(vel_dyn) <- c("SubNr", "Condition", "Skill", "Subcomponent", "N", "Mean", "SD")
print(vel_dyn)


## ----vel_dyn_bar,  echo = FALSE-------------------------------------------------------------------------------------------
p_vel_dyn <- ggplot(data = vel_dyn, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(.9)) +
  facet_grid(Subcomponent ~ .) +
  labs(y = "KV (0-127)", subtitle = "KV: Dynamics") + coord_cartesian(ylim = c(40, 100))
p_vel_dyn


## ----vel_dyn_all, echo = FALSE--------------------------------------------------------------------------------------------
# Group mean
vel_dyn_all <- aggregate(Mean~Condition*Skill*Subcomponent, data = vel_dyn,
                         FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_dyn_all <- cbind(vel_dyn_all[,1:3], as.data.frame(vel_dyn_all[,4]))
# Change colnames
colnames(vel_dyn_all) <- c("Condition", "Skill", "Subcomponent", "N", "Mean", "SD", "SEM")
print(vel_dyn_all)


## ----vel_dyn_all_bar, echo = FALSE----------------------------------------------------------------------------------------
p_vel_dyn_all <- ggplot(data = vel_dyn_all, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = .2, position = position_dodge(.9)) +
  labs(y = "KV (0-127)", subtitle = "KV: Dynamics") + coord_cartesian(ylim = c(40, 100))
p_vel_dyn_all


## ----vel_dyn_all_box, echo = FALSE----------------------------------------------------------------------------------------
p_vel_dyn_all_box <- ggplot(data = vel_dyn, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, position = position_dodge(0.75)) +
  labs(y = "KV (0-127)", subtitle = "KV: Dynamics")
p_vel_dyn_all_box


## ----normality1, echo = FALSE---------------------------------------------------------------------------------------------
# Reduce unused factors
vel_dyn$Subcomponent <- factor(vel_dyn$Subcomponent)
vel_dyn_norm <- by(vel_dyn$Mean, list(vel_dyn$Condition, vel_dyn$Subcomponent), shapiro.test)
print(vel_dyn_norm)


## ----stats1, echo = FALSE-------------------------------------------------------------------------------------------------
vel_dyn_aov <- ezANOVA(
  data = df_vel_dyn
  , dv = .(Velocity)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , detailed = TRUE
)
vel_dyn_aov$ANOVA$F <- round(vel_dyn_aov$ANOVA$F, 4)
vel_dyn_aov$ANOVA$p <- round(vel_dyn_aov$ANOVA$p, 4)
print(vel_dyn_aov$ANOVA)


## ----stats2, echo = FALSE-------------------------------------------------------------------------------------------------
vel_dyn_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = vel_dyn)
print(summary(vel_dyn_aov_2))


## ----post-hoc1, echo = FALSE----------------------------------------------------------------------------------------------
vel_ttest_forte <- t.test(vel_dyn$Mean[vel_dyn$Condition == "performing" & vel_dyn$Subcomponent == "Forte"], vel_dyn$Mean[vel_dyn$Condition == "teaching" & vel_dyn$Subcomponent == "Forte"], paired = TRUE)
vel_ttest_piano <- t.test(vel_dyn$Mean[vel_dyn$Condition == "performing" & vel_dyn$Subcomponent == "Piano"], vel_dyn$Mean[vel_dyn$Condition == "teaching" & vel_dyn$Subcomponent == "Piano"], paired = TRUE)

print(vel_ttest_forte)
print(vel_ttest_piano)


## ----effect1, echo = FALSE------------------------------------------------------------------------------------------------
# Effect size
vel_ttest_forte_cohend <- cohen.d(vel_dyn$Mean[vel_dyn$Subcomponent == "Forte"], vel_dyn$Condition[vel_dyn$Subcomponent == "Forte"], paired = TRUE)
vel_ttest_piano_cohend <- cohen.d(vel_dyn$Mean[vel_dyn$Subcomponent == "Piano"], vel_dyn$Condition[vel_dyn$Subcomponent == "Piano"], paired = TRUE)

print(vel_ttest_forte_cohend)
print(vel_ttest_piano_cohend)


## ----post-hoc2, echo = FALSE----------------------------------------------------------------------------------------------
# Create a group label for a post-hoc test
vel_dyn$Group[vel_dyn$Condition == "performing" & vel_dyn$Subcomponent == "Forte"] <- "Performing-Forte" 
vel_dyn$Group[vel_dyn$Condition == "performing" & vel_dyn$Subcomponent == "Piano"] <- "Performing-Piano"
vel_dyn$Group[vel_dyn$Condition == "teaching" & vel_dyn$Subcomponent == "Forte"] <- "Teaching-Forte"
vel_dyn$Group[vel_dyn$Condition == "teaching" & vel_dyn$Subcomponent == "Piano"] <- "Teaching-Piano"
# pairwise comparison
vel_dyn_pairwise <- pairwise.t.test(vel_dyn$Mean, vel_dyn$Group, paired = TRUE, p.adjust.method = "BH")
vel_dyn_pairwise$p.value <- round(vel_dyn_pairwise$p.value, 4)
print(vel_dyn_pairwise)


## ----vel_dyn_trial, echo = FALSE------------------------------------------------------------------------------------------
# For each individual
vel_dyn_trial <- aggregate(Velocity~SubNr*Condition*Skill*Subcomponent*TrialNr, data = df_vel_dyn,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_dyn_trial <- cbind(vel_dyn_trial[,1:5], as.data.frame(vel_dyn_trial[,6]))
# Change colnames
colnames(vel_dyn_trial) <- c("SubNr", "Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD")
print(vel_dyn_trial)


## ----vel_dyn_trial_line_forte, echo = FALSE, fig.height = 4---------------------------------------------------------------
p_vel_dyn_trial_forte <- ggplot(data = subset(vel_dyn_trial, vel_dyn_trial$Subcomponent == "Forte"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "KV (0-127)", subtitle = "KV: Dynamics/Forte") +　scale_x_continuous(breaks=seq(1,8,1))
p_vel_dyn_trial_forte


## ----vel_dyn_trial_line_piano, echo = FALSE, fig.height = 4---------------------------------------------------------------
p_vel_dyn_trial_piano <- ggplot(data = subset(vel_dyn_trial, vel_dyn_trial$Subcomponent == "Piano"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "KV (0-127)", subtitle = "KV: Dynamics/Piano") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_dyn_trial_piano


## ----vel_dyn_trial_all, echo = FALSE--------------------------------------------------------------------------------------
# Group mean
vel_dyn_trial_all <- aggregate(Mean~Condition*Skill*Subcomponent*TrialNr, data = vel_dyn_trial,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_dyn_trial_all <- cbind(vel_dyn_trial_all[,1:4], as.data.frame(vel_dyn_trial_all[,5]))
# Change colnames
colnames(vel_dyn_trial_all) <- c("Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD", "SEM")
print(vel_dyn_trial_all)


## ----vel_dyn_trial_all_line_forte, echo = FALSE---------------------------------------------------------------------------
p_vel_dyn_trial_all_forte <- ggplot(data = subset(vel_dyn_trial_all, vel_dyn_trial_all$Subcomponent == "Forte"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "KV (0-127)", subtitle = "KV: Dynamics/Forte") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_dyn_trial_all_forte


## ----vel_dyn_trial_all_line_piano, echo = FALSE---------------------------------------------------------------------------
p_vel_dyn_trial_all_piano <- ggplot(data = subset(vel_dyn_trial_all, vel_dyn_trial_all$Subcomponent == "Piano"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "KV (0-127)", subtitle = "KV: Dynamics/Piano") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_dyn_trial_all_piano


## ----vel_diff_dyn, echo = FALSE-------------------------------------------------------------------------------------------
# For each individual
vel_diff_dyn <- aggregate(Diff~SubNr*Condition*Skill*Subcomponent, data = subset(df_vel_diff_dyn, df_vel_diff_dyn$Subcomponent == "FtoP" | df_vel_diff_dyn$Subcomponent == "PtoF"),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_diff_dyn <- cbind(vel_diff_dyn[,1:4], as.data.frame(vel_diff_dyn[,5]))
# Change colnames
colnames(vel_diff_dyn) <- c("SubNr", "Condition", "Skill", "Subcomponent", "N", "Mean", "SD")
print(vel_diff_dyn)


## ----vel_diff_dyn_bar,  echo = FALSE--------------------------------------------------------------------------------------
p_vel_diff_dyn <- ggplot(data = vel_diff_dyn, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(.9)) +
  facet_grid(Subcomponent ~ .) +
  labs(y = "Difference", subtitle = "KV-Diff: Dynamics")
p_vel_diff_dyn


## ----vel_diff_dyn_all, echo = FALSE---------------------------------------------------------------------------------------
# Group mean
vel_diff_dyn_all <- aggregate(Mean~Condition*Skill*Subcomponent, data = vel_diff_dyn,
                         FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_diff_dyn_all <- cbind(vel_diff_dyn_all[,1:3], as.data.frame(vel_diff_dyn_all[,4]))
# Change colnames
colnames(vel_diff_dyn_all) <- c("Condition", "Skill", "Subcomponent", "N", "Mean", "SD", "SEM")
print(vel_diff_dyn_all)


## ----vel_diff_dyn_all_bar,  echo = FALSE----------------------------------------------------------------------------------
p_vel_diff_dyn_all <- ggplot(data = vel_diff_dyn_all, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = .2, position = position_dodge(.9)) +
  labs(y = "Difference", subtitle = "KV-Diff: Dynamics")
p_vel_diff_dyn_all


## ----vel_diff_dyn_all_box, echo = FALSE-----------------------------------------------------------------------------------
p_vel_diff_dyn_all_box <- ggplot(data = vel_diff_dyn, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, position = position_dodge(0.75)) +
  labs(y = "Difference", subtitle = "KV-Diff: Dynamics")
p_vel_diff_dyn_all_box


## ----normality2, echo = FALSE---------------------------------------------------------------------------------------------
# Reduce unused factors
vel_diff_dyn$Subcomponent <- factor(vel_diff_dyn$Subcomponent)
vel_diff_dyn_norm <- by(vel_diff_dyn$Mean, list(vel_diff_dyn$Condition, vel_diff_dyn$Subcomponent), shapiro.test)
print(vel_diff_dyn_norm)


## ----stats3, echo = FALSE-------------------------------------------------------------------------------------------------
vel_diff_dyn_aov <- ezANOVA(
  data = subset(df_vel_diff_dyn, df_vel_diff_dyn$Subcomponent == "FtoP" | df_vel_diff_dyn$Subcomponent == "PtoF")
  , dv = .(Diff)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , detailed = TRUE
)
vel_diff_dyn_aov$ANOVA$F <- round(vel_diff_dyn_aov$ANOVA$F, 4)
vel_diff_dyn_aov$ANOVA$p <- round(vel_diff_dyn_aov$ANOVA$p, 4)
print(vel_diff_dyn_aov$ANOVA)


## ----stats4, echo = FALSE-------------------------------------------------------------------------------------------------
vel_diff_dyn_aov_2 <- with(vel_diff_dyn,
                      aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent))))
print(summary(vel_diff_dyn_aov_2))


## ----post-hoc3, echo = FALSE----------------------------------------------------------------------------------------------
vel_diff_ttest_ftop <- t.test(vel_diff_dyn$Mean[vel_diff_dyn$Condition == "performing" & vel_diff_dyn$Subcomponent == "FtoP"], vel_diff_dyn$Mean[vel_diff_dyn$Condition == "teaching" & vel_diff_dyn$Subcomponent == "FtoP"], paired = TRUE)
vel_diff_ttest_ptof <- t.test(vel_diff_dyn$Mean[vel_diff_dyn$Condition == "performing" & vel_diff_dyn$Subcomponent == "PtoF"], vel_diff_dyn$Mean[vel_diff_dyn$Condition == "teaching" & vel_diff_dyn$Subcomponent == "PtoF"], paired = TRUE)

print(vel_diff_ttest_ftop)
print(vel_diff_ttest_ptof)


## ----effect2, echo = FALSE------------------------------------------------------------------------------------------------
# Effect size
vel_diff_ttest_ftop_cohend <- cohen.d(vel_diff_dyn$Mean[vel_diff_dyn$Subcomponent == "FtoP"], vel_diff_dyn$Condition[vel_diff_dyn$Subcomponent == "FtoP"], paired = TRUE)
vel_diff_ttest_ptof_cohend <- cohen.d(vel_diff_dyn$Mean[vel_diff_dyn$Subcomponent == "PtoF"], vel_diff_dyn$Condition[vel_diff_dyn$Subcomponent == "PtoF"], paired = TRUE)

print(vel_diff_ttest_ftop_cohend)
print(vel_diff_ttest_ptof_cohend)


## ----post-hoc4, echo = FALSE----------------------------------------------------------------------------------------------
# Create a group label for a post-hoc test
vel_diff_dyn$Group[vel_diff_dyn$Condition == "performing" & vel_diff_dyn$Subcomponent == "FtoP"] <- "Performing-FtoP" 
vel_diff_dyn$Group[vel_diff_dyn$Condition == "performing" & vel_diff_dyn$Subcomponent == "PtoF"] <- "Performing-PtoF"
vel_diff_dyn$Group[vel_diff_dyn$Condition == "teaching" & vel_diff_dyn$Subcomponent == "FtoP"] <- "Teaching-FtoP"
vel_diff_dyn$Group[vel_diff_dyn$Condition == "teaching" & vel_diff_dyn$Subcomponent == "PtoF"] <- "Teaching-PtoF"
# pairwise comparison
vel_diff_dyn_pairwise <- pairwise.t.test(vel_diff_dyn$Mean, vel_diff_dyn$Group, paired = TRUE, p.adjust.method = "BH")
vel_diff_dyn_pairwise$p.value <- round(vel_diff_dyn_pairwise$p.value, 4)
print(vel_diff_dyn_pairwise)


## ----vel_diff_dyn_trial, echo = FALSE-------------------------------------------------------------------------------------
# For each individual
vel_diff_dyn_trial <- aggregate(Diff~SubNr*Condition*Skill*Subcomponent*TrialNr, data = subset(df_vel_diff_dyn, df_vel_diff_dyn$Subcomponent == "FtoP" | df_vel_diff_dyn$Subcomponent == "PtoF"),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_diff_dyn_trial <- cbind(vel_diff_dyn_trial[,1:5], as.data.frame(vel_diff_dyn_trial[,6]))
# Change colnames
colnames(vel_diff_dyn_trial) <- c("SubNr", "Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD")
print(vel_diff_dyn_trial)


## ----vel_diff_dyn_trial_line_ftop, echo = FALSE, fig.height = 4-----------------------------------------------------------
p_vel_diff_dyn_trial_ftop <- ggplot(data = subset(vel_diff_dyn_trial, vel_diff_dyn_trial$Subcomponent == "FtoP"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "Difference", subtitle = "KV-Diff: Dynamics/Forte to Piano") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_diff_dyn_trial_ftop


## ----vel_diff_dyn_trial_line_ptof, echo = FALSE, fig.height = 4-----------------------------------------------------------
p_vel_diff_dyn_trial_ptof <- ggplot(data = subset(vel_diff_dyn_trial, vel_diff_dyn_trial$Subcomponent == "PtoF"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "Difference", subtitle = "KV-Diff: Dynamics/Piano to Forte") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_diff_dyn_trial_ptof


## ----vel_diff_dyn_trial_all, echo = FALSE---------------------------------------------------------------------------------
# Group mean
vel_diff_dyn_trial_all <- aggregate(Mean~Condition*Skill*Subcomponent*TrialNr, data = vel_diff_dyn_trial,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_diff_dyn_trial_all <- cbind(vel_diff_dyn_trial_all[,1:4], as.data.frame(vel_diff_dyn_trial_all[,5]))
# Change colnames
colnames(vel_diff_dyn_trial_all) <- c("Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD", "SEM")
print(vel_diff_dyn_trial_all)


## ----vel_diff_dyn_trial_all_line_ftop, echo = FALSE-----------------------------------------------------------------------
p_vel_diff_dyn_trial_all_ftop <- ggplot(data = subset(vel_diff_dyn_trial_all, vel_diff_dyn_trial_all$Subcomponent == "FtoP"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "Difference", subtitle = "KV-Diff: Dynamics/Forte to Piano") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_diff_dyn_trial_all_ftop


## ----vel_diff_dyn_trial_all_line_ptof, echo = FALSE-----------------------------------------------------------------------
p_vel_diff_dyn_trial_all_ptof <- ggplot(data = subset(vel_diff_dyn_trial_all, vel_diff_dyn_trial_all$Subcomponent == "PtoF"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "Difference", subtitle = "KV-Diff: Dynamics/Piano to Forte") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_diff_dyn_trial_all_ptof


## ----vel_art, echo = FALSE------------------------------------------------------------------------------------------------
# For each individual
vel_art <- aggregate(Velocity~SubNr*Condition*Skill*Subcomponent, data = df_vel_art,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_art <- cbind(vel_art[,1:4], as.data.frame(vel_art[,5]))
# Change colnames
colnames(vel_art) <- c("SubNr", "Condition", "Skill", "Subcomponent", "N", "Mean", "SD")
print(vel_art)


## ----vel_art_bar,  echo = FALSE-------------------------------------------------------------------------------------------
p_vel_art <- ggplot(data = vel_art, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(.9)) +
  facet_grid(Subcomponent ~ .) +
  labs(y = "KV (0-127)", subtitle = "KV: Articulation") + coord_cartesian(ylim = c(40, 100))
p_vel_art


## ----vel_art_all, echo = FALSE--------------------------------------------------------------------------------------------
# Group mean
vel_art_all <- aggregate(Mean~Condition*Skill*Subcomponent, data = vel_art,
                         FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_art_all <- cbind(vel_art_all[,1:3], as.data.frame(vel_art_all[,4]))
# Change colnames
colnames(vel_art_all) <- c("Condition", "Skill", "Subcomponent", "N", "Mean", "SD", "SEM")
print(vel_art_all)


## ----vel_art_all_bar, echo = FALSE----------------------------------------------------------------------------------------
p_vel_art_all <- ggplot(data = vel_art_all, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = .2, position = position_dodge(.9)) +
  labs(y = "KV (0-127)", subtitle = "KV: Articulation") + coord_cartesian(ylim = c(40, 100))
p_vel_art_all


## ----vel_art_all_box, echo = FALSE----------------------------------------------------------------------------------------
p_vel_art_all_box <- ggplot(data = vel_art, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, position = position_dodge(0.75)) +
  labs(y = "KV (0-127)", subtitle = "KV: Articulation")
p_vel_art_all_box


## ----normality3, echo = FALSE---------------------------------------------------------------------------------------------
# Reduce unused factors
vel_art$Subcomponent <- factor(vel_art$Subcomponent)
vel_art_norm <- by(vel_art$Mean, list(vel_art$Condition, vel_art$Subcomponent), shapiro.test)
print(vel_art_norm)


## ----stats5, echo = FALSE-------------------------------------------------------------------------------------------------
vel_art_aov <- ezANOVA(
  data = df_vel_art
  , dv = .(Velocity)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , detailed = TRUE
)
vel_art_aov$ANOVA$F <- round(vel_art_aov$ANOVA$F, 4)
vel_art_aov$ANOVA$p <- round(vel_art_aov$ANOVA$p, 4)
print(vel_art_aov$ANOVA)


## ----stats6, echo = FALSE-------------------------------------------------------------------------------------------------
vel_art_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = vel_art)
print(summary(vel_art_aov_2))


## ----vel_art_trial, echo = FALSE------------------------------------------------------------------------------------------
# For each individual
vel_art_trial <- aggregate(Velocity~SubNr*Condition*Skill*Subcomponent*TrialNr, data = df_vel_art,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_art_trial <- cbind(vel_art_trial[,1:5], as.data.frame(vel_art_trial[,6]))
# Change colnames
colnames(vel_art_trial) <- c("SubNr", "Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD")
print(vel_art_trial)


## ----vel_art_trial_line_legato, echo = FALSE, fig.height = 4--------------------------------------------------------------
p_vel_art_trial_legato <- ggplot(data = subset(vel_art_trial, vel_art_trial$Subcomponent == "Legato"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "KV (0-127)", subtitle = "KV: Articulation/Legato") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_art_trial_legato


## ----vel_art_trial_line_piano, echo = FALSE, fig.height = 4---------------------------------------------------------------
p_vel_art_trial_staccato <- ggplot(data = subset(vel_art_trial, vel_art_trial$Subcomponent == "Staccato"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "KV (0-127)", subtitle = "KV: Articulation/Staccato") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_art_trial_staccato


## ----vel_art_trial_all, echo = FALSE--------------------------------------------------------------------------------------
# Group mean
vel_art_trial_all <- aggregate(Mean~Condition*Skill*Subcomponent*TrialNr, data = vel_art_trial,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_art_trial_all <- cbind(vel_art_trial_all[,1:4], as.data.frame(vel_art_trial_all[,5]))
# Change colnames
colnames(vel_art_trial_all) <- c("Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD", "SEM")
print(vel_art_trial_all)


## ----vel_art_trial_all_line_legato, echo = FALSE--------------------------------------------------------------------------
p_vel_art_trial_all_legato <- ggplot(data = subset(vel_art_trial_all, vel_art_trial_all$Subcomponent == "Legato"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "KV (0-127)", subtitle = "KV: Articulation/Legato") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_art_trial_all_legato


## ----vel_art_trial_all_line_staccato, echo = FALSE------------------------------------------------------------------------
p_vel_art_trial_all_staccato <- ggplot(data = subset(vel_art_trial_all, vel_art_trial_all$Subcomponent == "Staccato"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "KV (0-127)", subtitle = "KV: Articulation/Staccato") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_art_trial_all_staccato


## ----vel_diff_art, echo = FALSE-------------------------------------------------------------------------------------------
# For each individual
vel_diff_art <- aggregate(Diff~SubNr*Condition*Skill*Subcomponent, data = subset(df_vel_diff_art, df_vel_diff_art$Subcomponent == "LtoS" | df_vel_diff_art$Subcomponent == "StoL"),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_diff_art <- cbind(vel_diff_art[,1:4], as.data.frame(vel_diff_art[,5]))
# Change colnames
colnames(vel_diff_art) <- c("SubNr", "Condition", "Skill", "Subcomponent", "N", "Mean", "SD")
print(vel_diff_art)


## ----vel_diff_art_bar,  echo = FALSE--------------------------------------------------------------------------------------
p_vel_diff_art <- ggplot(data = vel_diff_art, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(.9)) +
  facet_grid(Subcomponent ~ .) +
  labs(y = "Difference", subtitle = "KV-Diff: Articulaion")
p_vel_diff_art


## ----vel_diff_art_all, echo = FALSE---------------------------------------------------------------------------------------
# Group mean
vel_diff_art_all <- aggregate(Mean~Condition*Skill*Subcomponent, data = vel_diff_art,
                         FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_diff_art_all <- cbind(vel_diff_art_all[,1:3], as.data.frame(vel_diff_art_all[,4]))
# Change colnames
colnames(vel_diff_art_all) <- c("Condition", "Skill", "Subcomponent", "N", "Mean", "SD", "SEM")
print(vel_diff_art_all)


## ----vel_diff_art_all_bar,  echo = FALSE----------------------------------------------------------------------------------
p_vel_diff_art_all <- ggplot(data = vel_diff_art_all, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = .2, position = position_dodge(.9)) +
  labs(y = "Difference", subtitle = "KV-Diff: Articulation")
p_vel_diff_art_all


## ----vel_diff_art_all_box, echo = FALSE-----------------------------------------------------------------------------------
p_vel_diff_art_all_box <- ggplot(data = vel_diff_art, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, position = position_dodge(0.75)) +
  labs(y = "Difference", subtitle = "KV-Diff: Articulation")
p_vel_diff_art_all_box


## ----normality4, echo = FALSE---------------------------------------------------------------------------------------------
# Reduce unused factors
vel_diff_art$Subcomponent <- factor(vel_diff_art$Subcomponent)
vel_diff_art_norm <- by(vel_diff_art$Mean, list(vel_diff_art$Condition, vel_diff_art$Subcomponent), shapiro.test)
print(vel_diff_art_norm)


## ----stats7, echo = FALSE-------------------------------------------------------------------------------------------------
vel_diff_art_aov <- ezANOVA(
  data = subset(df_vel_diff_art, df_vel_diff_art$Subcomponent == "LtoS" | df_vel_diff_art$Subcomponent == "StoL")
  , dv = .(Diff)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , detailed = TRUE
)
vel_diff_art_aov$ANOVA$F <- round(vel_diff_art_aov$ANOVA$F, 4)
vel_diff_art_aov$ANOVA$p <- round(vel_diff_art_aov$ANOVA$p, 4)
print(vel_diff_art_aov$ANOVA)


## ----stats8, echo = FALSE-------------------------------------------------------------------------------------------------
vel_diff_art_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = vel_diff_art)
print(summary(vel_diff_art_aov_2))


## ----vel_diff_art_trial, echo = FALSE-------------------------------------------------------------------------------------
# For each individual
vel_diff_art_trial <- aggregate(Diff~SubNr*Condition*Skill*Subcomponent*TrialNr, data = subset(df_vel_diff_art, df_vel_diff_art$Subcomponent == "LtoS" | df_vel_diff_art$Subcomponent == "StoL"),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_diff_art_trial <- cbind(vel_diff_art_trial[,1:5], as.data.frame(vel_diff_art_trial[,6]))
# Change colnames
colnames(vel_diff_art_trial) <- c("SubNr", "Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD")
print(vel_diff_art_trial)


## ----vel_diff_art_trial_line_ltos, echo = FALSE, fig.height = 4-----------------------------------------------------------
p_vel_diff_art_trial_ltos <- ggplot(data = subset(vel_diff_art_trial, vel_diff_art_trial$Subcomponent == "LtoS"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "Difference", subtitle = "KV-Diff: Articulation/Legato to Staccato") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_diff_art_trial_ltos


## ----vel_diff_art_trial_line_stol, echo = FALSE, fig.height = 4-----------------------------------------------------------
p_vel_diff_art_trial_stol <- ggplot(data = subset(vel_diff_art_trial, vel_diff_art_trial$Subcomponent == "StoL"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "Difference", subtitle = "KV-Diff: Articulation/Staccato to Legato") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_diff_art_trial_stol


## ----vel_diff_art_trial_all, echo = FALSE---------------------------------------------------------------------------------
# Group mean
vel_diff_art_trial_all <- aggregate(Mean~Condition*Skill*Subcomponent*TrialNr, data = vel_diff_art_trial,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_diff_art_trial_all <- cbind(vel_diff_art_trial_all[,1:4], as.data.frame(vel_diff_art_trial_all[,5]))
# Change colnames
colnames(vel_diff_art_trial_all) <- c("Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD", "SEM")
print(vel_diff_art_trial_all)


## ----vel_diff_art_trial_all_line_ltos, echo = FALSE-----------------------------------------------------------------------
p_vel_diff_art_trial_all_ltos <- ggplot(data = subset(vel_diff_art_trial_all, vel_diff_art_trial_all$Subcomponent == "LtoS"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "Difference", subtitle = "KV-Diff: Articulation/Legato to Staccato") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_diff_art_trial_all_ltos


## ----vel_diff_art_trial_all_line_stol, echo = FALSE-----------------------------------------------------------------------
p_vel_diff_art_trial_all_stol <- ggplot(data = subset(vel_diff_art_trial_all, vel_diff_art_trial_all$Subcomponent == "StoL"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "Difference", subtitle = "KV-Diff: Articulation/Legato to Staccato") + scale_x_continuous(breaks=seq(1,8,1))
p_vel_diff_art_trial_all_stol


## ----seq_dyn, fig.width = 10, fig.height = 2, echo = FALSE----------------------------------------------------------------
# Average vel_dynocity for each note
# For each participant
vel_dyn_seq <- aggregate(Velocity~SubNr*Condition*Skill*RowNr, data = df_vel_dyn, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_dyn_seq <- cbind(vel_dyn_seq[,1:4], as.data.frame(vel_dyn_seq[,5]))
# Change colnames
colnames(vel_dyn_seq) <- c("SubNr", "Condition", "Skill", "Note", "N", "Mean", "SD")

# Group mean
vel_dyn_seq_stats <- aggregate(Mean~Condition*Skill*Note, data = vel_dyn_seq,
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_dyn_seq_stats <- cbind(vel_dyn_seq_stats[,1:3], as.data.frame(vel_dyn_seq_stats[,4]))
# Change colnames
colnames(vel_dyn_seq_stats) <- c("Condition", "Skill", "Note", "N", "Mean", "SD", "SEM")

# Average acceleration for each note
# For each participant
vel_dyn_diff_seq <- aggregate(Diff~SubNr*Condition*Skill*Interval, data = df_vel_diff_dyn, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_dyn_diff_seq <- cbind(vel_dyn_diff_seq[,1:4], as.data.frame(vel_dyn_diff_seq[,5]))
# Change colnames
colnames(vel_dyn_diff_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# Group mean
vel_dyn_diff_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = vel_dyn_diff_seq,
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_dyn_diff_seq_stats <- cbind(vel_dyn_diff_seq_stats[,1:3], as.data.frame(vel_dyn_diff_seq_stats[,4]))
# Change colnames
colnames(vel_dyn_diff_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

p_vel_dyn_seq <- ggplot(data = vel_dyn_seq_stats, aes(x = Note, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(0.05)) + 
  labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,67,1))
p_vel_dyn_seq

p_vel_dyn_diff_seq <- ggplot(data = vel_dyn_diff_seq_stats, aes(x = Interval, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "Difference") + scale_x_continuous(breaks=seq(1,66,1))
p_vel_dyn_diff_seq


## ----seq_art, fig.width = 10, fig.height = 2, echo = FALSE----------------------------------------------------------------
# Average vel_artocity for each note
# For each participant
vel_art_seq <- aggregate(Velocity~SubNr*Condition*Skill*RowNr, data = df_vel_art, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_art_seq <- cbind(vel_art_seq[,1:4], as.data.frame(vel_art_seq[,5]))
# Change colnames
colnames(vel_art_seq) <- c("SubNr", "Condition", "Skill", "Note", "N", "Mean", "SD")

# Group mean
vel_art_seq_stats <- aggregate(Mean~Condition*Skill*Note, data = vel_art_seq,
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_art_seq_stats <- cbind(vel_art_seq_stats[,1:3], as.data.frame(vel_art_seq_stats[,4]))
# Change colnames
colnames(vel_art_seq_stats) <- c("Condition", "Skill", "Note", "N", "Mean", "SD", "SEM")

# Average acceleration for each note
# For each participant
vel_art_diff_seq <- aggregate(Diff~SubNr*Condition*Skill*Interval, data = df_vel_diff_art, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_art_diff_seq <- cbind(vel_art_diff_seq[,1:4], as.data.frame(vel_art_diff_seq[,5]))
# Change colnames
colnames(vel_art_diff_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# Group mean
vel_art_diff_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = vel_art_diff_seq,
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_art_diff_seq_stats <- cbind(vel_art_diff_seq_stats[,1:3], as.data.frame(vel_art_diff_seq_stats[,4]))
# Change colnames
colnames(vel_art_diff_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

p_vel_art_seq <- ggplot(data = vel_art_seq_stats, aes(x = Note, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(0.05)) + 
  labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,67,1))
p_vel_art_seq

p_vel_art_diff_seq <- ggplot(data = vel_art_diff_seq_stats, aes(x = Interval, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "Difference") + scale_x_continuous(breaks=seq(1,66,1))
p_vel_art_diff_seq

