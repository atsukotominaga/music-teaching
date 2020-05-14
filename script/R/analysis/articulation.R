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
df_kot <- read.csv("./trimmed/data_kot.csv", header = T, sep = ",", dec = ".") # read a trimmed csv
df_kor <- read.csv("./trimmed/data_kor.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

# SubNr as a factor
df_kot$SubNr <- as.factor(df_kot$SubNr)
df_kor$SubNr <- as.factor(df_kor$SubNr)

# Include only articulation
df_kot_art <- df_kot %>% dplyr::filter(Skill == "articulation")
df_kor_art <- df_kor %>% dplyr::filter(Skill == "articulation")

# Include only dynamics
df_kot_dyn <- df_kot %>% dplyr::filter(Skill == "dynamics")
df_kor_dyn <- df_kor %>% dplyr::filter(Skill == "dynamics")


## ----kot_art, echo = FALSE------------------------------------------------------------------------------------------------
# For each individual
kot_art <- aggregate(KOT~SubNr*Condition*Skill*Subcomponent, data = subset(df_kot_art, df_kot_art$Subcomponent != "LtoS" & df_kot_art$Subcomponent != "StoL"),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kot_art <- cbind(kot_art[,1:4], as.data.frame(kot_art[,5]))
# Change colnames
colnames(kot_art) <- c("SubNr", "Condition", "Skill", "Subcomponent", "N", "Mean", "SD")
print(kot_art)


## ----kot_art_bar,  echo = FALSE-------------------------------------------------------------------------------------------
p_kot_art <- ggplot(data = kot_art, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(.9)) +
  facet_grid(Subcomponent ~ .) +
  labs(y = "KOT (ms)", subtitle = "KOT: Articulation")
p_kot_art


## ----kot_art_all, echo = FALSE--------------------------------------------------------------------------------------------
# Group mean
kot_art_all <- aggregate(Mean~Condition*Skill*Subcomponent, data = kot_art,
                         FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_art_all <- cbind(kot_art_all[,1:3], as.data.frame(kot_art_all[,4]))
# Change colnames
colnames(kot_art_all) <- c("Condition", "Skill", "Subcomponent", "N", "Mean", "SD", "SEM")
print(kot_art_all)


## ----kot_art_all_bar, echo = FALSE----------------------------------------------------------------------------------------
p_kot_art_all <- ggplot(data = kot_art_all, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = .2, position = position_dodge(.9)) +
  labs(y = "KOT (ms)", subtitle = "KOT: Articulation")
p_kot_art_all


## ----kot_art_all_box, echo = FALSE----------------------------------------------------------------------------------------
p_kot_art_all_box <- ggplot(data = kot_art, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, position = position_dodge(0.75)) +
  labs(y = "KOT (ms)", subtitle = "KOT: Articulation")
p_kot_art_all_box


## ----normality1, echo = FALSE---------------------------------------------------------------------------------------------
# Reduce unused factors
kot_art$Subcomponent <- factor(kot_art$Subcomponent)
kot_art_norm <- by(kot_art$Mean, list(kot_art$Condition, kot_art$Subcomponent), shapiro.test)
print(kot_art_norm)


## ----stats1, echo = FALSE-------------------------------------------------------------------------------------------------
kot_art_aov <- ezANOVA(
  data = subset(df_kot_art, df_kot_art$Subcomponent == "Legato" | df_kot_art$Subcomponent == "Staccato")
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , detailed = TRUE
)
kot_art_aov$ANOVA$F <- round(kot_art_aov$ANOVA$F, 4)
kot_art_aov$ANOVA$p <- round(kot_art_aov$ANOVA$p, 4)
print(kot_art_aov$ANOVA)


## ----stats2, echo = FALSE-------------------------------------------------------------------------------------------------
kot_art_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = kot_art)
print(summary(kot_art_aov_2))


## ----post-hoc1, echo = FALSE----------------------------------------------------------------------------------------------
kot_ttest_legato <- t.test(kot_art$Mean[kot_art$Condition == "performing" & kot_art$Subcomponent == "Legato"], kot_art$Mean[kot_art$Condition == "teaching" & kot_art$Subcomponent == "Legato"], paired = TRUE)
kot_ttest_staccato <- t.test(kot_art$Mean[kot_art$Condition == "performing" & kot_art$Subcomponent == "Staccato"], kot_art$Mean[kot_art$Condition == "teaching" & kot_art$Subcomponent == "Staccato"], paired = TRUE)
# Effect size
kot_ttest_legato_cohend <- cohen.d(kot_art$Mean[kot_art$Subcomponent == "Legato"], kot_art$Condition[kot_art$Subcomponent == "Legato"], paired = TRUE)
kot_ttest_staccato_cohend <- cohen.d(kot_art$Mean[kot_art$Subcomponent == "Staccato"], kot_art$Condition[kot_art$Subcomponent == "Staccato"], paired = TRUE)

print(kot_ttest_legato)
print(kot_ttest_staccato)
print(kot_ttest_legato_cohend)
print(kot_ttest_staccato_cohend)


## ----post-hoc2, echo = FALSE----------------------------------------------------------------------------------------------
# Create a group label for a post-hoc test
kot_art$Group[kot_art$Condition == "performing" & kot_art$Subcomponent == "Legato"] <- "Performing-Legato" 
kot_art$Group[kot_art$Condition == "performing" & kot_art$Subcomponent == "Staccato"] <- "Performing-Staccato"
kot_art$Group[kot_art$Condition == "teaching" & kot_art$Subcomponent == "Legato"] <- "Teaching-Legato"
kot_art$Group[kot_art$Condition == "teaching" & kot_art$Subcomponent == "Staccato"] <- "Teaching-Staccato"
# pairwise comparison
kot_art_pairwise <- pairwise.t.test(kot_art$Mean, kot_art$Group, paired = TRUE, p.adjust.method = "BH")
kot_art_pairwise$p.value <- round(kot_art_pairwise$p.value, 4)
print(kot_art_pairwise)


## ----kot_art_trial, echo = FALSE------------------------------------------------------------------------------------------
# For each individual
kot_art_trial <- aggregate(KOT~SubNr*Condition*Skill*Subcomponent*TrialNr, data = subset(df_kot_art, df_kot_art$Subcomponent != "LtoS" & df_kot_art$Subcomponent != "StoL"),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kot_art_trial <- cbind(kot_art_trial[,1:5], as.data.frame(kot_art_trial[,6]))
# Change colnames
colnames(kot_art_trial) <- c("SubNr", "Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD")
print(kot_art_trial)


## ----kot_art_trial_line_legato, echo = FALSE, fig.height = 4--------------------------------------------------------------
p_kot_art_trial_legato <- ggplot(data = subset(kot_art_trial, kot_art_trial$Subcomponent == "Legato"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "KOT (ms)", subtitle = "KOT: Articulation/Legato") + scale_x_continuous(breaks=seq(1,8,1))
p_kot_art_trial_legato


## ----kot_art_trial_line_staccato, echo = FALSE, fig.height = 4------------------------------------------------------------
p_kot_art_trial_staccato <- ggplot(data = subset(kot_art_trial, kot_art_trial$Subcomponent == "Staccato"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "KOT (ms)", subtitle = "KOT: Articulation/Staccato") + scale_x_continuous(breaks=seq(1,8,1))
p_kot_art_trial_staccato


## ----kot_art_trial_all, echo = FALSE--------------------------------------------------------------------------------------
# Group mean
kot_art_trial_all <- aggregate(Mean~Condition*Skill*Subcomponent*TrialNr, data = kot_art_trial,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_art_trial_all <- cbind(kot_art_trial_all[,1:4], as.data.frame(kot_art_trial_all[,5]))
# Change colnames
colnames(kot_art_trial_all) <- c("Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD", "SEM")
print(kot_art_trial_all)


## ----kot_art_trial_all_line_legato, echo = FALSE--------------------------------------------------------------------------
p_kot_art_trial_all_legato <- ggplot(data = subset(kot_art_trial_all, kot_art_trial_all$Subcomponent == "Legato"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "KOT (ms)", subtitle = "KOT: Articulation/Legato") + scale_x_continuous(breaks=seq(1,8,1))
p_kot_art_trial_all_legato


## ----kot_art_trial_all_line_staccato, echo = FALSE------------------------------------------------------------------------
p_kot_art_trial_all_staccato <- ggplot(data = subset(kot_art_trial_all, kot_art_trial_all$Subcomponent == "Staccato"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "KOT (ms)", subtitle = "KOT: Articulation/Staccato") + scale_x_continuous(breaks=seq(1,8,1))
p_kot_art_trial_all_staccato


## ----kor_art, echo = FALSE------------------------------------------------------------------------------------------------
# For each individual
kor_art <- aggregate(KOR~SubNr*Condition*Skill*Subcomponent, data = subset(df_kor_art, df_kor_art$Subcomponent != "LtoS" & df_kor_art$Subcomponent != "StoL"),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kor_art <- cbind(kor_art[,1:4], as.data.frame(kor_art[,5]))
# Change colnames
colnames(kor_art) <- c("SubNr", "Condition", "Skill", "Subcomponent", "N", "Mean", "SD")
print(kor_art)


## ----kor_art_bar,  echo = FALSE-------------------------------------------------------------------------------------------
p_kor_art <- ggplot(data = kor_art, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(.9)) +
  facet_grid(Subcomponent ~ .) +
  labs(y = "KOR", subtitle = "KOR: Articulation")
p_kor_art


## ----kor_art_all, echo = FALSE--------------------------------------------------------------------------------------------
# Group mean
kor_art_all <- aggregate(Mean~Condition*Skill*Subcomponent, data = kor_art,
                         FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kor_art_all <- cbind(kor_art_all[,1:3], as.data.frame(kor_art_all[,4]))
# Change colnames
colnames(kor_art_all) <- c("Condition", "Skill", "Subcomponent", "N", "Mean", "SD", "SEM")
print(kor_art_all)


## ----kor_art_all_bar, echo = FALSE----------------------------------------------------------------------------------------
p_kor_art_all <- ggplot(data = kor_art_all, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = .2, position = position_dodge(.9)) +
  labs(y = "KOR", subtitle = "KOR: Articulation")
p_kor_art_all


## ----kor_art_all_box, echo = FALSE----------------------------------------------------------------------------------------
p_kor_art_all_box <- ggplot(data = kor_art, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, position = position_dodge(0.75)) +
  labs(y = "KOR", subtitle = "KOR: Articulation")
p_kor_art_all_box


## ----normality2, echo = FALSE---------------------------------------------------------------------------------------------
# Reduce unused factors
kor_art$Subcomponent <- factor(kor_art$Subcomponent)
kor_art_norm <- by(kor_art$Mean, list(kor_art$Condition, kor_art$Subcomponent), shapiro.test)
print(kor_art_norm)


## ----stats3, echo = FALSE-------------------------------------------------------------------------------------------------
kor_art_aov <- ezANOVA(
  data = subset(df_kor_art, df_kor_art$Subcomponent == "Legato" | df_kor_art$Subcomponent == "Staccato")
  , dv = .(KOR)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , detailed = TRUE
)
kor_art_aov$ANOVA$F <- round(kor_art_aov$ANOVA$F, 4)
kor_art_aov$ANOVA$p <- round(kor_art_aov$ANOVA$p, 4)
print(kor_art_aov$ANOVA)


## ----stats4, echo = FALSE-------------------------------------------------------------------------------------------------
kor_art_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = kor_art)
print(summary(kor_art_aov_2))


## ----post-hoc3, echo = FALSE----------------------------------------------------------------------------------------------
kor_ttest_legato <- t.test(kor_art$Mean[kor_art$Condition == "performing" & kor_art$Subcomponent == "Legato"], kor_art$Mean[kor_art$Condition == "teaching" & kor_art$Subcomponent == "Legato"], paired = TRUE)
kor_ttest_staccato <- t.test(kor_art$Mean[kor_art$Condition == "performing" & kor_art$Subcomponent == "Staccato"], kor_art$Mean[kor_art$Condition == "teaching" & kor_art$Subcomponent == "Staccato"], paired = TRUE)
# Effect size
kor_ttest_legato_cohend <- cohen.d(kor_art$Mean[kor_art$Subcomponent == "Legato"], kor_art$Condition[kor_art$Subcomponent == "Legato"], paired = TRUE)
kor_ttest_staccato_cohend <- cohen.d(kor_art$Mean[kor_art$Subcomponent == "Staccato"], kor_art$Condition[kor_art$Subcomponent == "Staccato"], paired = TRUE)

print(kor_ttest_legato)
print(kor_ttest_staccato)
print(kor_ttest_legato_cohend)
print(kor_ttest_staccato_cohend)


## ----post-hoc4, echo = FALSE----------------------------------------------------------------------------------------------
# Create a group label for a post-hoc test
kor_art$Group[kor_art$Condition == "performing" & kor_art$Subcomponent == "Legato"] <- "Performing-Legato" 
kor_art$Group[kor_art$Condition == "performing" & kor_art$Subcomponent == "Staccato"] <- "Performing-Staccato"
kor_art$Group[kor_art$Condition == "teaching" & kor_art$Subcomponent == "Legato"] <- "Teaching-Legato"
kor_art$Group[kor_art$Condition == "teaching" & kor_art$Subcomponent == "Staccato"] <- "Teaching-Staccato"
# pairwise comparison
kor_art_pairwise <- pairwise.t.test(kor_art$Mean, kor_art$Group, paired = TRUE, p.adjust.method = "BH")
kor_art_pairwise$p.value <- round(kor_art_pairwise$p.value, 4)
print(kor_art_pairwise)


## ----kor_art_trial, echo = FALSE------------------------------------------------------------------------------------------
# For each individual
kor_art_trial <- aggregate(KOR~SubNr*Condition*Skill*Subcomponent*TrialNr, data = subset(df_kor_art, df_kor_art$Subcomponent != "LtoS" & df_kor_art$Subcomponent != "StoL"),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kor_art_trial <- cbind(kor_art_trial[,1:5], as.data.frame(kor_art_trial[,6]))
# Change colnames
colnames(kor_art_trial) <- c("SubNr", "Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD")
print(kor_art_trial)


## ----kor_art_trial_line_legato, echo = FALSE, fig.height = 4--------------------------------------------------------------
p_kor_art_trial_legato <- ggplot(data = subset(kor_art_trial, kor_art_trial$Subcomponent == "Legato"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "KOR", subtitle = "KOR: Articulation/Legato") + scale_x_continuous(breaks=seq(1,8,1))
p_kor_art_trial_legato


## ----kor_art_trial_line_staccato, echo = FALSE, fig.height = 4------------------------------------------------------------
p_kor_art_trial_staccato <- ggplot(data = subset(kor_art_trial, kor_art_trial$Subcomponent == "Staccato"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "KOR", subtitle = "KOR: Articulation/Staccato") + scale_x_continuous(breaks=seq(1,8,1))
p_kor_art_trial_staccato


## ----kor_art_trial_all, echo = FALSE--------------------------------------------------------------------------------------
# Group mean
kor_art_trial_all <- aggregate(Mean~Condition*Skill*Subcomponent*TrialNr, data = kor_art_trial,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kor_art_trial_all <- cbind(kor_art_trial_all[,1:4], as.data.frame(kor_art_trial_all[,5]))
# Change colnames
colnames(kor_art_trial_all) <- c("Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD", "SEM")
print(kor_art_trial_all)


## ----kor_art_trial_all_line_legato, echo = FALSE--------------------------------------------------------------------------
p_kor_art_trial_all_legato <- ggplot(data = subset(kor_art_trial_all, kor_art_trial_all$Subcomponent == "Legato"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "KOR", subtitle = "KOR: Articulation/Legato") + scale_x_continuous(breaks=seq(1,8,1))
p_kor_art_trial_all_legato


## ----kor_art_trial_all_line_staccato, echo = FALSE------------------------------------------------------------------------
p_kor_art_trial_all_staccato <- ggplot(data = subset(kor_art_trial_all, kor_art_trial_all$Subcomponent == "Staccato"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "KOR", subtitle = "KOR: Articulation/Staccato") + scale_x_continuous(breaks=seq(1,8,1))
p_kor_art_trial_all_staccato


## ----kot_dyn, echo = FALSE------------------------------------------------------------------------------------------------
# For each individual
kot_dyn <- aggregate(KOT~SubNr*Condition*Skill*Subcomponent, data = subset(df_kot_dyn, df_kot_dyn$Subcomponent != "FtoP" & df_kot_dyn$Subcomponent != "PtoF"),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kot_dyn <- cbind(kot_dyn[,1:4], as.data.frame(kot_dyn[,5]))
# Change colnames
colnames(kot_dyn) <- c("SubNr", "Condition", "Skill", "Subcomponent", "N", "Mean", "SD")
print(kot_dyn)


## ----kot_dyn_bar,  echo = FALSE-------------------------------------------------------------------------------------------
p_kot_dyn <- ggplot(data = kot_dyn, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(.9)) +
  facet_grid(Subcomponent ~ .) +
  labs(y = "KOT (ms)", subtitle = "KOT: Dynamics")
p_kot_dyn


## ----kot_dyn_all, echo = FALSE--------------------------------------------------------------------------------------------
# Group mean
kot_dyn_all <- aggregate(Mean~Condition*Skill*Subcomponent, data = kot_dyn,
                         FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_dyn_all <- cbind(kot_dyn_all[,1:3], as.data.frame(kot_dyn_all[,4]))
# Change colnames
colnames(kot_dyn_all) <- c("Condition", "Skill", "Subcomponent", "N", "Mean", "SD", "SEM")
print(kot_dyn_all)


## ----kot_dyn_all_bar, echo = FALSE----------------------------------------------------------------------------------------
p_kot_dyn_all <- ggplot(data = kot_dyn_all, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = .2, position = position_dodge(.9)) +
  labs(y = "KOT (ms)", subtitle = "KOT: Dynamics")
p_kot_dyn_all


## ----kot_dyn_all_box, echo = FALSE----------------------------------------------------------------------------------------
p_kot_dyn_all_box <- ggplot(data = kot_dyn, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, position = position_dodge(0.75)) +
  labs(y = "KOT (ms)", subtitle = "KOT: Dynamics")
p_kot_dyn_all_box


## ----normality3, echo = FALSE---------------------------------------------------------------------------------------------
# Reduce unused factors
kot_dyn$Subcomponent <- factor(kot_dyn$Subcomponent)
kot_dyn_norm <- by(kot_dyn$Mean, list(kot_dyn$Condition, kot_dyn$Subcomponent), shapiro.test)
print(kot_dyn_norm)


## ----stats5, echo = FALSE-------------------------------------------------------------------------------------------------
kot_dyn_aov <- ezANOVA(
  data = subset(df_kot_dyn, df_kot_dyn$Subcomponent == "Forte" | df_kot_dyn$Subcomponent == "Piano")
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , detailed = TRUE
)
kot_dyn_aov$ANOVA$F <- round(kot_dyn_aov$ANOVA$F, 4)
kot_dyn_aov$ANOVA$p <- round(kot_dyn_aov$ANOVA$p, 4)
print(kot_dyn_aov$ANOVA)


## ----stats6, echo = FALSE-------------------------------------------------------------------------------------------------
kot_dyn_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = kot_dyn)
print(summary(kot_dyn_aov_2))


## ----kot_dyn_trial, echo = FALSE------------------------------------------------------------------------------------------
# For each individual
kot_dyn_trial <- aggregate(KOT~SubNr*Condition*Skill*Subcomponent*TrialNr, data = subset(df_kot_dyn, df_kot_dyn$Subcomponent != "FtoP" & df_kot_dyn$Subcomponent != "PtoF"),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kot_dyn_trial <- cbind(kot_dyn_trial[,1:5], as.data.frame(kot_dyn_trial[,6]))
# Change colnames
colnames(kot_dyn_trial) <- c("SubNr", "Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD")
print(kot_dyn_trial)


## ----kot_dyn_trial_line_forte, echo = FALSE, fig.height = 4---------------------------------------------------------------
p_kot_dyn_trial_forte <- ggplot(data = subset(kot_dyn_trial, kot_dyn_trial$Subcomponent == "Forte"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "KOT (ms)", subtitle = "KOT: Dynamics/Forte") + scale_x_continuous(breaks=seq(1,8,1))
p_kot_dyn_trial_forte


## ----kot_dyn_trial_line_piano, echo = FALSE, fig.height = 4---------------------------------------------------------------
p_kot_dyn_trial_piano <- ggplot(data = subset(kot_dyn_trial, kot_dyn_trial$Subcomponent == "Piano"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "KOT (ms)", subtitle = "KOT: Dynamics/Piano") + scale_x_continuous(breaks=seq(1,8,1))
p_kot_dyn_trial_piano


## ----kot_dyn_trial_all, echo = FALSE--------------------------------------------------------------------------------------
# Group mean
kot_dyn_trial_all <- aggregate(Mean~Condition*Skill*Subcomponent*TrialNr, data = kot_dyn_trial,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_dyn_trial_all <- cbind(kot_dyn_trial_all[,1:4], as.data.frame(kot_dyn_trial_all[,5]))
# Change colnames
colnames(kot_dyn_trial_all) <- c("Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD", "SEM")
print(kot_dyn_trial_all)


## ----kot_dyn_trial_all_line_forte, echo = FALSE---------------------------------------------------------------------------
p_kot_dyn_trial_all_forte <- ggplot(data = subset(kot_dyn_trial_all, kot_dyn_trial_all$Subcomponent == "Forte"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "KOT (ms)", subtitle = "KOT: Dynamics/Forte") + scale_x_continuous(breaks=seq(1,8,1))
p_kot_dyn_trial_all_forte


## ----kot_dyn_trial_all_line_piano, echo = FALSE---------------------------------------------------------------------------
p_kot_dyn_trial_all_piano <- ggplot(data = subset(kot_dyn_trial_all, kot_dyn_trial_all$Subcomponent == "Piano"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "KOT (ms)", subtitle = "KOT: Dynamics/Piano") + scale_x_continuous(breaks=seq(1,8,1))
p_kot_dyn_trial_all_piano


## ----kor_dyn, echo = FALSE------------------------------------------------------------------------------------------------
# For each individual
kor_dyn <- aggregate(KOR~SubNr*Condition*Skill*Subcomponent, data = subset(df_kor_dyn, df_kor_dyn$Subcomponent != "FtoP" & df_kor_dyn$Subcomponent != "PtoF"),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kor_dyn <- cbind(kor_dyn[,1:4], as.data.frame(kor_dyn[,5]))
# Change colnames
colnames(kor_dyn) <- c("SubNr", "Condition", "Skill", "Subcomponent", "N", "Mean", "SD")
print(kor_dyn)


## ----kor_dyn_bar,  echo = FALSE-------------------------------------------------------------------------------------------
p_kor_dyn <- ggplot(data = kor_dyn, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(.9)) +
  facet_grid(Subcomponent ~ .) +
  labs(y = "KOR", subtitle = "KOR: Dynamics")
p_kor_dyn


## ----kor_dyn_all, echo = FALSE--------------------------------------------------------------------------------------------
# Group mean
kor_dyn_all <- aggregate(Mean~Condition*Skill*Subcomponent, data = kor_dyn,
                         FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kor_dyn_all <- cbind(kor_dyn_all[,1:3], as.data.frame(kor_dyn_all[,4]))
# Change colnames
colnames(kor_dyn_all) <- c("Condition", "Skill", "Subcomponent", "N", "Mean", "SD", "SEM")
print(kor_dyn_all)


## ----kor_dyn_all_bar, echo = FALSE----------------------------------------------------------------------------------------
p_kor_dyn_all <- ggplot(data = kor_dyn_all, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = .2, position = position_dodge(.9)) +
  labs(y = "KOR", subtitle = "KOR: Dynamics")
p_kor_dyn_all


## ----kor_dyn_all_box, echo = FALSE----------------------------------------------------------------------------------------
p_kor_dyn_all_box <- ggplot(data = kor_dyn, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, position = position_dodge(0.75)) +
  labs(y = "KOR", subtitle = "KOR: Dynamics")
p_kor_dyn_all_box


## ----normality4, echo = FALSE---------------------------------------------------------------------------------------------
# Reduce unused factors
kor_dyn$Subcomponent <- factor(kor_dyn$Subcomponent)
kor_dyn_norm <- by(kor_dyn$Mean, list(kor_dyn$Condition, kor_dyn$Subcomponent), shapiro.test)
print(kor_dyn_norm)


## ----stats7, echo = FALSE-------------------------------------------------------------------------------------------------
kor_dyn_aov <- ezANOVA(
  data = subset(df_kor_dyn, df_kor_dyn$Subcomponent == "Forte" | df_kor_dyn$Subcomponent == "Piano")
  , dv = .(KOR)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , detailed = TRUE
)
kor_dyn_aov$ANOVA$F <- round(kor_dyn_aov$ANOVA$F, 4)
kor_dyn_aov$ANOVA$p <- round(kor_dyn_aov$ANOVA$p, 4)
print(kor_dyn_aov$ANOVA)


## ----stats8, echo = FALSE-------------------------------------------------------------------------------------------------
kor_dyn_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = kor_dyn)
print(summary(kor_dyn_aov_2))


## ----kor_dyn_trial, echo = FALSE------------------------------------------------------------------------------------------
# For each individual
kor_dyn_trial <- aggregate(KOR~SubNr*Condition*Skill*Subcomponent*TrialNr, data = subset(df_kor_dyn, df_kor_dyn$Subcomponent != "FtoP" & df_kor_dyn$Subcomponent != "PtoF"),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kor_dyn_trial <- cbind(kor_dyn_trial[,1:5], as.data.frame(kor_dyn_trial[,6]))
# Change colnames
colnames(kor_dyn_trial) <- c("SubNr", "Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD")
print(kor_dyn_trial)


## ----kor_dyn_trial_line_forte, echo = FALSE, fig.height = 4---------------------------------------------------------------
p_kor_dyn_trial_forte <- ggplot(data = subset(kor_dyn_trial, kor_dyn_trial$Subcomponent == "Forte"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "KOR", subtitle = "KOR: Dynamics/Forte") + scale_x_continuous(breaks=seq(1,8,1))
p_kor_dyn_trial_forte


## ----kor_dyn_trial_line_piano, echo = FALSE, fig.height = 4---------------------------------------------------------------
p_kor_dyn_trial_piano <- ggplot(data = subset(kor_dyn_trial, kor_dyn_trial$Subcomponent == "Piano"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "KOR", subtitle = "KOR: Dynamics/Piano") + scale_x_continuous(breaks=seq(1,8,1))
p_kor_dyn_trial_piano


## ----kor_dyn_trial_all, echo = FALSE--------------------------------------------------------------------------------------
# Group mean
kor_dyn_trial_all <- aggregate(Mean~Condition*Skill*Subcomponent*TrialNr, data = kor_dyn_trial,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kor_dyn_trial_all <- cbind(kor_dyn_trial_all[,1:4], as.data.frame(kor_dyn_trial_all[,5]))
# Change colnames
colnames(kor_dyn_trial_all) <- c("Condition", "Skill", "Subcomponent", "TrialNr", "N", "Mean", "SD", "SEM")
print(kor_dyn_trial_all)


## ----kor_dyn_trial_all_line_forte, echo = FALSE---------------------------------------------------------------------------
p_kor_dyn_trial_all_forte <- ggplot(data = subset(kor_dyn_trial_all, kor_dyn_trial_all$Subcomponent == "Forte"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "KOR", subtitle = "KOR: Dynamics/Forte") + scale_x_continuous(breaks=seq(1,8,1))
p_kor_dyn_trial_all_forte


## ----kor_dyn_trial_all_line_piano, echo = FALSE---------------------------------------------------------------------------
p_kor_dyn_trial_all_piano <- ggplot(data = subset(kor_dyn_trial_all, kor_dyn_trial_all$Subcomponent == "Piano"), aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "KOR", subtitle = "KOR: Dynamics/Piano") + scale_x_continuous(breaks=seq(1,8,1))
p_kor_dyn_trial_all_piano


## ----seq_art, fig.width = 10, fig.height = 2, echo = FALSE----------------------------------------------------------------
# For each individual
kot_art_seq <- aggregate(KOT~SubNr*Condition*Skill*Interval, data = df_kot_art,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kot_art_seq <- cbind(kot_art_seq[,1:4], as.data.frame(kot_art_seq[,5]))
# Change colnames
colnames(kot_art_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# Group mean
kot_art_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = kot_art_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_art_seq_stats <- cbind(kot_art_seq_stats[,1:3], as.data.frame(kot_art_seq_stats[,4]))
# Change colnames for each interval
colnames(kot_art_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

p_kot_art_seq <- ggplot(data = kot_art_seq_stats, aes(x = Interval, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "KOT (ms)") + scale_x_continuous(breaks=seq(1,66,1))

# For each individual
kor_art_seq <- aggregate(KOR~SubNr*Condition*Skill*Interval, data = df_kor_art,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kor_art_seq <- cbind(kor_art_seq[,1:4], as.data.frame(kor_art_seq[,5]))
# Change colnames
colnames(kor_art_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# Group mean
kor_art_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = kor_art_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kor_art_seq_stats <- cbind(kor_art_seq_stats[,1:3], as.data.frame(kor_art_seq_stats[,4]))
# Change colnames for each interval
colnames(kor_art_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

p_kor_art_seq <- ggplot(data = kor_art_seq_stats, aes(x = Interval, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "KOR") + scale_x_continuous(breaks=seq(1,66,1))

p_kot_art_seq
p_kor_art_seq


## ----seq_dyn, fig.width = 10, fig.height = 2, echo = FALSE----------------------------------------------------------------
# For each individual
kot_dyn_seq <- aggregate(KOT~SubNr*Condition*Skill*Interval, data = df_kot_dyn,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kot_dyn_seq <- cbind(kot_dyn_seq[,1:4], as.data.frame(kot_dyn_seq[,5]))
# Change colnames
colnames(kot_dyn_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# Group mean
kot_dyn_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = kot_dyn_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_dyn_seq_stats <- cbind(kot_dyn_seq_stats[,1:3], as.data.frame(kot_dyn_seq_stats[,4]))
# Change colnames for each interval
colnames(kot_dyn_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

p_kot_dyn_seq <- ggplot(data = kot_dyn_seq_stats, aes(x = Interval, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "KOT (ms)") + scale_x_continuous(breaks=seq(1,66,1))

# For each individual
kor_dyn_seq <- aggregate(KOR~SubNr*Condition*Skill*Interval, data = df_kor_dyn,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kor_dyn_seq <- cbind(kor_dyn_seq[,1:4], as.data.frame(kor_dyn_seq[,5]))
# Change colnames
colnames(kor_dyn_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# Group mean
kor_dyn_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = kor_dyn_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kor_dyn_seq_stats <- cbind(kor_dyn_seq_stats[,1:3], as.data.frame(kor_dyn_seq_stats[,4]))
# Change colnames for each interval
colnames(kor_dyn_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

p_kor_dyn_seq <- ggplot(data = kor_dyn_seq_stats, aes(x = Interval, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "KOR") + scale_x_continuous(breaks=seq(1,66,1))

p_kot_dyn_seq
p_kor_dyn_seq

