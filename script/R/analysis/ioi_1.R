## ----setup, include = FALSE---------------------------------------------------
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


## ----extract, include = FALSE-------------------------------------------------
df_ioi <- read.csv("../R/preprocessor/trimmed/data_ioi_1.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

# SubNr as a factor
df_ioi$SubNr <- as.factor(df_ioi$SubNr)

# Include only articulation
df_ioi_art <- df_ioi %>% dplyr::filter(Skill == "articulation")
# Include only dynamics
df_ioi_dyn <- df_ioi %>% dplyr::filter(Skill == "dynamics")


## ----ioi_art, echo = FALSE----------------------------------------------------
# For each individual
ioi_art <- aggregate(normIOI~SubNr*Condition*Skill, data = df_ioi_art,
                 FUN = function(x){c(length(x), mean = mean(x), sd = sd(x))})
ioi_art <- cbind(ioi_art[,1:3], as.data.frame(ioi_art[,4]))
# Change colnames
colnames(ioi_art) <- c("SubNr", "Condition", "Skill", "N", "Mean", "SD")
print(ioi_art)


## ----ioi_art_bar,  echo = FALSE-----------------------------------------------
p_ioi_art <- ggplot(data = ioi_art, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(.9)) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  labs(y = "Normalised IOI", subtitle = "IOI: Articulation")
p_ioi_art


## ----ioi_art_all, echo = FALSE------------------------------------------------
# Group mean
ioi_art_all <- aggregate(Mean~Condition*Skill, data = ioi_art,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_art_all <- cbind(ioi_art_all[,1:2], as.data.frame(ioi_art_all[,3]))
# Change colnames
colnames(ioi_art_all) <- c("Condition", "Skill", "N", "Mean", "SD", "SEM")
print(ioi_art_all)


## ----ioi_art_all_bar, echo = FALSE--------------------------------------------
p_ioi_art_all <- ggplot(data = ioi_art_all, aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  labs(y = "Normalised IOI", subtitle = "IOI: Articulation") + coord_cartesian(ylim = c(.75, 1.1)) +
  theme(legend.position = "None")
p_ioi_art_all


## ----ioi_art_all_box, echo = FALSE--------------------------------------------
p_ioi_art_all_box <- ggplot(data = ioi_art, aes(x = Condition, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  labs(y = "Normalised IOI", subtitle = "IOI: Articulation") +
  theme(legend.position = "None")
p_ioi_art_all_box


## ----normality1, echo = FALSE-------------------------------------------------
# Normality check
ioi_art_norm <- by(ioi_art$Mean, list(ioi_art$Condition), shapiro.test)
print(ioi_art_norm)


## ----stats1, echo = FALSE-----------------------------------------------------
ioi_art_ttest <- t.test(ioi_art$Mean[ioi_art$Condition == "teaching"], ioi_art$Mean[ioi_art$Condition == "performing"], paired = TRUE)
print(ioi_art_ttest)


## ----stats2, echo = FALSE-----------------------------------------------------
ioi_art_cohend <- cohen.d(ioi_art$Mean, ioi_art$Condition, paired = TRUE)
print(ioi_art_cohend)


## ----ioi_art_ch, echo = FALSE-------------------------------------------------
# Define phrase boundaries
df_ioi_art$Boundary <- "No"
df_ioi_art$Boundary[df_ioi_art$Subcomponent == "LtoS" | df_ioi_art$Subcomponent == "StoL" |
                      df_ioi_art$Subcomponent == "FtoP" | df_ioi_art$Subcomponent == "PtoF"] <- "Yes"

# For each individual
ioi_art_ch <- aggregate(normIOI~SubNr*Condition*Skill*Boundary, data = df_ioi_art,
                    FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
ioi_art_ch <- cbind(ioi_art_ch[,1:4], as.data.frame(ioi_art_ch[,5]))
# Change colnames
colnames(ioi_art_ch) <- c("SubNr", "Condition", "Skill", "Boundary", "N", "Mean", "SD")
print(ioi_art_ch)


## ----ioi_art_ch_bar, echo = FALSE---------------------------------------------
p_ioi_art_ch <- ggplot(data = ioi_art_ch, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(.9)) +
  facet_grid(Boundary ~ .) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  labs(y = "Normalised IOI", subtitle = "IOI: Articulation")
p_ioi_art_ch


## ----ioi_art_ch_all, echo = FALSE---------------------------------------------
# Group mean
ioi_art_ch_all <- aggregate(Mean~Condition*Skill*Boundary, data = ioi_art_ch,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_art_ch_all <- cbind(ioi_art_ch_all[,1:3], as.data.frame(ioi_art_ch_all[,4]))
# Change colnames
colnames(ioi_art_ch_all) <- c("Condition", "Skill", "Boundary", "N", "Mean", "SD", "SEM")
print(ioi_art_ch_all)


## ----ioi_art_ch_all_bar, echo = FALSE-----------------------------------------
p_ioi_art_ch_all_bar <- ggplot(data = ioi_art_ch_all, aes(x = Boundary, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  labs(y = "Normalised IOI", subtitle = "IOI: Articulation") + coord_cartesian(ylim = c(.75, 1.1))
p_ioi_art_ch_all_bar


## ----ioi_art_ch_all_box, echo = FALSE-----------------------------------------
p_ioi_art_ch_box <- ggplot(data = ioi_art_ch, aes(x = Boundary, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, position = position_dodge(0.75)) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  labs(y = "Normalised IOI", subtitle = "IOI: Articulation")
p_ioi_art_ch_box


## ----normality2, echo = FALSE-------------------------------------------------
ioi_art_ch_norm <- by(ioi_art_ch$Mean, list(ioi_art_ch$Condition, ioi_art_ch$Boundary), shapiro.test)
print(ioi_art_ch_norm)


## ----stats3, echo = FALSE-----------------------------------------------------
ioi_art_ch_aov <- ezANOVA(
  data = df_ioi_art
  , dv = .(normIOI)
  , wid = .(SubNr)
  , within = .(Condition, Boundary)
  , type = 3
  , detailed = TRUE
)
ioi_art_ch_aov$ANOVA$F <- round(ioi_art_ch_aov$ANOVA$F, 4)
ioi_art_ch_aov$ANOVA$p <- round(ioi_art_ch_aov$ANOVA$p, 4)
print(ioi_art_ch_aov$ANOVA)


## ----stats4, echo = FALSE-----------------------------------------------------
ioi_art_ch_aov_2 <- aov(Mean ~ Condition*Boundary + Error(SubNr/(Condition*Boundary)), data = ioi_art_ch)
print(summary(ioi_art_ch_aov_2))


## ----ioi_art_trial, echo = FALSE----------------------------------------------
# For each individual
ioi_art_trial <- aggregate(normIOI~SubNr*Condition*Skill*TrialNr, data = df_ioi_art,
                    FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
ioi_art_trial <- cbind(ioi_art_trial[,1:4], as.data.frame(ioi_art_trial[,5]))
# Change colnames
colnames(ioi_art_trial) <- c("SubNr", "Condition", "Skill", "TrialNr", "N", "Mean", "SD")
print(ioi_art_trial)


## ----ioi_art_trial_line, echo = FALSE, fig.height = 4-------------------------
p_ioi_art_trial <- ggplot(data = ioi_art_trial, aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "Normalised IOI", subtitle = "IOI: Articulation") + scale_x_continuous(breaks=seq(1,8,1))
p_ioi_art_trial


## ----ioi_art_trial_all, echo = FALSE------------------------------------------
# Group mean
ioi_art_trial_all <- aggregate(Mean~Condition*Skill*TrialNr, data = ioi_art_trial,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_art_trial_all <- cbind(ioi_art_trial_all[,1:3], as.data.frame(ioi_art_trial_all[,4]))
# Change colnames
colnames(ioi_art_trial_all) <- c("Condition", "Skill", "TrialNr", "N", "Mean", "SD", "SEM")
print(ioi_art_trial_all)


## ----ioi_art_trial_all_line, echo = FALSE-------------------------------------
p_ioi_art_trial_all <- ggplot(data = ioi_art_trial_all, aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "Normalised IOI", subtitle = "IOI: Articulation") + scale_x_continuous(breaks=seq(1,8,1))
p_ioi_art_trial_all


## ----ioi_art_var, echo = FALSE------------------------------------------------
# Create a data frame for variability (SD/)
df_ioi_art_var <- data.frame()
for (subnr in unique(df_ioi_art$SubNr)){
  for (block in unique(df_ioi_art$BlockNr)){
    cond = as.character(unique(df_ioi_art$Condition[df_ioi_art$SubNr == subnr & df_ioi_art$BlockNr == block]))
    skill = as.character(unique(df_ioi_art$Skill[df_ioi_art$SubNr == subnr & df_ioi_art$BlockNr == block]))
    for (trial in unique(df_ioi_art$TrialNr)){
      df_current <- df_ioi_art %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      if (nrow(df_current) != 0){
        df_ioi_art_var <- rbind(df_ioi_art_var, 
                                data.frame(subnr, block, trial, cond, skill, sd(df_current$IOI)/mean(df_current$IOI)))
      }
    }
  }
}
colnames(df_ioi_art_var) <- c("SubNr", "BlockNr", "TrialNr", "Condition", "Skill", "Variability")

# Average variability
# For each individual
ioi_art_var <- aggregate(Variability~SubNr*Condition*Skill, data = df_ioi_art_var,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
ioi_art_var <- cbind(ioi_art_var[,1:3], as.data.frame(ioi_art_var[,4]))
# Change colnames
colnames(ioi_art_var) <- c("SubNr", "Condition", "Skill", "N", "Mean", "SD")
print(ioi_art_var)


## ----ioi_art_var_bar, echo = FALSE--------------------------------------------
p_ioi_art_var <- ggplot(data = ioi_art_var, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(1)) +
  labs(y = "Variability (SD/mean IOI)", subtitle = "IOI: Articulation")
p_ioi_art_var


## ----ioi_art_var_all, echo = FALSE--------------------------------------------
# Group mean
ioi_art_var_all <- aggregate(Mean~Condition*Skill, data = ioi_art_var,
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_art_var_all <- cbind(ioi_art_var_all[,1:2], as.data.frame(ioi_art_var_all[,3]))
# Change colnames
colnames(ioi_art_var_all) <- c("Condition", "Skill", "N", "Mean", "SD", "SEM")
print(ioi_art_var_all)


## ----ioi_art_var_all_bar, echo = FALSE----------------------------------------
p_ioi_art_var_all <- ggplot(data = ioi_art_var_all, aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = .2, position = position_dodge()) +
  labs(y = "Variability (SD/mean IOI)", subtitle = "IOI: Articulation") +
  theme(legend.position = "None")
p_ioi_art_var_all


## ----ioi_art_var_all_box, echo = FALSE----------------------------------------
p_ioi_art_var_box <- ggplot(data = ioi_art_var, aes(x = Condition, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1) +
  labs(y = "Variability (SD/mean IOI)", subtitle = "IOI: Articulation") +
  theme(legend.position = "None")
p_ioi_art_var_box


## ----normality3, echo = FALSE-------------------------------------------------
ioi_art_var_norm <- by(ioi_art_var$Mean, list(ioi_art_var$Condition), shapiro.test)
print(ioi_art_var_norm)


## ----stats5, echo = FALSE-----------------------------------------------------
ioi_art_var_ttest <- t.test(ioi_art_var$Mean[ioi_art_var$Condition == "teaching"], ioi_art_var$Mean[ioi_art_var$Condition == "performing"], paired = TRUE)
print(ioi_art_var_ttest)


## ----stats6, echo = FALSE-----------------------------------------------------
ioi_art_var_cohend <- cohen.d(ioi_art_var$Mean, ioi_art_var$Condition, paired = TRUE)
print(ioi_art_var_cohend)


## ----ioi_art_var_all_15, echo = FALSE-----------------------------------------
# Group mean
ioi_art_var_all_15 <- aggregate(Mean~Condition*Skill, data = subset(ioi_art_var, ioi_art_var$SubNr != 15),
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_art_var_all_15 <- cbind(ioi_art_var_all_15[,1:2], as.data.frame(ioi_art_var_all_15[,3]))
# Change colnames
colnames(ioi_art_var_all_15) <- c("Condition", "Skill", "N", "Mean", "SD", "SEM")
print(ioi_art_var_all_15)


## ----ioi_art_var_all_bar_15, echo = FALSE-------------------------------------
p_ioi_art_var_all_15 <- ggplot(data = ioi_art_var_all_15, aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = .2, position = position_dodge()) +
  labs(y = "Variability (SD/mean IOI)", subtitle = "IOI: Articulation") +
  theme(legend.position = "None")
p_ioi_art_var_all_15


## ----ioi_art_var_all_box_15, echo = FALSE-------------------------------------
p_ioi_art_var_box_15 <- ggplot(data = subset(ioi_art_var, ioi_art_var$SubNr != 15), aes(x = Condition, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1) +
  labs(y = "Variability (SD/mean IOI)", subtitle = "IOI: Articulation") +
  theme(legend.position = "None")
p_ioi_art_var_box_15


## ----normality3_15, echo = FALSE----------------------------------------------
ioi_art_var_norm_15 <- by(subset(ioi_art_var, ioi_art_var$SubNr != 15)$Mean, list(subset(ioi_art_var, ioi_art_var$SubNr != 15)$Condition), shapiro.test)
print(ioi_art_var_norm_15)


## ----stats5_15, echo = FALSE--------------------------------------------------
ioi_art_var_ttest_15 <- t.test(subset(ioi_art_var, ioi_art_var$SubNr != 15)$Mean[subset(ioi_art_var, ioi_art_var$SubNr != 15)$Condition == "teaching"], subset(ioi_art_var, ioi_art_var$SubNr != 15)$Mean[subset(ioi_art_var, ioi_art_var$SubNr != 15)$Condition == "performing"], paired = TRUE)
print(ioi_art_var_ttest_15)


## ----stats6_15, echo = FALSE--------------------------------------------------
ioi_art_var_cohend_15 <- cohen.d(subset(ioi_art_var, ioi_art_var$SubNr != 15)$Mean, subset(ioi_art_var, ioi_art_var$SubNr != 15)$Condition, paired = TRUE)
print(ioi_art_var_cohend_15)


## ----ioi_art_var_trial, echo = FALSE------------------------------------------
# For each individual
ioi_art_var_trial <- aggregate(Variability~SubNr*Condition*Skill*TrialNr, data = df_ioi_art_var,
                         FUN = function(x){c(N = length(x), mean = mean(x))})
ioi_art_var_trial <- cbind(ioi_art_var_trial[,1:4], as.data.frame(ioi_art_var_trial[,5]))
# Change colnames
colnames(ioi_art_var_trial) <- c("SubNr", "Condition", "Skill", "TrialNr", "N", "Mean")
print(ioi_art_var_trial)


## ----ioi_art_var_trial_line, echo = FALSE, fig.height = 4---------------------
p_ioi_art_var_trial <- ggplot(data = ioi_art_var_trial, aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "Variability (SD/mean IOI)", subtitle = "IOI: Articulation") + scale_x_continuous(breaks=seq(1,8,1))
p_ioi_art_var_trial


## ----ioi_art_var_trial_all, echo = FALSE--------------------------------------
# Group mean
ioi_art_var_trial_all <- aggregate(Mean~Condition*Skill*TrialNr, data = ioi_art_var_trial,
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_art_var_trial_all <- cbind(ioi_art_var_trial_all[,1:3], as.data.frame(ioi_art_var_trial_all[,4]))
# Change colnames
colnames(ioi_art_var_trial_all) <- c("Condition", "Skill", "TrialNr", "N", "Mean", "SD", "SEM")
print(ioi_art_var_trial_all)


## ----ioi_art_var_trial_all_line, echo = FALSE---------------------------------
p_ioi_art_var_trial_all <- ggplot(data = ioi_art_var_trial_all, 
                                  aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "Variability (SD/mean IOI)", subtitle = "IOI: Articulation") + scale_x_continuous(breaks=seq(1,8,1))
p_ioi_art_var_trial_all


## ----ioi_dyn, echo = FALSE----------------------------------------------------
# For each individual
ioi_dyn <- aggregate(normIOI~SubNr*Condition*Skill, data = df_ioi_dyn,
                 FUN = function(x){c(length(x), mean = mean(x), sd = sd(x))})
ioi_dyn <- cbind(ioi_dyn[,1:3], as.data.frame(ioi_dyn[,4]))
# Change colnames
colnames(ioi_dyn) <- c("SubNr", "Condition", "Skill", "N", "Mean", "SD")
print(ioi_dyn)


## ----ioi_dyn_bar,  echo = FALSE-----------------------------------------------
p_ioi_dyn <- ggplot(data = ioi_dyn, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(.9)) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  labs(y = "Normalised IOI", subtitle = "IOI: Dynamics")
p_ioi_dyn


## ----ioi_dyn_all, echo = FALSE------------------------------------------------
# Group mean
ioi_dyn_all <- aggregate(Mean~Condition*Skill, data = ioi_dyn,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_dyn_all <- cbind(ioi_dyn_all[,1:2], as.data.frame(ioi_dyn_all[,3]))
# Change colnames
colnames(ioi_dyn_all) <- c("Condition", "Skill", "N", "Mean", "SD", "SEM")
print(ioi_dyn_all)


## ----ioi_dyn_all_bar, echo = FALSE--------------------------------------------
p_ioi_dyn_all_bar <- ggplot(data = ioi_dyn_all, aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo + 
  labs(y = "Normalised IOI", subtitle = "IOI: Dynamics") +  coord_cartesian(ylim = c(.75, 1.1)) +
  theme(legend.position = "None")
p_ioi_dyn_all_bar


## ----ioi_dyn_all_box, echo = FALSE--------------------------------------------
p_ioi_dyn_all_box <- ggplot(data = ioi_dyn, aes(x = Condition, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  labs(y = "Normalised IOI", subtitle = "IOI: Dynamics") + 
  theme(legend.position = "None")
p_ioi_dyn_all_box


## ----normality4, echo = FALSE-------------------------------------------------
# Normality check
ioi_dyn_norm <- by(ioi_dyn$Mean, list(ioi_dyn$Condition), shapiro.test)
print(ioi_dyn_norm)


## ----stats7, echo = FALSE-----------------------------------------------------
ioi_dyn_ttest <- t.test(ioi_dyn$Mean[ioi_dyn$Condition == "teaching"], ioi_dyn$Mean[ioi_dyn$Condition == "performing"], paired = TRUE)
print(ioi_dyn_ttest)


## ----stats8, echo = FALSE-----------------------------------------------------
ioi_dyn_cohend <- cohen.d(ioi_dyn$Mean, ioi_dyn$Condition, paired = TRUE)
print(ioi_dyn_cohend)


## ----ioi_dyn_ch, echo = FALSE-------------------------------------------------
df_ioi_dyn$Boundary <- "No"
df_ioi_dyn$Boundary[df_ioi_dyn$Subcomponent == "LtoS" | df_ioi_dyn$Subcomponent == "StoL" |
                      df_ioi_dyn$Subcomponent == "FtoP" | df_ioi_dyn$Subcomponent == "PtoF"] <- "Yes"

# For each individual
ioi_dyn_ch <- aggregate(normIOI~SubNr*Condition*Skill*Boundary, data = df_ioi_dyn,
                    FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
ioi_dyn_ch <- cbind(ioi_dyn_ch[,1:4], as.data.frame(ioi_dyn_ch[,5]))
# Change colnames
colnames(ioi_dyn_ch) <- c("SubNr", "Condition", "Skill", "Boundary", "N", "Mean", "SD")
print(ioi_dyn_ch)


## ----ioi_dyn_ch_bar, echo = FALSE---------------------------------------------
p_ioi_dyn_ch <- ggplot(data = ioi_dyn_ch, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(.9)) +
  facet_grid(Boundary ~ .) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  labs(y = "Normalised IOI", subtitle = "IOI: Dynamics")
p_ioi_dyn_ch


## ----ioi_dyn_ch_all, echo = FALSE---------------------------------------------
# Group mean
ioi_dyn_ch_all <- aggregate(Mean~Condition*Skill*Boundary, data = ioi_dyn_ch,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_dyn_ch_all <- cbind(ioi_dyn_ch_all[,1:3], as.data.frame(ioi_dyn_ch_all[,4]))
# Change colnames
colnames(ioi_dyn_ch_all) <- c("Condition", "Skill", "Boundary", "N", "Mean", "SD", "SEM")
print(ioi_dyn_ch_all)


## ----ioi_dyn_ch_all_bar, echo = FALSE-----------------------------------------
p_ioi_dyn_ch_all_bar <- ggplot(data = ioi_dyn_ch_all, aes(x = Boundary, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  labs(y = "Normalised IOI", subtitle = "IOI: Dynamics") + coord_cartesian(ylim = c(.75, 1.1))
p_ioi_dyn_ch_all_bar


## ----ioi_dyn_ch_all_box, echo = FALSE-----------------------------------------
p_ioi_dyn_ch_box <- ggplot(data = ioi_dyn_ch, aes(x = Boundary, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1, position = position_dodge(0.75)) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  labs(y = "Normalised IOI", subtitle = "normIOI: Dynamics")
p_ioi_dyn_ch_box


## ----normality5, echo = FALSE-------------------------------------------------
ioi_dyn_ch_norm <- by(ioi_dyn_ch$Mean, list(ioi_dyn_ch$Condition, ioi_dyn_ch$Boundary), shapiro.test)
print(ioi_dyn_ch_norm)


## ----stats9, echo = FALSE-----------------------------------------------------
ioi_dyn_ch_aov <- ezANOVA(
  data = df_ioi_dyn
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, Boundary)
  , type = 3
  , detailed = TRUE
)
ioi_dyn_ch_aov$ANOVA$F <- round(ioi_dyn_ch_aov$ANOVA$F, 4)
ioi_dyn_ch_aov$ANOVA$p <- round(ioi_dyn_ch_aov$ANOVA$p, 4)
print(ioi_dyn_ch_aov)


## ----stats10, echo = FALSE----------------------------------------------------
ioi_dyn_ch_aov_2 <- aov(Mean ~ Condition*Boundary + Error(SubNr/(Condition*Boundary)), data = ioi_dyn_ch)
print(summary(ioi_dyn_ch_aov_2))


## ----ioi_dyn_trial, echo = FALSE----------------------------------------------
# For each individual
ioi_dyn_trial <- aggregate(normIOI~SubNr*Condition*Skill*TrialNr, data = df_ioi_dyn,
                    FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
ioi_dyn_trial <- cbind(ioi_dyn_trial[,1:4], as.data.frame(ioi_dyn_trial[,5]))
# Change colnames
colnames(ioi_dyn_trial) <- c("SubNr", "Condition", "Skill", "TrialNr", "N", "Mean", "SD")
print(ioi_dyn_trial)


## ----ioi_dyn_trial_line, echo = FALSE, fig.height = 4-------------------------
p_ioi_dyn_trial <- ggplot(data = ioi_dyn_trial, aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "Normalised IOI", subtitle = "IOI: Dynamics") + scale_x_continuous(breaks=seq(1,8,1))
p_ioi_dyn_trial


## ----ioi_dyn_trial_all, echo = FALSE------------------------------------------
# Group mean
ioi_dyn_trial_all <- aggregate(Mean~Condition*Skill*TrialNr, data = ioi_dyn_trial,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_dyn_trial_all <- cbind(ioi_dyn_trial_all[,1:3], as.data.frame(ioi_dyn_trial_all[,4]))
# Change colnames
colnames(ioi_dyn_trial_all) <- c("Condition", "Skill", "TrialNr", "N", "Mean", "SD", "SEM")
print(ioi_dyn_trial_all)


## ----ioi_dyn_trial_all_line, echo = FALSE-------------------------------------
p_ioi_dyn_trial_all <- ggplot(data = ioi_dyn_trial_all, aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "Normalised IOI", subtitle = "IOI: Dynamics") + scale_x_continuous(breaks=seq(1,8,1))
p_ioi_dyn_trial_all


## ----ioi_dyn_var, echo = FALSE------------------------------------------------
# Create a data frame for variability (SD/)
df_ioi_dyn_var <- data.frame()
for (subnr in unique(df_ioi_dyn$SubNr)){
  for (block in unique(df_ioi_dyn$BlockNr)){
    cond = as.character(unique(df_ioi_dyn$Condition[df_ioi_dyn$SubNr == subnr & df_ioi_dyn$BlockNr == block]))
    skill = as.character(unique(df_ioi_dyn$Skill[df_ioi_dyn$SubNr == subnr & df_ioi_dyn$BlockNr == block]))
    for (trial in unique(df_ioi_dyn$TrialNr)){
      df_current <- df_ioi_dyn %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      if (nrow(df_current) != 0){
        df_ioi_dyn_var <- rbind(df_ioi_dyn_var, 
                                data.frame(subnr, block, trial, cond, skill, sd(df_current$IOI)/mean(df_current$IOI)))
      }
    }
  }
}
colnames(df_ioi_dyn_var) <- c("SubNr", "BlockNr", "TrialNr", "Condition", "Skill", "Variability")

# Average variability
# For each individual
ioi_dyn_var <- aggregate(Variability~SubNr*Condition*Skill, data = df_ioi_dyn_var,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
ioi_dyn_var <- cbind(ioi_dyn_var[,1:3], as.data.frame(ioi_dyn_var[,4]))
# Change colnames
colnames(ioi_dyn_var) <- c("SubNr", "Condition", "Skill", "N", "Mean", "SD")
print(ioi_dyn_var)


## ----ioi_dyn_var_bar, echo = FALSE--------------------------------------------
p_ioi_dyn_var <- ggplot(data = ioi_dyn_var, aes(x = SubNr, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = .2, position = position_dodge(1)) +
  labs(y = "Variability (SD/mean IOI)", subtitle = "IOI: Dynamics")
p_ioi_dyn_var


## ----ioi_dyn_var_all, echo = FALSE--------------------------------------------
# Group mean
ioi_dyn_var_all <- aggregate(Mean~Condition*Skill, data = ioi_dyn_var,
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_dyn_var_all <- cbind(ioi_dyn_var_all[,1:2], as.data.frame(ioi_dyn_var_all[,3]))
# Change colnames
colnames(ioi_dyn_var_all) <- c("Condition", "Skill", "N", "Mean", "SD", "SEM")
print(ioi_dyn_var_all)


## ----ioi_dyn_var_all_bar, echo = FALSE----------------------------------------
p_ioi_dyn_var_all <- ggplot(data = ioi_dyn_var_all, aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = .2, position = position_dodge()) +
  labs(y = "Variability (SD/mean IOI)", subtitle = "IOI: Dynamics") +
  theme(legend.position = "None")
p_ioi_dyn_var_all


## ----ioi_dyn_var_all_box, echo = FALSE----------------------------------------
p_ioi_dyn_var_box <- ggplot(data = ioi_dyn_var, aes(x = Condition, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1) +
  labs(y = "Variability (SD/mean IOI)", subtitle = "IOI: Dynamics") +
  theme(legend.position = "None")
p_ioi_dyn_var_box


## ----normality6, echo = FALSE-------------------------------------------------
ioi_dyn_var_norm <- by(ioi_dyn_var$Mean, list(ioi_dyn_var$Condition), shapiro.test)
print(ioi_dyn_var_norm)


## ----stats11, echo = FALSE----------------------------------------------------
ioi_dyn_var_ttest <- t.test(ioi_dyn_var$Mean[ioi_dyn_var$Condition == "teaching"], ioi_dyn_var$Mean[ioi_dyn_var$Condition == "performing"], paired = TRUE)
print(ioi_dyn_var_ttest)


## ----stats12, echo = FALSE----------------------------------------------------
ioi_dyn_var_cohend <- cohen.d(ioi_dyn_var$Mean, ioi_dyn_var$Condition, paired = TRUE)
print(ioi_dyn_var_cohend)


## ----ioi_dyn_var_all_15, echo = FALSE-----------------------------------------
# Group mean
ioi_dyn_var_all_15 <- aggregate(Mean~Condition*Skill, data = subset(ioi_dyn_var, ioi_dyn_var$SubNr != 15),
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_dyn_var_all_15 <- cbind(ioi_dyn_var_all_15[,1:2], as.data.frame(ioi_dyn_var_all_15[,3]))
# Change colnames
colnames(ioi_dyn_var_all_15) <- c("Condition", "Skill", "N", "Mean", "SD", "SEM")
print(ioi_dyn_var_all_15)


## ----ioi_dyn_var_all_bar_15, echo = FALSE-------------------------------------
p_ioi_dyn_var_all_15 <- ggplot(data = ioi_dyn_var_all_15, aes(x = Condition, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width = .2, position = position_dodge()) +
  labs(y = "Variability (SD/mean IOI)", subtitle = "IOI: Dynamics") +
  theme(legend.position = "None")
p_ioi_dyn_var_all_15


## ----ioi_dyn_var_all_box_15, echo = FALSE-------------------------------------
p_ioi_dyn_var_box_15 <- ggplot(data = subset(ioi_dyn_var, ioi_dyn_var$SubNr != 15), aes(x = Condition, y = Mean, fill = Condition)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 1) +
  labs(y = "Variability (SD/mean IOI)", subtitle = "IOI: Dynamics") +
  theme(legend.position = "None")
p_ioi_dyn_var_box_15


## ----normality6_15, echo = FALSE----------------------------------------------
ioi_dyn_var_norm_15 <- by(subset(ioi_dyn_var, ioi_dyn_var$SubNr != 15)$Mean, list(subset(ioi_dyn_var, ioi_dyn_var$SubNr != 15)$Condition), shapiro.test)
print(ioi_dyn_var_norm_15)


## ----stats11_15, echo = FALSE-------------------------------------------------
ioi_dyn_var_ttest_15 <- t.test(subset(ioi_dyn_var, ioi_dyn_var$SubNr != 15)$Mean[subset(ioi_dyn_var, ioi_dyn_var$SubNr != 15)$Condition == "teaching"], subset(ioi_dyn_var, ioi_dyn_var$SubNr != 15)$Mean[subset(ioi_dyn_var, ioi_dyn_var$SubNr != 15)$Condition == "performing"], paired = TRUE)
print(ioi_dyn_var_ttest_15)


## ----stats12_15, echo = FALSE-------------------------------------------------
ioi_dyn_var_cohend_15 <- cohen.d(subset(ioi_dyn_var, ioi_dyn_var$SubNr != 15)$Mean, subset(ioi_dyn_var, ioi_dyn_var$SubNr != 15)$Condition, paired = TRUE)
print(ioi_dyn_var_cohend_15)


## ----ioi_dyn_var_trial, echo = FALSE------------------------------------------
# For each individual
ioi_dyn_var_trial <- aggregate(Variability~SubNr*Condition*Skill*TrialNr, data = df_ioi_dyn_var,
                         FUN = function(x){c(N = length(x), mean = mean(x))})
ioi_dyn_var_trial <- cbind(ioi_dyn_var_trial[,1:4], as.data.frame(ioi_dyn_var_trial[,5]))
# Change colnames
colnames(ioi_dyn_var_trial) <- c("SubNr", "Condition", "Skill", "TrialNr", "N", "Mean")
print(ioi_dyn_var_trial)


## ----ioi_dyn_var_trial_line, echo = FALSE, fig.height = 4---------------------
p_ioi_dyn_var_trial <- ggplot(data = ioi_dyn_var_trial, aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(SubNr ~ .) + 
  labs(x = "Trial", y = "Variability (SD/mean IOI)", subtitle = "IOI: Dynamics") + scale_x_continuous(breaks=seq(1,8,1))
p_ioi_dyn_var_trial


## ----ioi_dyn_var_trial_all, echo = FALSE--------------------------------------
# Group mean
ioi_dyn_var_trial_all <- aggregate(Mean~Condition*Skill*TrialNr, data = ioi_dyn_var_trial,
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_dyn_var_trial_all <- cbind(ioi_dyn_var_trial_all[,1:3], as.data.frame(ioi_dyn_var_trial_all[,4]))
# Change colnames
colnames(ioi_dyn_var_trial_all) <- c("Condition", "Skill", "TrialNr", "N", "Mean", "SD", "SEM")
print(ioi_dyn_var_trial_all)


## ----ioi_dyn_var_trial_all_line, echo = FALSE---------------------------------
p_ioi_dyn_var_trial_all <- ggplot(data = ioi_dyn_var_trial_all, 
                                  aes(x = TrialNr, y = Mean, group = Condition, colour = Condition)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(position = position_dodge(.2)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = .2, position = position_dodge(.2)) + 
  labs(x = "Trial", y = "Variability (SD/mean IOI)", subtitle = "IOI: Dynamics") + scale_x_continuous(breaks=seq(1,8,1))
p_ioi_dyn_var_trial_all


## ----seq_art, fig.width = 10, fig.height = 2, echo = FALSE--------------------
# For each individual
ioi_art_seq <- aggregate(normIOI~SubNr*Condition*Skill*Interval, data = df_ioi_art,
                                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
ioi_art_seq <- cbind(ioi_art_seq[,1:4], as.data.frame(ioi_art_seq[,5]))
# Change colnames
colnames(ioi_art_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# Group mean
ioi_art_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = ioi_art_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_art_seq_stats <- cbind(ioi_art_seq_stats[,1:3], as.data.frame(ioi_art_seq_stats[,4]))
# Change colnames
colnames(ioi_art_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

p_ioi_art_seq <- ggplot(data = ioi_art_seq_stats, aes(x = Interval, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "Normalised IOI") + scale_x_continuous(breaks=seq(1,71,1))
p_ioi_art_seq


## ----seq_dyn, fig.width = 10, fig.height = 2, echo = FALSE--------------------
# For each individual
ioi_dyn_seq <- aggregate(normIOI~SubNr*Condition*Skill*Interval, data = df_ioi_dyn,
                                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
ioi_dyn_seq <- cbind(ioi_dyn_seq[,1:4], as.data.frame(ioi_dyn_seq[,5]))
# Change colnames
colnames(ioi_dyn_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# Group mean
ioi_dyn_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = ioi_dyn_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_dyn_seq_stats <- cbind(ioi_dyn_seq_stats[,1:3], as.data.frame(ioi_dyn_seq_stats[,4]))
# Change colnames
colnames(ioi_dyn_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

p_ioi_dyn_seq <- ggplot(data = ioi_dyn_seq_stats, aes(x = Interval, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed") + # Tempo
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "Normalised IOI") + scale_x_continuous(breaks=seq(1,71,1))
p_ioi_dyn_seq

