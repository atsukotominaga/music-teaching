#!/usr/bin/Rscript
rm(list=ls(all=TRUE)) # clear all in Environment

####################################
#  Documentation
####################################
# Created: 26/02/2019
# This script aggregates, plots data (IOI) and runs statistical tests
# GitHub repo (private): https://github.com/atsukotominaga/teaching_v1.0/script/R 

####################################
#  Requirements
####################################
# Install and load required packages
# data manipulation
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
# plot
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
# statistics
if (!require("stats")) {install.packages("stats"); require("stats")}
if (!require("ez")) {install.packages("ez"); require("ez")}

# Create necessary folders if not exist
# 3_stats
if (!file.exists("3_stats")){
  dir.create("3_stats")
}

# 3_stats/ioi - store csv files
if (!file.exists("3_stats/ioi")){
  dir.create("3_stats/ioi")
}

# 3_stats/plot
if (!file.exists("3_stats/plot")){
  dir.create("3_stats/plot")
}
# 3_stats/ioi - store png files
if (!file.exists("3_stats/plot/ioi")){
  dir.create("3_stats/plot/ioi")
}

####################################
# Reading and formatting data
####################################
df_ioi <- read.csv("./2_trimmed/data_ioi.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

# SubNr as a factor
df_ioi$SubNr <- as.factor(df_ioi$SubNr)

####################################
# Aggregate data
####################################
# 1. Overall tempo
# For each individual
ioi <- aggregate(IOI~SubNr*Condition*Skill, data = df_ioi,
                 FUN = function(x){c(length(x), mean = mean(x), sd = sd(x))})
ioi <- cbind(ioi[,1:3], as.data.frame(ioi[,4]))
# Change colnames
colnames(ioi) <- c("SubNr", "Condition", "Skill", "N", "Mean", "SD")

# Group mean
ioi_stats <- aggregate(Mean~Condition*Skill, data = ioi,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_stats <- cbind(ioi_stats[,1:2], as.data.frame(ioi_stats[,3]))
# Change colnames
colnames(ioi_stats) <- c("Condition", "Skill", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
ioi_ezstats <- ezStats(
  data = df_ioi
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , check_args = TRUE
)

# 2. Average tempo for skill changes
# For each individual
ioi_ch <- aggregate(IOI~SubNr*Condition*Skill, data = subset(df_ioi, df_ioi$Interval == 8 | df_ioi$Interval == 16 | df_ioi$Interval == 24 | df_ioi$Interval == 41 | df_ioi$Interval == 49 | df_ioi$Interval == 57),
                    FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
ioi_ch <- cbind(ioi_ch[,1:3], as.data.frame(ioi_ch[,4]))
# Change colnames
colnames(ioi_ch) <- c("SubNr", "Condition", "Skill", "N", "Mean", "SD")

# Group mean
ioi_ch_stats <- aggregate(Mean~Condition*Skill, data = ioi_ch,
                       FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_ch_stats <- cbind(ioi_ch_stats[,1:2], as.data.frame(ioi_ch_stats[,3]))
# Change colnames
colnames(ioi_ch_stats) <- c("Condition", "Skill", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
ioi_ch_ezstats <- ezStats(
  data = subset(df_ioi, df_ioi$Interval == 8 | df_ioi$Interval == 16 | df_ioi$Interval == 24 | df_ioi$Interval == 41 | df_ioi$Interval == 49 | df_ioi$Interval == 57)
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , check_args = TRUE
)

# 3. Average tempo for each sub-skill change
# For each individual
ioi_ch_sub <- aggregate(IOI~SubNr*Condition*Skill*SubSkill, data = subset(df_ioi, df_ioi$Interval == 8 | df_ioi$Interval == 16 | df_ioi$Interval == 24 | df_ioi$Interval == 41 | df_ioi$Interval == 49 | df_ioi$Interval == 57),
                    FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
ioi_ch_sub <- cbind(ioi_ch_sub[,1:4], as.data.frame(ioi_ch_sub[,5]))
# Change colnames
colnames(ioi_ch_sub) <- c("SubNr", "Condition", "Skill", "SubSkill", "N", "Mean", "SD")

# Group mean
ioi_ch_sub_stats <- aggregate(Mean~Condition*Skill*SubSkill, data = ioi_ch_sub,
                          FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_ch_sub_stats <- cbind(ioi_ch_sub_stats[,1:3], as.data.frame(ioi_ch_sub_stats[,4]))
# Change colnames
colnames(ioi_ch_sub_stats) <- c("Condition", "Skill", "SubSkill", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
ioi_ch_sub_ezstats <- ezStats(
  data = subset(df_ioi, df_ioi$Interval == 8 | df_ioi$Interval == 16 | df_ioi$Interval == 24 | df_ioi$Interval == 41 | df_ioi$Interval == 49 | df_ioi$Interval == 57)
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , check_args = TRUE
)

# 4. Average tempo for each note
# For each individual
ioi_seq <- aggregate(IOI~SubNr*Condition*Skill*Interval, data = df_ioi,
                                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
ioi_seq <- cbind(ioi_seq[,1:4], as.data.frame(ioi_seq[,5]))
# Change colnames
colnames(ioi_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# Group mean
ioi_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = ioi_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_seq_stats <- cbind(ioi_seq_stats[,1:3], as.data.frame(ioi_seq_stats[,4]))
# Change colnames
colnames(ioi_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

# 5. Variability (SD/mean IOI)
# Create a data frame for variability
df_var <- data.frame()
for (subnr in unique(df_ioi$SubNr)){
  for (block in unique(df_ioi$BlockNr)){
    cond = as.character(unique(df_ioi$Condition[df_ioi$SubNr == subnr & df_ioi$BlockNr == block]))
    skill = as.character(unique(df_ioi$Skill[df_ioi$SubNr == subnr & df_ioi$BlockNr == block]))
    for (trial in unique(df_ioi$TrialNr)){
      df_current <- df_ioi %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      df_var <- rbind(df_var, data.frame(subnr, block, trial, cond, skill, sd(df_current$IOI)/mean(df_current$IOI)))
    }
  }
}
colnames(df_var) <- c("SubNr", "BlockNr", "TrialNr", "Condition", "Skill", "Variability")

# Average variability
# For each individual
ioi_var <- aggregate(Variability~SubNr*Condition*Skill, data = df_var,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
ioi_var <- cbind(ioi_var[,1:3], as.data.frame(ioi_var[,4]))
# Change colnames
colnames(ioi_var) <- c("SubNr", "Condition", "Skill", "N", "Mean", "SD")

# Group mean
ioi_var_stats <- aggregate(Mean~Condition*Skill, data = ioi_var,
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_var_stats <- cbind(ioi_var_stats[,1:2], as.data.frame(ioi_var_stats[,3]))
# Change colnames
colnames(ioi_var_stats) <- c("Condition", "Skill", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
ioi_var_ezstats <- ezStats(
  data = df_var[complete.cases(df_var),]
  , dv = .(Variability)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , check_args = TRUE
)

# Variability for each trial
# For each individual
ioi_var_tri <- aggregate(Variability~SubNr*Condition*Skill*TrialNr, data = df_var,
                         FUN = function(x){c(N = length(x), mean = mean(x))})
ioi_var_tri <- cbind(ioi_var_tri[,1:4], as.data.frame(ioi_var_tri[,5]))
# Change colnames
colnames(ioi_var_tri) <- c("SubNr", "Condition", "Skill", "TrialNr", "N", "Mean")

# Group mean
ioi_var_tri_stats <- aggregate(Mean~Condition*Skill*TrialNr, data = ioi_var_tri,
                               FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_var_tri_stats <- cbind(ioi_var_tri_stats[,1:3], as.data.frame(ioi_var_tri_stats[,4]))
colnames(ioi_var_tri_stats) <- c("Condition", "Skill", "TrialNr", "N", "Mean", "SD", "SEM")

# 6. The intervals related to subskill changes vs. other intervals
df_ioi_comp <- df_ioi

# Assess whether a given interval is on sub-skill change points or not (Yes / No)
df_ioi_comp$Change <- "No"
df_ioi_comp$Change[df_ioi_comp$Interval == 8 | df_ioi_comp$Interval == 16 | df_ioi_comp$Interval == 24 | df_ioi_comp$Interval == 41 | df_ioi_comp$Interval == 49 | df_ioi_comp$Interval == 57] <- "Yes"

# Change as a factor
df_ioi_comp$Change <- as.factor(df_ioi_comp$Change)

# For each participant
ioi_comp <- aggregate(IOI~SubNr*Condition*Skill*Change, data = df_ioi_comp,
                         FUN = function(x){c(length(x), mean = mean(x), sd = sd(x))})
ioi_comp <- cbind(ioi_comp[,1:4], ioi_comp[,5])
# Change colnames
colnames(ioi_comp) <- c("SubNr", "Condition", "Skill", "Change", "N", "Mean", "SD")

# Group mean
ioi_comp_stats <- aggregate(Mean~Condition*Skill*Change, data = ioi_comp,
                               FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_comp_stats <- cbind(ioi_comp_stats[,1:3], ioi_comp_stats[,4])
# Change colnames
colnames(ioi_comp_stats) <- c("Condition", "Skill", "Change", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
ioi_comp_ezstats <- ezStats(
  data = df_ioi_comp
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, Skill, Change)
  , type = 3
  , check_args = TRUE
)

# 7. The intervals related to subskill changes vs. middle intervals
df_ioi_comp2 <- df_ioi

# Define phrases
ls_phrase <- list(c(1:7), c(9:15), c(17:23), c(25:31), c(34:40), c(42:48), c(50:56), c(58:64))

# Assess whether a given interval is on sub-skill change points or not (Yes / No)
df_ioi_comp2$Change <- NA
df_ioi_comp2$Change[df_ioi_comp2$Interval == 8 | df_ioi_comp2$Interval == 16 | df_ioi_comp2$Interval == 24 | df_ioi_comp2$Interval == 41 | df_ioi_comp2$Interval == 49 | df_ioi_comp2$Interval == 57] <- "Yes"
for (phrase in 1:length(ls_phrase)){
  for (interval in 1:length(ls_phrase[[phrase]])){
    if (interval == 4){
      df_ioi_comp2$Change[df_ioi_comp2$Interval == ls_phrase[[phrase]][interval]] <- "No"
    }
  }
}
# Change as a factor
df_ioi_comp2$Change <- as.factor(df_ioi_comp2$Change)

# For each participant
ioi_comp2 <- aggregate(IOI~SubNr*Condition*Skill*Change, data = df_ioi_comp2,
                      FUN = function(x){c(length(x), mean = mean(x), sd = sd(x))})
ioi_comp2 <- cbind(ioi_comp[,1:4], ioi_comp2[,5])
# Change colnames
colnames(ioi_comp2) <- c("SubNr", "Condition", "Skill", "Change", "N", "Mean", "SD")

# Group mean
ioi_comp2_stats <- aggregate(Mean~Condition*Skill*Change, data = ioi_comp2,
                            FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_comp2_stats <- cbind(ioi_comp2_stats[,1:3], ioi_comp2_stats[,4])
# Change colnames
colnames(ioi_comp2_stats) <- c("Condition", "Skill", "Change", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
ioi_comp2_ezstats <- ezStats(
  data = df_ioi_comp2[complete.cases(df_ioi_comp2),]
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, Skill, Change)
  , type = 3
  , check_args = TRUE
)

# Add order info
ioi_ch_sub_stats$LabelOrder[ioi_ch_sub_stats$Skill == "articulation"] <- 1
ioi_ch_sub_stats$LabelOrder[ioi_ch_sub_stats$Skill == "dynamics"] <- 2
ioi_var_stats$LabelOrder[ioi_var_stats$Skill == "articulation"] <- 1
ioi_var_stats$LabelOrder[ioi_var_stats$Skill == "dynamics"] <- 2
ioi_comp_stats$LabelOrder[ioi_comp_stats$Skill == "articulation"] <- 1
ioi_comp_stats$LabelOrder[ioi_comp_stats$Skill == "dynamics"] <- 2
ioi_comp_stats$LabelOrder2[ioi_comp_stats$Change == "Yes"] <- 1
ioi_comp_stats$LabelOrder2[ioi_comp_stats$Change == "No"] <- 2
ioi_comp2_stats$LabelOrder[ioi_comp2_stats$Skill == "articulation"] <- 1
ioi_comp2_stats$LabelOrder[ioi_comp2_stats$Skill == "dynamics"] <- 2
ioi_comp2_stats$LabelOrder2[ioi_comp2_stats$Change == "Yes"] <- 1
ioi_comp2_stats$LabelOrder2[ioi_comp2_stats$Change == "No"] <- 2

####################################
# Plots
####################################
p_ioi <- ggplot(data = ioi_stats, aes(x = Skill, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean IOI (ms)") + coord_cartesian(ylim = c(100, 230)) +
  theme_classic()
p_ioi

p_ioi_ch <- ggplot(data = ioi_ch_stats, aes(x = Skill, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(x = "Skill", y = "IOI (ms)") + coord_cartesian(ylim = c(100, 230)) +
  theme_classic()
p_ioi_ch

p_ioi_ch_sub <- ggplot(data = ioi_ch_sub_stats, aes(x = reorder(SubSkill, LabelOrder), y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "IOI (ms)") + coord_cartesian(ylim = c(100, 230)) +
  theme_classic()
p_ioi_ch_sub

p_ioi_seq <- ggplot(data = ioi_seq_stats, aes(x = Interval, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 188, linetype = "dashed") + # Tempo
  facet_grid(Skill ~ .) +
  annotate("text", 0, 188, label = "Tempo (80bpm)", vjust = -1) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "IOI (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()
p_ioi_seq

p_ioi_var <- ggplot(data = ioi_var_stats, aes(x = reorder(Skill, LabelOrder), y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(x = "Skill", y = "CV (SD/mean IOI)") + coord_cartesian(ylim = c(0, .1)) +
  theme_classic()
p_ioi_var

p_ioi_var_tri <- ggplot(data = ioi_var_tri_stats, aes(x = TrialNr, y = Mean, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Trial Number", y = "CV (SD/mean IOI)") + scale_x_continuous(breaks=seq(1,8,1)) +
  theme_classic()
p_ioi_var_tri

p_ioi_comp <- ggplot(data = ioi_comp_stats, aes(x = reorder(Skill, LabelOrder), y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  facet_grid(. ~ reorder(Change, LabelOrder2)) +
  labs(x = "Skill", y = "IOI (ms)") + coord_cartesian(ylim = c(100, 230)) +
  theme_classic()
p_ioi_comp

p_ioi_comp2 <- ggplot(data = ioi_comp2_stats, aes(x = reorder(Skill, LabelOrder), y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  facet_grid(. ~ reorder(Change, LabelOrder2)) +
  labs(x = "Skill", y = "IOI (ms)") + coord_cartesian(ylim = c(100, 230)) +
  theme_classic()
p_ioi_comp2

# Save plots
# png files
ggsave("./3_stats/plot/ioi/p_ioi.png", plot = p_ioi, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/ioi/p_ioi_ch.png", plot = p_ioi_ch, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/ioi/p_ioi_ch_sub.png", plot = p_ioi_ch_sub, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/ioi/p_ioi_seq.png", plot = p_ioi_seq, dpi = 600, width = 15, height = 4)
ggsave("./3_stats/plot/ioi/p_ioi_var.png", plot = p_ioi_var, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/ioi/p_ioi_var_tri.png", plot = p_ioi_var_tri, dpi = 600, width = 10, height = 4)
ggsave("./3_stats/plot/ioi/p_ioi_comp.png", plot = p_ioi_comp, dpi = 600, width = 7, height = 4)
ggsave("./3_stats/plot/ioi/p_ioi_comp2.png", plot = p_ioi_comp2, dpi = 600, width = 7, height = 4)

####################################
# Statistics
####################################
# 1. Normality check
ioi_norm <- by(ioi$Mean, list(ioi$Condition, ioi$Skill), shapiro.test)
ioi_ch_norm <- by(ioi_ch$Mean, list(ioi_ch$Condition, ioi$Skill), shapiro.test)
ioi_ch_sub_norm <- by(ioi_ch_sub$Mean, list(ioi_ch_sub$Condition, ioi_ch_sub$Skill), shapiro.test)
ioi_var_norm <- by(ioi_var$Mean, list(ioi_var$Condition, ioi_var$Skill), shapiro.test)
ioi_comp_norm <- by(ioi_comp$Mean, list(ioi_comp$Condition, ioi_comp$Skill, ioi_comp$Change), shapiro.test)
ioi_comp2_norm <- by(ioi_comp2$Mean, list(ioi_comp2$Condition, ioi_comp2$Skill, ioi_comp2$Change), shapiro.test)

# Draw qqnorm when there is the violation of Normality
qqnorm(ioi_var$Mean[ioi_var$Condition == "performing" & ioi_var$Skill == "dynamics"])
qqnorm(ioi_var$Mean[ioi_var$Condition == "teaching" & ioi_var$Skill == "dynamics"])
qqnorm(ioi_var$Mean[ioi_var$Condition == "teaching" & ioi_var$Skill == "articulation"])
qqnorm(ioi_comp$Mean[ioi_comp$Condition == "teaching" & ioi_comp$Skill == "articulation" & ioi_comp$Change == "Yes"])
qqnorm(ioi_comp$Mean[ioi_comp$Condition == "teaching" & ioi_comp$Skill == "dynamics" & ioi_comp$Change == "Yes"])
qqnorm(ioi_comp2$Mean[ioi_comp2$Condition == "teaching" & ioi_comp2$Skill == "articulation" & ioi_comp2$Change == "Yes"])
qqnorm(ioi_comp2$Mean[ioi_comp2$Condition == "teaching" & ioi_comp2$Skill == "dynamics" & ioi_comp2$Change == "Yes"])

# 2. Two-way ANOVA
# ioi
ioi_aov <- ezANOVA(
  data = df_ioi
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(ioi_aov)
write.csv(ioi_aov$ANOVA, file = "./3_stats/ioi/ioi_aov.csv")

# ioi_ch
ioi_ch_aov <- ezANOVA(
  data = subset(df_ioi, df_ioi$Interval == 8 | df_ioi$Interval == 16 | df_ioi$Interval == 24 | 
                  df_ioi$Interval == 41 | df_ioi$Interval == 49 | df_ioi$Interval == 57)
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(ioi_ch_aov)
write.csv(ioi_ch_aov$ANOVA, file = "./3_stats/ioi/ioi_ch_aov.csv")

# ioi_ch_sub
ioi_ch_sub_aov <- ezANOVA(
  data = subset(df_ioi, df_ioi$Interval == 8 | df_ioi$Interval == 16 | df_ioi$Interval == 24 | 
                  df_ioi$Interval == 41 | df_ioi$Interval == 49 | df_ioi$Interval == 57)
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(ioi_ch_sub_aov)
write.csv(ioi_ch_sub_aov$ANOVA, file = "./3_stats/ioi/ioi_ch_sub_aov.csv")

# ioi_var
ioi_var_aov <- ezANOVA(
  data = df_var[complete.cases(df_var),]
  , dv = .(Variability)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(ioi_var_aov)
write.csv(ioi_var_aov$ANOVA, file = "./3_stats/ioi/ioi_var_aov.csv")

# ioi_comp
ioi_comp_aov <- ezANOVA(
  data = df_ioi_comp
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, Skill, Change)
  , type = 3
  , detailed = TRUE
)
print(ioi_comp_aov)
write.csv(ioi_comp_aov$ANOVA, file = "./3_stats/ioi/ioi_comp_aov.csv")

# ioi_comp2
ioi_comp2_aov <- ezANOVA(
  data = df_ioi_comp2[complete.cases(df_ioi_comp2),]
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, Skill, Change)
  , type = 3
  , detailed = TRUE
)
print(ioi_comp2_aov)
write.csv(ioi_comp2_aov$ANOVA, file = "./3_stats/ioi/ioi_comp2_aov.csv")
