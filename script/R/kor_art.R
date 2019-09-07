#!/usr/local/bin/R
rm(list=ls(all=TRUE)) # clear all in Environment

####################################
#  Documentation
####################################
# Created: 20/08/2019
# This script aggregates, plots data (KOR) and runs statistical tests for articulation
# GitHub repo (private): https://github.com/atsukotominaga/teaching_v1.0/script/R

####################################
#  Requirements
####################################
# Install and load required packages
# data manipulation
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
# plot
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("ggsignif")) {install.packages("ggsignif"); require("ggsignif")}
# statistics
if (!require("stats")) {install.packages("stats"); require("stats")}
if (!require("ez")) {install.packages("ez"); require("ez")}

# Create necessary folders if not exist
# 3_stats
if (!file.exists("3_stats")){
  dir.create("3_stats")
}

# 3_stats/kor_art - store csv files
if (!file.exists("3_stats/kor_art")){
  dir.create("3_stats/kor_art")
}

# 3_stats/plot
if (!file.exists("3_stats/plot")){
  dir.create("3_stats/plot")
}
# 3_stats/kor_art - store png files
if (!file.exists("3_stats/plot/kor_art")){
  dir.create("3_stats/plot/kor_art")
}

####################################
# Reading and formatting data
####################################
df_ioi <- read.csv("./2_trimmed/data_ioi.csv", header = T, sep = ",", dec = ".") # read a trimmed csv
df_kot <- read.csv("./2_trimmed/data_kot.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

# SubNr as a factor
df_ioi$SubNr <- as.factor(df_ioi$SubNr)
df_kot$SubNr <- as.factor(df_kot$SubNr)

# Include only articulation
df_ioi <- df_ioi %>% dplyr::filter(Skill == "articulation")
df_kot <- df_kot %>% dplyr::filter(Skill == "articulation")

####################################
# Calculate KOR
####################################
# For each individual, each block, each trial
ioi <- aggregate(IOI~SubNr*BlockNr*TrialNr*Condition*Skill, data = df_ioi,
                 FUN = function(x){c(length(x), mean = mean(x), sd = sd(x))})
ioi <- cbind(ioi[,1:5], as.data.frame(ioi[,6]))
# Change colnames
colnames(ioi) <- c("SubNr", "BlockNr", "TrialNr", "Condition", "Skill", "N", "Mean", "SD")

# Calculate KOR for each interval
df_kor <- data.frame()
for (subnr in unique(df_kot$SubNr)){
  for (block in unique(df_kot$BlockNr[df_kot$SubNr == subnr])){
    for (trial in unique(df_kot$TrialNr[df_kot$SubNr == subnr & df_kot$BlockNr == block])){
      df_current <- df_kot %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      df_current$KOR <- df_current$KOT/ioi$Mean[ioi$SubNr == subnr & ioi$BlockNr == block & ioi$TrialNr == trial]
      df_kor <- rbind(df_kor, df_current)
    }
  }
}

####################################
# Aggregate data
####################################
# 1. Average KOR for each subcomponent
# For each individual
kor_sub <- aggregate(KOR~SubNr*Condition*Skill*Subcomponent, data = subset(df_kor, df_kor$Interval != 8 & df_kor$Interval != 16 & df_kor$Interval != 24 & df_kor$Interval != 41 & df_kor$Interval != 49 & df_kor$Interval != 57),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kor_sub <- cbind(kor_sub[,1:4], as.data.frame(kor_sub[,5]))
# Change colnames
colnames(kor_sub) <- c("SubNr", "Condition", "Skill", "Subcomponent", "N", "Mean", "SD")

# Group mean
kor_sub_stats <- aggregate(Mean~Condition*Skill*Subcomponent, data = kor_sub,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kor_sub_stats <- cbind(kor_sub_stats[,1:3], as.data.frame(kor_sub_stats[,4]))
# Change colnames
colnames(kor_sub_stats) <- c("Condition", "Skill", "Subcomponent", "N", "Mean", "SD", "SEM")
# Export summary stats
capture.output(kor_sub_stats, file = "./3_stats/kor_art/summary_stats.txt")

# Checking values with ezStats
kor_sub_ezstats <- ezStats(
  data = kor_sub
  , dv = .(Mean)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , check_args = TRUE
)

# 2. Average KOR for each note
# For each individual
kor_seq <- aggregate(KOR~SubNr*Condition*Skill*Interval, data = df_kor,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kor_seq <- cbind(kor_seq[,1:4], as.data.frame(kor_seq[,5]))
# Change colnames
colnames(kor_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# Group mean
kor_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = kor_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kor_seq_stats <- cbind(kor_seq_stats[,1:3], as.data.frame(kor_seq_stats[,4]))
# Change colnames for each interval
colnames(kor_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

####################################
# Plots
####################################
p_kor_sub <- ggplot(data = kor_sub_stats, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(x = "Subcomponent", y = "KOR") + coord_cartesian(ylim = c(-0.8, 0.2)) +
  geom_signif(y_position=c(mean(kor_sub_stats$Mean[kor_sub_stats$Subcomponent == "Legato"])+mean(kor_sub_stats$SEM[kor_sub_stats$Subcomponent == "Legato"]+0.05),
                           mean(kor_sub_stats$Mean[kor_sub_stats$Subcomponent == "Staccato"])+mean(kor_sub_stats$SEM[kor_sub_stats$Subcomponent == "Staccato"]+0.05)),
              xmin = c(0.8, 1.8), xmax = c(1.2, 2.2), annotation = c("***", "***"), tip_length = 0, textsize = 10, family = "Helvetica Neue LT Std 57 Condensed") +
  theme_classic() +
  theme(text = element_text(size = 20, family = "Helvetica Neue LT Std 57 Condensed"))
p_kor_sub

p_kor_seq <- ggplot(data = kor_seq_stats, aes(x = Interval, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  facet_grid(Skill ~ .) + 
  labs(x = "Interval", y = "KOR") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic() +
  theme(text = element_text(size = 15, family = "Helvetica Neue LT Std 57 Condensed"))
p_kor_seq

# Save plots
# png files
ggsave("./3_stats/plot/kor_art/p_kor_sub.png", plot = p_kor_sub, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/kor_art/p_kor_seq.png", plot = p_kor_seq, dpi = 600, width = 15, height = 4)

####################################
### Statistics
####################################
# Two-way ANOVA
# kor_sub
kor_sub_aov <- ezANOVA(
  data = subset(df_kor, df_kor$Interval != 8 & df_kor$Interval != 16 & df_kor$Interval != 24 & 
                  df_kor$Interval != 41 & df_kor$Interval != 49 & df_kor$Interval != 57)
  , dv = .(KOR)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , detailed = TRUE
)
print(kor_sub_aov)
write.csv(kor_sub_aov$ANOVA, file = "./3_stats/kor_art/kor_sub_aov.csv")

# aov
kor_sub_aov_2 <- aov(Mean ~ Condition*Subcomponent + Error(SubNr/(Condition*Subcomponent)), data = kor_sub)
capture.output(summary(kor_sub_aov_2), file = "./3_stats/kor_art/kor_sub_aov_2.txt")

# posthoc comparison
kor_sub_ph <- aov(Mean~Condition*Subcomponent, kor_sub)
kor_sub_ph <- TukeyHSD(kor_sub_ph)
print(kor_sub_ph)
write.csv(kor_sub_ph$`Condition:Subcomponent`, file = "./3_stats/kor_art/kor_sub_ph.csv")
