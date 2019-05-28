#!/usr/bin/Rscript
rm(list=ls(all=TRUE)) # clear all in Environment

####################################
#  Documentation
####################################
# Created: 19/05/2019
# This script aggregates, plots data (KOT) and runs statistical tests for articulation
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

# 3_stats/kot_art - store csv files
if (!file.exists("3_stats/kot_art")){
  dir.create("3_stats/kot_art")
}

# 3_stats/plot
if (!file.exists("3_stats/plot")){
  dir.create("3_stats/plot")
}
# 3_stats/plot/kot_art - store png files
if (!file.exists("3_stats/plot/kot_art")){
  dir.create("3_stats/plot/kot_art")
}

####################################
# Reading and formatting data
####################################
df_kot <- read.csv("./2_trimmed/data_kot.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

# SubNr as a factor
df_kot$SubNr <- as.factor(df_kot$SubNr)

# Include only articulation
df_kot <- df_kot %>% dplyr::filter(Skill == "articulation")

####################################
# Aggregate data
####################################
# 1. Average KOT for each subcomponent
# For each individual
kot_sub <- aggregate(KOT~SubNr*Condition*Skill*Subcomponent, data = subset(df_kot, df_kot$Interval != 8 & df_kot$Interval != 16 & df_kot$Interval != 24 & df_kot$Interval != 41 & df_kot$Interval != 49 & df_kot$Interval != 57),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kot_sub <- cbind(kot_sub[,1:4], as.data.frame(kot_sub[,5]))
# Change colnames
colnames(kot_sub) <- c("SubNr", "Condition", "Skill", "Subcomponent", "N", "Mean", "SD")

# Group mean
kot_sub_stats <- aggregate(Mean~Condition*Skill*Subcomponent, data = kot_sub,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_sub_stats <- cbind(kot_sub_stats[,1:3], as.data.frame(kot_sub_stats[,4]))
# Change colnames
colnames(kot_sub_stats) <- c("Condition", "Skill", "Subcomponent", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
kot_sub_ezstats <- ezStats(
  data = subset(df_kot, df_kot$Interval  != 8 & df_kot$Interval != 16 & df_kot$Interval != 24 & df_kot$Interval != 41 & df_kot$Interval != 49 & df_kot$Interval != 57)
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , check_args = TRUE
)

# 2. Average KOT for each subcomponent change
# For each individual
kot_ch_sub <- aggregate(KOT~SubNr*Condition*Skill*Subcomponent, data = subset(df_kot, df_kot$Interval == 8 | df_kot$Interval == 16 | df_kot$Interval == 24 | df_kot$Interval == 41 | df_kot$Interval == 49 | df_kot$Interval == 57),
                        FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kot_ch_sub <- cbind(kot_ch_sub[,1:4], as.data.frame(kot_ch_sub[,5]))
# Change colnames
colnames(kot_ch_sub) <- c("SubNr", "Condition", "Skill", "Subcomponent", "N", "Mean", "SD")

# Group mean
kot_ch_sub_stats <- aggregate(Mean~Condition*Skill*Subcomponent, data = kot_ch_sub,
                              FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_ch_sub_stats <- cbind(kot_ch_sub_stats[,1:3], as.data.frame(kot_ch_sub_stats[,4]))
# Change colnames
colnames(kot_ch_sub_stats) <- c("Condition", "Skill", "Subcomponent", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
kot_ch_sub_ezstats <- ezStats(
  data = subset(df_kot, df_kot$Interval == 8 | df_kot$Interval == 16 | df_kot$Interval == 24 | df_kot$Interval == 41 | df_kot$Interval == 49 | df_kot$Interval == 57)
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , check_args = TRUE
)

# 3. Average KOT for each note
# For each individual
kot_seq <- aggregate(KOT~SubNr*Condition*Skill*Interval, data = df_kot,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kot_seq <- cbind(kot_seq[,1:4], as.data.frame(kot_seq[,5]))
# Change colnames
colnames(kot_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# Group mean
kot_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = kot_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_seq_stats <- cbind(kot_seq_stats[,1:3], as.data.frame(kot_seq_stats[,4]))
# Change colnames for each interval
colnames(kot_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

# 4. The beginning or the ending note of each phrase vs. other notes
# Define phrases
ls_phrase <- list(c(1:7), c(9:15), c(17:23), c(25:31), c(34:40), c(42:48), c(50:56), c(58:64))

# Extract data without change points
df_kot_phrase <- df_kot %>% dplyr::filter(Interval != 8 & Interval != 16 & Interval != 24 & Interval != 41 & Interval != 49 & Interval != 57)

# Assess whether a given interval is the beginnning or the ending of a phrase (Yes / No)
df_kot_phrase$Boundary <- NA
for (phrase in 1:length(ls_phrase)){
  for (interval in 1:length(ls_phrase[[phrase]])){
    if (interval == 1 | interval == 7){ # the beginning of the ending of each phrase
      df_kot_phrase$Boundary[df_kot_phrase$Interval == ls_phrase[[phrase]][interval]] <- "Yes"
    } else {
      df_kot_phrase$Boundary[df_kot_phrase$Interval == ls_phrase[[phrase]][interval]] <- "No"
    }
  }
}
# Boundary as a factor
df_kot_phrase$Boundary <- as.factor(df_kot_phrase$Boundary)

# For each participant
kot_phrase <- aggregate(KOT~SubNr*Condition*Skill*Subcomponent*Boundary, data = df_kot_phrase,
                        FUN = function(x){c(length(x), mean = mean(x), sd = sd(x))})
kot_phrase <- cbind(kot_phrase[,1:5], kot_phrase[,6])
# Change colnames
colnames(kot_phrase) <- c("SubNr", "Condition", "Skill", "Subcomponent", "Boundary", "N", "Mean", "SD")

# Group mean
kot_phrase_stats <- aggregate(Mean~Condition*Skill*Subcomponent*Boundary, data = kot_phrase,
                              FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_phrase_stats <- cbind(kot_phrase_stats[,1:4], kot_phrase_stats[,5])
# Change colnames
colnames(kot_phrase_stats) <- c("Condition", "Skill", "Subcomponent", "Boundary", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
kot_phrase_ezstats <- ezStats(
  data = df_kot_phrase
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent, Boundary)
  , type = 3
  , check_args = TRUE
)

# Add order info
kot_phrase_stats$LabelOrder[kot_phrase_stats$Boundary == "Yes"] <- 1
kot_phrase_stats$LabelOrder[kot_phrase_stats$Boundary == "No"] <- 2

####################################
# Plots
####################################
p_kot_sub <- ggplot(data = kot_sub_stats, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(x = "Subcomponent", y = "KOT (ms)") +
  geom_signif(y_position=c(mean(kot_sub_stats$Mean[kot_sub_stats$Subcomponent == "Legato"])+mean(kot_sub_stats$SEM[kot_sub_stats$Subcomponent == "Legato"])+6,
                           mean(kot_sub_stats$Mean[kot_sub_stats$Subcomponent == "Staccato"])+mean(kot_sub_stats$SEM[kot_sub_stats$Subcomponent == "Staccato"])+7),
              xmin=c(0.8, 1.8), xmax=c(1.2, 2.2), annotation=c("***", "***"), tip_length=0) +
  theme_classic()
p_kot_sub

p_kot_ch_sub <- ggplot(data = kot_ch_sub_stats, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(x = "Subcomponent change", y = "KOT (ms)") +
  theme_classic()
p_kot_ch_sub

p_kot_seq <- ggplot(data = kot_seq_stats, aes(x = Interval, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "KOT (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()
p_kot_seq

p_kot_phrase <- ggplot(data = kot_phrase_stats, aes(x = Subcomponent, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  facet_grid(. ~ reorder(Boundary, LabelOrder)) +
  labs(x = "Subcomponent", y = "KOT (ms)") +
  theme_classic()
p_kot_phrase

# Save plots
# png files
ggsave("./3_stats/plot/kot_art/p_kot_sub.png", plot = p_kot_sub, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/kot_art/p_kot_ch_sub.png", plot = p_kot_ch_sub, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/kot_art/p_kot_seq.png", plot = p_kot_seq, dpi = 600, width = 15, height = 4)
ggsave("./3_stats/plot/kot_art/p_kot_phrase.png", plot = p_kot_phrase, dpi = 600, width = 7, height = 4)

####################################
### Statistics
####################################
# 1. Normality check
kot_sub_norm <- by(kot_sub$Mean, list(kot_sub$Condition, kot_sub$Subcomponent), shapiro.test)
kot_ch_sub_norm <- by(kot_ch_sub$Mean, list(kot_ch_sub$Condition, kot_ch_sub$Subcomponent), shapiro.test)
kot_phrase_norm <- by(kot_phrase$Mean, list(kot_phrase$Condition, kot_phrase$Subcomponent, kot_phrase$Boundary), shapiro.test)

# Draw qqnorm when there is the violation of Normality
# Violation!

# Two-way ANOVA
# kot_sub
kot_sub_aov <- ezANOVA(
  data = subset(df_kot, df_kot$Interval != 8 & df_kot$Interval != 16 & df_kot$Interval != 24 & 
                  df_kot$Interval != 41 & df_kot$Interval != 49 & df_kot$Interval != 57)
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , detailed = TRUE
)
print(kot_sub_aov)
write.csv(kot_sub_aov$ANOVA, file = "./3_stats/kot_art/kot_sub_aov.csv")

# posthoc comparison
kot_sub_ph <- aov(KOT~Condition*Subcomponent, data = subset(df_kot, df_kot$Interval != 8 & df_kot$Interval != 16 & df_kot$Interval != 24 & 
                                                               df_kot$Interval != 41 & df_kot$Interval != 49 & df_kot$Interval != 57))
kot_sub_ph <- TukeyHSD(kot_sub_ph)
print(kot_sub_ph)
write.csv(kot_sub_ph$`Condition:Subcomponent`, file = "./3_stats/kot_art/kot_sub_ph.csv")

# kot_ch_sub
kot_ch_sub_aov <- ezANOVA(
  data = subset(df_kot, df_kot$Interval == 8 | df_kot$Interval == 16 | df_kot$Interval == 24 | 
                  df_kot$Interval == 41 | df_kot$Interval == 49 | df_kot$Interval == 57)
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent)
  , type = 3
  , detailed = TRUE
)
print(kot_ch_sub_aov)
write.csv(kot_ch_sub_aov$ANOVA, file = "./3_stats/kot_art/kot_ch_sub_aov.csv")

# kot_phrase
kot_phrase_aov <- ezANOVA(
  data = df_kot_phrase
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, Subcomponent, Boundary)
  , type = 3
  , detailed = TRUE
)
print(kot_phrase_aov)
write.csv(kot_phrase_aov$ANOVA, file = "./3_stats/kot_art/kot_phrase_aov.csv")
