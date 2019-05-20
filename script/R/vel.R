#!/usr/bin/Rscript
rm(list=ls(all=TRUE)) # clear all in Environment

####################################
#  Documentation
####################################
# Created: 26/02/2019
# This script aggregates, plots data (KV) and runs statistical tests
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

# 3_stats/vel - store csv files
if (!file.exists("3_stats/vel")){
  dir.create("3_stats/vel")
}

# 3_stats/plot
if (!file.exists("3_stats/plot")){
  dir.create("3_stats/plot")
}
# 3_stats/vel - store png files
if (!file.exists("3_stats/plot/vel")){
  dir.create("3_stats/plot/vel")
}

####################################
# Reading and formatting data
####################################
df_vel <- read.csv("./2_trimmed/data_vel.csv", header = T, sep = ",", dec = ".") # read a trimmed csv
df_vel_acc <- read.csv("./2_trimmed/data_vel_acc.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

# SubNr as a factor
df_vel$SubNr <- as.factor(df_vel$SubNr)
df_vel_acc$SubNr <- as.factor(df_vel_acc$SubNr)

####################################
# Aggregate data
####################################
# 1. Average velocity for each sub-skill
# For each participant
vel_sub <- aggregate(Velocity~SubNr*Condition*Skill*SubSkill, data = df_vel,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_sub <- cbind(vel_sub[,1:4], as.data.frame(vel_sub[,5]))
# Change colnames
colnames(vel_sub) <- c("SubNr", "Condition", "Skill", "SubSkill", "N", "Mean", "SD")

# Group mean
vel_sub_stats <- aggregate(Mean~Condition*Skill*SubSkill, data = vel_sub,
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_sub_stats <- cbind(vel_sub_stats[,1:3], as.data.frame(vel_sub_stats[,4]))
# Change colnames
colnames(vel_sub_stats) <- c("Condition", "Skill", "SubSkill", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
vel_sub_ezstats <- ezStats(
  data = df_vel
  , dv = .(Velocity)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , check_args = TRUE
)

# 2. Average acceleration for each sub-skill change
# For each participant
vel_ch_sub <- aggregate(Acc~SubNr*Condition*Skill*SubSkill, data = subset(df_vel_acc, df_vel_acc$Interval == 8 | df_vel_acc$Interval == 16 | df_vel_acc$Interval == 24 | df_vel_acc$Interval == 41 | df_vel_acc$Interval == 49 | df_vel_acc$Interval == 57),
                        FUN = function(x){c(length(x), mean = mean(x), sd = sd(x))})
vel_ch_sub <- cbind(vel_ch_sub[,1:4], as.data.frame(vel_ch_sub[,5]))
# Change colnames
colnames(vel_ch_sub) <- c("SubNr", "Condition", "Skill", "SubSkill", "N", "Mean", "SD")

# Group mean
vel_ch_sub_stats <- aggregate(Mean~Condition*Skill*SubSkill, data = vel_ch_sub,
                              FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_ch_sub_stats <- cbind(vel_ch_sub_stats[,1:3], as.data.frame(vel_ch_sub_stats[,4]))
# Change colnames
colnames(vel_ch_sub_stats) <- c("Condition", "Skill", "SubSkill", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
vel_ch_sub_ezstats <- ezStats(
  data = subset(df_vel_acc, df_vel_acc$Interval == 8 | df_vel_acc$Interval == 16 | df_vel_acc$Interval == 24 | df_vel_acc$Interval == 41 | df_vel_acc$Interval == 49 | df_vel_acc$Interval == 57)
  , dv = .(Acc)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , check_args = TRUE
)

# 3. Average velocity for each note
vel_seq <- aggregate(Velocity~SubNr*Condition*Skill*Note, data = df_vel, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_seq <- cbind(vel_seq[,1:4], as.data.frame(vel_seq[,5]))
# Change colnames
colnames(vel_seq) <- c("SubNr", "Condition", "Skill", "Note", "N", "Mean", "SD")

# Group mean
vel_seq_stats <- aggregate(Mean~Condition*Skill*Note, data = vel_seq,
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_seq_stats <- cbind(vel_seq_stats[,1:3], as.data.frame(vel_seq_stats[,4]))
# Change colnames
colnames(vel_seq_stats) <- c("Condition", "Skill", "Note", "N", "Mean", "SD", "SEM")

# 4. Average acceleration for each note
vel_acc_seq <- aggregate(Acc~SubNr*Condition*Skill*Interval, data = df_vel_acc, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
vel_acc_seq <- cbind(vel_acc_seq[,1:4], as.data.frame(vel_acc_seq[,5]))
# Change colnames
colnames(vel_acc_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# Group mean
vel_acc_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = vel_acc_seq,
                           FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_acc_seq_stats <- cbind(vel_acc_seq_stats[,1:3], as.data.frame(vel_acc_seq_stats[,4]))
# Change colnames
colnames(vel_acc_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

# 5. The beginning or the ending note of each phrase vs. other notes
# Define phrases for each note
ls_phrase2 <- list(c(1:8), c(9:16), c(17:24), c(25:32), c(34:41), c(42:49), c(50:57), c(58:65))

# Create a new data frame
df_vel_phrase <- df_vel

# Assess whether a given note is the beginnning or the ending of a phrase (Yes / No)
df_vel_phrase$Boundary <- NA
for (phrase in 1:length(ls_phrase2)){
  for (note in 1:length(ls_phrase2[[phrase]])){
    if (note == 1 | note == 8){ # the beginning of the ending of each phrase
      df_vel_phrase$Boundary[df_vel_phrase$Note == ls_phrase2[[phrase]][note]] <- "Yes"
    } else {
      df_vel_phrase$Boundary[df_vel_phrase$Note == ls_phrase2[[phrase]][note]] <- "No"
    }
  }
}
# Boundary as a factor
df_vel_phrase$Boundary <- as.factor(df_vel_phrase$Boundary)

# For each participant
vel_phrase <- aggregate(Velocity~SubNr*Condition*Skill*SubSkill*Boundary, data = df_vel_phrase,
                        FUN = function(x){c(length(x), mean = mean(x), sd = sd(x))})
vel_phrase <- cbind(vel_phrase[,1:5], vel_phrase[,6])
# Change colnames
colnames(vel_phrase) <- c("SubNr", "Condition", "Skill", "SubSkill", "Boundary", "N", "Mean", "SD")

# Group mean
vel_phrase_stats <- aggregate(Mean~Condition*Skill*SubSkill*Boundary, data = vel_phrase,
                              FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_phrase_stats <- cbind(vel_phrase_stats[,1:4], vel_phrase_stats[,5])
# Change colnames
colnames(vel_phrase_stats) <- c("Condition", "Skill", "SubSkill", "Boundary", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
vel_phrase_ezstats <- ezStats(
  data = df_vel_phrase
  , dv = .(Velocity)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill, Boundary)
  , type = 3
  , check_args = TRUE
)

# 6. The beginning or the ending note of each phrase vs. middle two notes
# Extract data without change points
df_vel_phrase2 <- df_vel

# Assess whether a given note is the beginnning or the ending of a phrase (Yes / No)
df_vel_phrase2$Boundary <- NA
for (phrase in 1:length(ls_phrase2)){
  for (note in 1:length(ls_phrase2[[phrase]])){
    if (note == 1 | note == 8){ # the beginning of the ending of each phrase
      df_vel_phrase2$Boundary[df_vel_phrase2$Note == ls_phrase2[[phrase]][note]] <- "Yes"
    } else if (note == 4 | note == 5){
      df_vel_phrase2$Boundary[df_vel_phrase2$Note == ls_phrase2[[phrase]][note]] <- "No"
    }
  }
}
# Boundary as a factor
df_vel_phrase2$Boundary <- as.factor(df_vel_phrase2$Boundary)

# For each participant
vel_phrase2 <- aggregate(Velocity~SubNr*Condition*Skill*SubSkill*Boundary, data = df_vel_phrase2,
                         FUN = function(x){c(length(x), mean = mean(x), sd = sd(x))})
vel_phrase2 <- cbind(vel_phrase2[,1:5], vel_phrase2[,6])
# Change colnames
colnames(vel_phrase2) <- c("SubNr", "Condition", "Skill", "SubSkill", "Boundary", "N", "Mean", "SD")

# Group mean
vel_phrase2_stats <- aggregate(Mean~Condition*Skill*SubSkill*Boundary, data = vel_phrase2,
                               FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_phrase2_stats <- cbind(vel_phrase2_stats[,1:4], vel_phrase2_stats[,5])
# Change colnames
colnames(vel_phrase2_stats) <- c("Condition", "Skill", "SubSkill", "Boundary", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
vel_phrase2_ezstats <- ezStats(
  data = df_vel_phrase2[complete.cases(df_vel_phrase2),]
  , dv = .(Velocity)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill, Boundary)
  , type = 3
  , check_args = TRUE
)

# Add order info
vel_sub_stats$LabelOrder[vel_sub_stats$Skill == "articulation"] <- 1
vel_sub_stats$LabelOrder[vel_sub_stats$Skill == "dynamics"] <- 2
vel_ch_sub_stats$LabelOrder[vel_ch_sub_stats$Skill == "articulation"] <- 1
vel_ch_sub_stats$LabelOrder[vel_ch_sub_stats$Skill == "dynamics"] <- 2
vel_phrase_stats$LabelOrder[vel_phrase_stats$Skill == "articulation"] <- 1
vel_phrase_stats$LabelOrder[vel_phrase_stats$Skill == "dynamics"] <- 2
vel_phrase_stats$LabelOrder2[vel_phrase_stats$Boundary == "Yes"] <- 1
vel_phrase_stats$LabelOrder2[vel_phrase_stats$Boundary == "No"] <- 2
vel_phrase2_stats$LabelOrder[vel_phrase2_stats$Skill == "articulation"] <- 1
vel_phrase2_stats$LabelOrder[vel_phrase2_stats$Skill == "dynamics"] <- 2
vel_phrase2_stats$LabelOrder2[vel_phrase2_stats$Boundary == "Yes"] <- 1
vel_phrase2_stats$LabelOrder2[vel_phrase2_stats$Boundary == "No"] <- 2

####################################
# Velocity plots
####################################
p_vel_sub <- ggplot(data = vel_sub_stats, aes(x = reorder(SubSkill, LabelOrder), y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "Velocity (0-127)") + coord_cartesian(ylim = c(50, 90)) + 
  geom_signif(y_position=c(mean(vel_sub_stats$Mean[vel_sub_stats$SubSkill == "Legato"])+mean(vel_sub_stats$SEM[vel_sub_stats$SubSkill == "Legato"])+2,
                           mean(vel_sub_stats$Mean[vel_sub_stats$SubSkill == "Staccato"])+mean(vel_sub_stats$SEM[vel_sub_stats$SubSkill == "Staccato"])+3.3,
                           mean(vel_sub_stats$Mean[vel_sub_stats$SubSkill == "Forte"])+mean(vel_sub_stats$SEM[vel_sub_stats$SubSkill == "Forte"])+2,
                           mean(vel_sub_stats$Mean[vel_sub_stats$SubSkill == "Piano"])+mean(vel_sub_stats$SEM[vel_sub_stats$SubSkill == "Piano"])+2),
              xmin=c(0.8, 1.8, 2.8, 3.8), xmax=c(1.2, 2.2, 3.2, 4.2), annotation=c("*", "***", "***", "n.s."), tip_length=0) +
  theme_classic()
p_vel_sub

p_vel_ch_sub <- ggplot(data = vel_ch_sub_stats, aes(x = reorder(SubSkill, LabelOrder), y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(x = "Skill change", y = "Acceleration") + coord_cartesian(ylim = c(-20, 30)) + 
  geom_signif(y_position=c(mean(vel_ch_sub_stats$Mean[vel_ch_sub_stats$SubSkill == "LtoS"])+mean(vel_ch_sub_stats$SEM[vel_ch_sub_stats$SubSkill == "LtoS"])+3.3,
                           mean(vel_ch_sub_stats$Mean[vel_ch_sub_stats$SubSkill == "StoL"])+mean(vel_ch_sub_stats$SEM[vel_ch_sub_stats$SubSkill == "StoL"])+2,
                           mean(vel_ch_sub_stats$Mean[vel_ch_sub_stats$SubSkill == "FtoP"])+mean(vel_ch_sub_stats$SEM[vel_ch_sub_stats$SubSkill == "FtoP"])+2,
                           mean(vel_ch_sub_stats$Mean[vel_ch_sub_stats$SubSkill == "PtoF"])+mean(vel_ch_sub_stats$SEM[vel_ch_sub_stats$SubSkill == "PtoF"])+3.3),
              xmin=c(0.8, 1.8, 2.8, 3.8), xmax=c(1.2, 2.2, 3.2, 4.2), annotation=c("**", "n.s.", "*", "***"), tip_length=0) +
  theme_classic()
p_vel_ch_sub

p_vel_seq <- ggplot(data = vel_seq_stats, aes(x = Note, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(0.05)) + 
  labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,67,1)) +
  theme_classic()
p_vel_seq

p_vel_acc_seq <- ggplot(data = vel_acc_seq_stats, aes(x = Interval, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "Acceleration (0-127)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()
p_vel_acc_seq

p_vel_phrase <- ggplot(data = vel_phrase_stats, aes(x = reorder(SubSkill, LabelOrder), y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  facet_grid(. ~ reorder(Boundary, LabelOrder2)) +
  labs(x = "SubSkill", y = "Velocity (0-127)") + #coord_cartesian(ylim = c(100, 230)) +
  theme_classic()
p_vel_phrase

p_vel_phrase2 <- ggplot(data = vel_phrase2_stats, aes(x = reorder(SubSkill, LabelOrder), y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  facet_grid(. ~ reorder(Boundary, LabelOrder2)) +
  labs(x = "SubSkill", y = "Velocity (0-127)") + #coord_cartesian(ylim = c(100, 230)) +
  theme_classic()
p_vel_phrase2

# Save plots
# png files
ggsave("./3_stats/plot/vel/p_vel_sub.png", plot = p_vel_sub, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/vel/p_vel_ch_sub.png", plot = p_vel_ch_sub, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/vel/p_vel_seq.png", plot = p_vel_seq, dpi = 600, width = 15, height = 4)
ggsave("./3_stats/plot/vel/p_vel_acc_seq.png", plot = p_vel_acc_seq, dpi = 600, width = 15, height = 4) 
ggsave("./3_stats/plot/vel/p_vel_phrase.png", plot = p_vel_phrase, dpi = 600, width = 7, height = 4)
ggsave("./3_stats/plot/vel/p_vel_phrase2.png", plot = p_vel_phrase2, dpi = 600, width = 7, height = 4)

####################################
# Statistics
####################################
# 1. Normality check
vel_sub_norm <- by(vel_sub$Mean, list(vel_sub$Condition, vel_sub$SubSkill), shapiro.test)
vel_ch_sub_norm <- by(vel_ch_sub$Mean, list(vel_ch_sub$Condition, vel_ch_sub$SubSkill), shapiro.test)
vel_phrase_norm <- by(vel_phrase$Mean, list(vel_phrase$Condition, vel_phrase$SubSkill, vel_phrase$Boundary), shapiro.test)
vel_phrase2_norm <- by(vel_phrase2$Mean, list(vel_phrase2$Condition, vel_phrase2$SubSkill, vel_phrase2$Boundary), shapiro.test)

# Draw qqnorm when there is the violation of Normality
# Violation!

# Two-way ANOVA
# vel_sub
vel_sub_aov <- ezANOVA(
  data = df_vel
  , dv = .(Velocity)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(vel_sub_aov)
write.csv(vel_sub_aov$ANOVA, file = "./3_stats/vel/vel_sub_aov.csv")
write.csv(vel_sub_aov$`Mauchly's Test for Sphericity`, file = "./3_stats/vel/vel_sub_aov_mau.csv")
write.csv(vel_sub_aov$`Sphericity Corrections`, file = "./3_stats/vel/vel_sub_aov_sph.csv")

# posthoc comparison
vel_sub_ph <- aov(Velocity~Condition*SubSkill, data = df_vel)
vel_sub_ph <- TukeyHSD(vel_sub_ph)
print(vel_sub_ph)
write.csv(vel_sub_ph$`Condition:SubSkill`, file = "./3_stats/vel/vel_sub_ph.csv")

# vel_ch_sub
vel_ch_sub_aov <- ezANOVA(
  data = subset(df_vel_acc, df_vel_acc$Interval == 8 | df_vel_acc$Interval == 16 | df_vel_acc$Interval == 24 |
                  df_vel_acc$Interval == 41 | df_vel_acc$Interval == 49 | df_vel_acc$Interval == 57)
  , dv = .(Acc)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(vel_ch_sub_aov)
write.csv(vel_ch_sub_aov$ANOVA, file = "./3_stats/vel/vel_ch_sub_aov.csv")
write.csv(vel_ch_sub_aov$`Mauchly's Test for Sphericity`, file = "./3_stats/vel/vel_ch_sub_aov_mau.csv")
write.csv(vel_ch_sub_aov$`Sphericity Corrections`, file = "./3_stats/vel/vel_ch_sub_aov_sph.csv")

# posthoc comparison
vel_ch_sub_ph <- aov(Acc~Condition*SubSkill, data = subset(df_vel_acc, df_vel_acc$Interval == 8 | df_vel_acc$Interval == 16 | df_vel_acc$Interval == 24 |
                                                             df_vel_acc$Interval == 41 | df_vel_acc$Interval == 49 | df_vel_acc$Interval == 57))
vel_ch_sub_ph <- TukeyHSD(vel_ch_sub_ph)
print(vel_ch_sub_ph)
write.csv(vel_ch_sub_ph$`Condition:SubSkill`, file = "./3_stats/vel/vel_ch_sub_ph.csv")

# vel_phrase
vel_phrase_aov <- ezANOVA(
  data = df_vel_phrase
  , dv = .(Velocity)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill, Boundary)
  , type = 3
  , detailed = TRUE
)
print(vel_phrase_aov)
write.csv(vel_phrase_aov$ANOVA, file = "./3_stats/vel/vel_phrase_aov.csv")

# vel_phrase2
vel_phrase2_aov <- ezANOVA(
  data = df_vel_phrase2[complete.cases(df_vel_phrase2),]
  , dv = .(Velocity)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill, Boundary)
  , type = 3
  , detailed = TRUE
)
print(vel_phrase2_aov)
write.csv(vel_phrase2_aov$ANOVA, file = "./3_stats/vel/vel_phrase2_aov.csv")
