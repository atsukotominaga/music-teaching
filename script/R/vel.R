#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 26/02/2019
# This script aggregate and plot data (Velocity)
# GitHub repo (private): https://github.com/atsukotominaga/expertpiano/tree/master/script/R 

####################################
#  Requirements
####################################
### !!! Set working directory to file source location !!!

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

# 3_stats/vel - store csv files
if (!file.exists("3_stats/vel")){
  dir.create("3_stats/vel")
}

# 3_stats/plot
if (!file.exists("3_stats/plot")){
  dir.create("3_stats/plot")
}
# 3_stats/vel - store png files
if (!file.exists("3_stats/plot/vel/")){
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

# Add order info
vel_sub_stats$LabelOrder[vel_sub_stats$Skill == "articulation"] <- 1
vel_sub_stats$LabelOrder[vel_sub_stats$Skill == "dynamics"] <- 2
vel_ch_sub_stats$LabelOrder[vel_ch_sub_stats$Skill == "articulation"] <- 1
vel_ch_sub_stats$LabelOrder[vel_ch_sub_stats$Skill == "dynamics"] <- 2

####################################
# Diff between first/end and others
####################################
# # Define phrases
# # For each note
# ls_legato <- list(c(1:8), c(17:24), c(42:49), c(58:65))
# ls_staccato <- list(c(9:16), c(25:32), c(34:41), c(50:57))
# ls_forte <- list(c(1:8), c(17:24), c(42:49), c(58:65))
# ls_piano <- list(c(9:16), c(25:32), c(34:41), c(50:57))

# # Difference between first+end and other notes within each phrase
# df_diff <- data.frame()
# df_diff_forte <- data.frame()
# df_diff_piano <- data.frame()
# for (cond in unique(vel_seq$Condition)){
#   for (skill in unique(vel_seq$Skill)){
#     df_current <- vel_seq %>% dplyr::filter(vel_seq$Condition == cond & vel_seq$Skill == skill)
#     if (skill == "dynamics"){
#       for (phrase in 1:length(ls_forte)){
#         first <- df_current$Velocity[,2][df_current$Note == ls_forte[[phrase]][1]]
#         end <- df_current$Velocity[,2][df_current$Note == ls_forte[[phrase]][8]]
#         others <- mean(df_current$Velocity[,2][df_current$Note == ls_forte[[phrase]][2]],
#                        df_current$Velocity[,2][df_current$Note == ls_forte[[phrase]][3]],
#                        df_current$Velocity[,2][df_current$Note == ls_forte[[phrase]][4]],
#                        df_current$Velocity[,2][df_current$Note == ls_forte[[phrase]][5]],
#                        df_current$Velocity[,2][df_current$Note == ls_forte[[phrase]][6]],
#                        df_current$Velocity[,2][df_current$Note == ls_forte[[phrase]][7]])
#         df_diff_forte <- data.frame(cond, skill, "Forte", phrase, mean(first, end)-others)
#         colnames(df_diff_forte) <- c("Condition", "Skill", "SubSkill", "PhraseNo", "Diff")
#         df_diff <- rbind(df_diff, df_diff_forte)
#       }
#       for (phrase in 1:length(ls_piano)){
#         first <- df_current$Velocity[,2][df_current$Note == ls_piano[[phrase]][1]]
#         end <- df_current$Velocity[,2][df_current$Note == ls_piano[[phrase]][8]]
#         others <- mean(df_current$Velocity[,2][df_current$Note == ls_piano[[phrase]][2]],
#                        df_current$Velocity[,2][df_current$Note == ls_piano[[phrase]][3]],
#                        df_current$Velocity[,2][df_current$Note == ls_piano[[phrase]][4]],
#                        df_current$Velocity[,2][df_current$Note == ls_piano[[phrase]][5]],
#                        df_current$Velocity[,2][df_current$Note == ls_piano[[phrase]][6]],
#                        df_current$Velocity[,2][df_current$Note == ls_piano[[phrase]][7]])
#         df_diff_piano <- data.frame(cond, skill, "Piano", phrase, mean(first, end)-others)
#         colnames(df_diff_piano) <- c("Condition", "Skill", "SubSkill", "PhraseNo", "Diff")
#         df_diff <- rbind(df_diff, df_diff_piano)
#       }
#     }
#   }
# }
# 
# vel_diff <- aggregate(Diff~Condition*Skill*SubSkill, data = df_diff,
#                       FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
# vel_diff <- cbind(vel_diff, as.data.frame(vel_diff[,4]))
# 
# p_vel_diff <- ggplot(data = vel_diff, aes(x = SubSkill, y = mean, fill = Condition)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
#                 width=.2, position = position_dodge(.9)) +
#   labs(y = "Difference") + #coord_cartesian(ylim = c(100, 250)) +
#   theme_classic()
# p_vel_diff
# ggsave("./plot/vel/p_vel_diff.png", plot = p_vel_diff, dpi = 600, width = 5, height = 4)

####################################
# Velocity plots
####################################
p_vel_sub <- ggplot(data = vel_sub_stats, aes(x = reorder(SubSkill, LabelOrder), y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "Velocity (0-127)") + coord_cartesian(ylim = c(50, 90)) + 
  theme_classic()
p_vel_sub

p_vel_ch_sub <- ggplot(data = vel_ch_sub_stats, aes(x = reorder(SubSkill, LabelOrder), y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(x = "Skill change", y = "Acceleration") + coord_cartesian(ylim = c(-20, 30)) + 
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
  labs(x = "Interval", y = "Acceleration") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()
p_vel_acc_seq

# Save plots
# png files
ggsave("./3_stats/plot/vel/p_vel_sub.png", plot = p_vel_sub, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/vel/p_vel_ch_sub.png", plot = p_vel_ch_sub, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/vel/p_vel_seq.png", plot = p_vel_seq, dpi = 600, width = 15, height = 4)
ggsave("./3_stats/plot/vel/p_vel_acc_seq.png", plot = p_vel_acc_seq, dpi = 600, width = 15, height = 4) 

####################################
# Statistics
####################################
# 1. Normality check
vel_sub_norm <- by(vel_sub$Mean, list(vel_sub$Condition, vel_sub$SubSkill), shapiro.test)
vel_ch_sub_norm <- by(vel_ch_sub$Mean, list(vel_ch_sub$Condition, vel_ch_sub$SubSkill), shapiro.test)

# Export the results
write.csv(vel_sub_norm, file = "./3_stats/vel/vel_sub_norm.txt", row.names = FALSE)
write.csv(vel_ch_sub_norm, file = "./3_stats/vel/vel_ch_sub_norm.txt", row.names = FALSE)

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
