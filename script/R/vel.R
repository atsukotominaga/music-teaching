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
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}
# statistics
if (!require("car")) {install.packages("car"); require("car")}
if (!require("stats")) {install.packages("stats"); require("stats")}
if (!require("ez")) {install.packages("ez"); require("ez")}

### Create necessary folders if not exist
# plot
if (!file.exists("plot")){
  dir.create("plot")
}
# velocity
if (!file.exists("plot/vel/")){
  dir.create("plot/vel")
}

####################################
# Reading and formatting data
####################################
df_all <- read.csv("./processed/data_analysis.csv", header = T, sep = ",", dec = ".") # clear data without pitch errors
df_exc <- read.csv("./processed/data_errorRate.csv", header = T, sep = ",", dec = ".") # exclusion criteria

### Exclude participants
include <- df_exc$SubNr[df_exc$LessThan10 == "include"]

### Data frame with only included participants
df_analysis <- data.frame()
for (subnr in include){
  df_current <- df_all %>% dplyr::filter(SubNr == subnr)
  df_analysis <- rbind(df_analysis, df_current)
}

####################################
# Velocity - dynamics
####################################
### Calculate Acc (acceleration - velocity difference between notes)
df_vel <- df_analysis %>% dplyr::filter(Key_OnOff == 1)
df_vel$Acc <- diff(c(0, df_vel$Velocity))

### Remove the first note
df_vel_acc <- df_vel %>% dplyr::filter(NoteNr != 17)
df_vel$Acc <- NULL # Remove Acc from df_vel

### Assign a sequence number for each tone / interval
df_vel$Note <- rep(1:67, length(df_vel$NoteNr)/67) # for vel_seq
df_vel_acc$Interval <- rep(1:66, length(df_vel_acc$NoteNr)/66) # for vel_acc_seq

### Define SubSkills
# For each note (for df_vel)
ls_legato_1 <- list(c(1:8), c(17:24), c(42:49), c(58:65))
ls_staccato_1 <- list(c(9:16), c(25:32), c(34:41), c(50:57))
ls_forte_1 <- list(c(1:8), c(17:24), c(42:49), c(58:65))
ls_piano_1 <- list(c(9:16), c(25:32), c(34:41), c(50:57))
# For intervals (for df_vel_acc)
ls_legato_2 <- list(c(1:7), c(17:23), c(42:48), c(58:64))
ls_staccato_2 <- list(c(9:15), c(25:31), c(34:40), c(50:56))
ls_forte_2 <- list(c(1:7), c(17:23), c(42:48), c(58:64))
ls_piano_2 <- list(c(9:15), c(25:31), c(34:40), c(50:56))

### Assign SubSkills
# For each note (for df_vel)
df_vel$SubSkill <- NA
# Legato
for (phrase in 1:length(ls_legato_1)){
  for (note in 1:length(ls_legato_1[[phrase]])){
    df_vel$SubSkill[df_vel$Skill == "articulation" & df_vel$Note == ls_legato_1[[phrase]][note]] <- "Legato"
  }
}

# Staccato
for (phrase in 1:length(ls_staccato_1)){
  for (note in 1:length(ls_staccato_1[[phrase]])){
    df_vel$SubSkill[df_vel$Skill == "articulation" & df_vel$Note == ls_staccato_1[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte_1)){
  for (note in 1:length(ls_forte_1[[phrase]])){
    df_vel$SubSkill[df_vel$Skill == "dynamics" & df_vel$Note == ls_forte_1[[phrase]][note]] <- "Forte"
  }
}

# Piano
for (phrase in 1:length(ls_piano_1)){
  for (note in 1:length(ls_piano_1[[phrase]])){
    df_vel$SubSkill[df_vel$Skill == "dynamics" & df_vel$Note == ls_piano_1[[phrase]][note]] <- "Piano"
  }
}

# For intervals (for df_vel_acc)
# Legato
df_vel_acc$SubSkill <- NA
for (phrase in 1:length(ls_legato_2)){
  for (note in 1:length(ls_legato_2[[phrase]])){
    df_vel_acc$SubSkill[df_vel_acc$Skill == "articulation" & df_vel_acc$Interval == ls_legato_2[[phrase]][note]] <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato_2)){
  for (note in 1:length(ls_staccato_2[[phrase]])){
    df_vel_acc$SubSkill[df_vel_acc$Skill == "articulation" & df_vel_acc$Interval == ls_staccato_2[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte_2)){
  for (note in 1:length(ls_forte_2[[phrase]])){
    df_vel_acc$SubSkill[df_vel_acc$Skill == "dynamics" & df_vel_acc$Interval == ls_forte_2[[phrase]][note]] <- "Forte"
  }
}
# Piano
for (phrase in 1:length(ls_piano_2)){
  for (note in 1:length(ls_piano_2[[phrase]])){
    df_vel_acc$SubSkill[df_vel_acc$Skill == "dynamics" & df_vel_acc$Interval == ls_piano_2[[phrase]][note]] <- "Piano"
  }
}

# Define Skill Change (LtoS, FtoP)
change_1 <- c(8, 24, 49)
# Define Skill Change (StoL, PtoF)
change_2 <- c(16, 41, 57)

for (i in change_1){
  df_vel_acc$SubSkill[df_vel_acc$Skill == "articulation" & df_vel_acc$Interval == i] <- "LtoS"
  df_vel_acc$SubSkill[df_vel_acc$Skill == "dynamics" & df_vel_acc$Interval == i] <- "FtoP"
}
for (i in change_2){
  df_vel_acc$SubSkill[df_vel_acc$Skill == "articulation" & df_vel_acc$Interval == i] <- "StoL"
  df_vel_acc$SubSkill[df_vel_acc$Skill == "dynamics" & df_vel_acc$Interval == i] <- "PtoF"
}

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    df_vel$Grouping[df_vel$Condition == ls_grouping$Condition[cond] & df_vel$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    df_vel_acc$Grouping[df_vel_acc$Condition == ls_grouping$Condition[cond] & df_vel_acc$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

####################################
### Remove outliers
####################################
# Exclude irrelevant notes
df_subset <- subset(df_vel, df_vel$Note != 33 & df_vel$Note != 66 & df_vel$Note != 67)
df_subset_acc <- subset(df_vel_acc, df_vel_acc$Interval != 32 & df_vel_acc$Interval != 33 & df_vel_acc$Interval != 65 & df_vel_acc$Interval != 66)

# Draw histogram
p_hist <- ggplot(df_subset, aes(x = Velocity, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

p_hist_acc <- ggplot(df_subset_acc, aes(x = Acc, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

# Exclude vel > +- 3SD (within a given condition)
vel_subskill <- aggregate(Velocity~SubSkill, data = df_subset,
                          FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_subskill <- cbind(vel_subskill, as.data.frame(vel_subskill[,2]))
df_trim_sd <- data.frame()
for (subskill in unique(df_subset$SubSkill)){
  upper = vel_subskill$mean[vel_subskill$SubSkill == subskill]+3*vel_subskill$sd[vel_subskill$SubSkill == subskill]
  lower = vel_subskill$mean[vel_subskill$SubSkill == subskill]-3*vel_subskill$sd[vel_subskill$SubSkill == subskill]
  df_current <- df_subset %>% dplyr::filter(SubSkill == subskill & Velocity < upper & Velocity > lower)
  df_trim_sd <- rbind(df_trim_sd, df_current)
}
print(sprintf("Velocity - Remove %i trials beyond +- 3SD", nrow(df_subset)-nrow(df_trim_sd)))

# Exclude vel_acc > +- 3SD (within a given condition)
vel_subskill_acc <- aggregate(Acc~SubSkill, data = df_subset_acc,
                          FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_subskill_acc <- cbind(vel_subskill_acc, as.data.frame(vel_subskill_acc[,2]))
df_trim_sd_acc <- data.frame()
for (subskill in unique(df_subset_acc$SubSkill)){
  upper = vel_subskill_acc$mean[vel_subskill_acc$SubSkill == subskill]+3*vel_subskill_acc$sd[vel_subskill_acc$SubSkill == subskill]
  lower = vel_subskill_acc$mean[vel_subskill_acc$SubSkill == subskill]-3*vel_subskill_acc$sd[vel_subskill_acc$SubSkill == subskill]
  df_current <- df_subset_acc %>% dplyr::filter(SubSkill == subskill & Acc < upper & Acc > lower)
  df_trim_sd_acc <- rbind(df_trim_sd_acc, df_current)
}
print(sprintf("Acc - Remove %i trials beyond +- 3SD", nrow(df_subset_acc)-nrow(df_trim_sd_acc)))

p_hist_sd <- ggplot(df_trim_sd, aes(x = Velocity, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

p_box_sd <- ggboxplot(df_trim_sd, x = "Skill", y = "Velocity", color = "Condition")
p_box_sd <- ggpar(p_box_sd, ylab = "Velocity (1-127)")

p_hist_sd_acc <- ggplot(df_trim_sd_acc, aes(x = Acc, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

p_box_sd_acc <- ggboxplot(df_trim_sd_acc, x = "Skill", y = "Acc", color = "Condition")
p_box_sd_acc <- ggpar(p_box_sd_acc, ylab = "Acceleration")

# png files
ggsave("./plot/vel/p_hist.png", plot = p_hist, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/p_hist_acc.png", plot = p_hist_acc, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/p_hist_sd.png", plot = p_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/p_box_sd.png", plot = p_box_sd, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/p_hist_sd_acc.png", plot = p_hist_sd_acc, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/p_box_sd_acc.png", plot = p_box_sd_acc, dpi = 600, width = 5, height = 4)

# Export a csv file for df_trimmed
write.csv(df_trim_sd, file = "./trimmed/data_vel.csv", row.names = F)
write.csv(df_trim_sd_acc, file = "./trimmed/data_vel_acc.csv", row.names = F)

####################################
# Aggregate data
####################################
df_trim_vel <- read.csv("./trimmed/data_vel.csv", header = T, sep = ",", dec = ".") # read a trimmed csv
df_trim_vel_acc <- read.csv("./trimmed/data_vel_acc.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

### Average data
# Overall average
vel <- aggregate(Velocity~Condition*Skill, data = df_trim_vel, 
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for SubSkill
vel_sub <- aggregate(Velocity~Condition*Skill*SubSkill, data = df_trim_vel,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_acc_change <- aggregate(Acc~Condition*Skill*SubSkill, data = subset(df_trim_vel_acc, df_trim_vel_acc$Interval == 8 | df_trim_vel_acc$Interval == 16 | df_trim_vel_acc$Interval == 24 | df_trim_vel_acc$Interval == 41
                                                                        | df_trim_vel_acc$Interval == 49 | df_trim_vel_acc$Interval == 57),
                            FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for the first note
vel_first <- aggregate(Velocity~Condition*Skill*SubSkill, data = subset(df_trim_vel, df_trim_vel$Note == 1 | df_trim_vel$Note == 9 | df_trim_vel$Note == 17 | df_trim_vel$Note == 25
                                                                        | df_trim_vel$Note == 34 | df_trim_vel$Note == 42 | df_trim_vel$Note == 50 | df_trim_vel$Note == 58),
                       FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
vel_seq <- aggregate(Velocity~Note*Condition*Skill, data = df_trim_vel, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_acc_seq <- aggregate(Acc~Interval*Condition*Skill, data = df_trim_vel_acc, 
                         FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Descriptive stats
vel <- cbind(vel, as.data.frame(vel[,3]))
vel_sub <- cbind(vel_sub, as.data.frame(vel_sub[,4]))
vel_acc_change <- cbind(vel_acc_change, as.data.frame(vel_acc_change[,4]))
vel_first <- cbind(vel_first, as.data.frame(vel_first[,4]))
vel_seq <- cbind(vel_seq, as.data.frame(vel_seq[,4]))
vel_acc_seq <- cbind(vel_acc_seq, as.data.frame(vel_acc_seq[,4]))

# Add a grouping name
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    vel$Grouping[vel$Condition == ls_grouping$Condition[cond] & vel$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], "-", ls_grouping$Skill[skill], sep = "")
    vel_sub$Grouping[vel_sub$Condition == ls_grouping$Condition[cond] & vel_sub$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], "-", ls_grouping$Skill[skill], sep = "")
    vel_acc_change$Grouping[vel_acc_change$Condition == ls_grouping$Condition[cond] & vel_acc_change$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], "-", ls_grouping$Skill[skill], sep = "")
    vel_seq$Grouping[vel_seq$Condition == ls_grouping$Condition[cond] & vel_seq$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], "-", ls_grouping$Skill[skill], sep = "")
    vel_acc_seq$Grouping[vel_acc_seq$Condition == ls_grouping$Condition[cond] & vel_acc_seq$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], "-", ls_grouping$Skill[skill], sep = "")
  }
}

# Add order info
vel_sub$Order[vel_sub$Skill == "articulation"] <- 1
vel_sub$Order[vel_sub$Skill == "dynamics"] <- 2
vel_acc_change$Order[vel_acc_change$Skill == "articulation"] <- 1
vel_acc_change$Order[vel_acc_change$Skill == "dynamics"] <- 2
vel_first$Order[vel_first$Skill == "articulation"] <- 1
vel_first$Order[vel_first$Skill == "dynamics"] <- 2

####################################
### Velocity plots
####################################
p_vel <- ggplot(data = vel, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = .2, position=position_dodge(.9)) + 
  labs(y = "Velocity (0-127)") + coord_cartesian(ylim = c(40, 80)) + 
  theme_classic()
p_vel

p_vel_sub <- ggplot(data = vel_sub, aes(x = reorder(SubSkill, Order), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "Velocity (0-127)") + coord_cartesian(ylim = c(40, 90)) + 
  theme_classic()
p_vel_sub

p_vel_acc_change <- ggplot(data = vel_acc_change, aes(x = reorder(SubSkill, Order), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(x = "Skill change", y = "Acceleration") + #coord_cartesian(ylim = c(0, 20)) + 
  theme_classic()
p_vel_acc_change

p_vel_first <- ggplot(data = vel_first, aes(x = reorder(SubSkill, Order), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "Velocity (0-127)") + #coord_cartesian(ylim = c(40, 90)) + 
  theme_classic()
p_vel_first

p_vel_seq <- ggplot(data = vel_seq, aes(x = Note, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(0.05)) + 
  labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,67,1)) +
  theme_classic()
p_vel_seq

p_vel_acc_seq <- ggplot(data = vel_acc_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "Acceleration") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()
p_vel_acc_seq

# Save plots
# png files
ggsave("./plot/vel/p_vel.png", plot = p_vel, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/p_vel_sub.png", plot = p_vel_sub, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/p_vel_acc_change.png", plot = p_vel_acc_change, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/p_vel_first.png", plot = p_vel_first, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/p_vel_seq.png", plot = p_vel_seq, dpi = 600, width = 15, height = 4)
ggsave("./plot/vel/p_vel_acc_seq.png", plot = p_vel_acc_seq, dpi = 600, width = 15, height = 4) 

####################################
### Statistics
####################################
# Two-way ANOVA
# vel
vel_anova <- ezANOVA(
  data = df_trim_vel
  , dv = .(Velocity)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(vel_anova)
write.csv(vel_anova$ANOVA, file = "./stats/vel_anova.csv")

# posthoc comparison
vel_posthoc <- aov(Velocity~Condition*Skill, data = df_trim_vel)
vel_posthoc <- TukeyHSD(vel_posthoc)
write.csv(vel_posthoc$`Condition:Skill`, file = "./stats/vel_posthoc.csv")

# vel_sub
vel_sub_anova <- ezANOVA(
  data = df_trim_vel
  , dv = .(Velocity)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(vel_sub_anova)
write.csv(vel_sub_anova$ANOVA, file = "./stats/vel_sub_anova.csv")

vel_sub_posthoc <- aov(Velocity~Condition*SubSkill, data = df_trim_vel)
vel_sub_posthoc <- TukeyHSD(vel_sub_posthoc)
write.csv(vel_sub_posthoc$`Condition:SubSkill`, file = "./stats/vel_sub_posthoc.csv")

# # vel_first CONSIDER LATER should be firstEnd?
# vel_first_anova <- ezANOVA(
#   data = subset(df_trim_vel, df_trim_vel$Note == 1 | df_trim_vel$Note == 9 | df_trim_vel$Note == 17 | df_trim_vel$Note == 25
#                       | df_trim_vel$Note == 34 | df_trim_vel$Note == 42 | df_trim_vel$Note == 50 | df_trim_vel$Note == 58)
#   , dv = .(Velocity)
#   , wid = .(SubNr)
#   , within = .(Condition, SubSkill)
#   , type = 3
#   , detailed = TRUE
# )
# print(vel_first_anova)
# 
# vel_firts_posthoc <- aov(Velocity~Condition*SubSkill, data = df_trim_vel)
# print(TukeyHSD(vel_first_posthoc))

# vel_acc_change
vel_acc_change_anova <- ezANOVA(
  data = subset(df_trim_vel_acc, df_trim_vel_acc$Interval == 8 | df_trim_vel_acc$Interval == 16 | df_trim_vel_acc$Interval == 24 | 
                  df_trim_vel_acc$Interval == 41 | df_trim_vel_acc$Interval == 49 | df_trim_vel_acc$Interval == 57)
  , dv = .(Acc)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(vel_acc_change_anova)
write.csv(vel_acc_change_anova$ANOVA, file = "./stats/vel_acc_change_anova.csv")
write.csv(vel_acc_change_anova$`Mauchly's Test for Sphericity`, file = "./stats/vel_acc_change_anova_mau.csv")
write.csv(vel_acc_change_anova$`Sphericity Corrections`, file = "./stats/vel_acc_change_anova_sph.csv")

vel_acc_change_posthoc <- aov(Acc~Condition*SubSkill, data = subset(df_trim_vel_acc, df_trim_vel_acc$Interval == 8 | df_trim_vel_acc$Interval == 16 | df_trim_vel_acc$Interval == 24 | 
                                                                      df_trim_vel_acc$Interval == 41 | df_trim_vel_acc$Interval == 49 | df_trim_vel_acc$Interval == 57))
vel_acc_change_posthoc <- TukeyHSD(vel_acc_change_posthoc)
write.csv(vel_acc_change_posthoc$`Condition:SubSkill`, file = "./stats/vel_acc_change_posthoc.csv")

#Normality
q_plot_vel <- ggplot(df_trim_vel, aes(sample = Velocity, shape = SubSkill, color = SubSkill)) +
  stat_qq() + theme_classic()
q_plot_vel

q_plot_vel_acc <- ggplot(subset(df_trim_vel_acc, df_trim_vel_acc$Interval != 8 & df_trim_vel_acc$Interval != 16 & df_trim_vel_acc$Interval != 24 & 
                              df_trim_vel_acc$Interval != 41 & df_trim_vel_acc$Interval != 49 & df_trim_vel_acc$Interval != 57), aes(sample = Acc, shape = SubSkill, color = SubSkill)) +
  stat_qq() + theme_classic()
q_plot_vel_acc

# Save plots
# png files
ggsave("./plot/vel/q_plot_vel.png", plot = q_plot_vel, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/q_plot_vel_acc.png", plot = q_plot_vel_acc, dpi = 600, width = 5, height = 4)