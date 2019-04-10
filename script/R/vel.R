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
# vel
if (!file.exists("plot/vel/")){
  dir.create("plot/vel")
}
# vel/withoutOrder
if (!file.exists("plot/vel/withoutOrder")){
  dir.create("plot/vel/withoutOrder")
}
# vel/withOrder
if (!file.exists("plot/vel/withOrder")){
  dir.create("plot/vel/withOrder")
}

# stats
if (!file.exists("stats")){
  dir.create("stats")
}
# stats/withoutOrder
if (!file.exists("stats/withoutOrder")){
  dir.create("stats/withoutOrder")
}
# stats/withOrder
if (!file.exists("stats/withOrder")){
  dir.create("stats/withOrder")
}

####################################
# Reading and formatting data
####################################
df_trim_vel <- read.csv("./trimmed/data_vel.csv", header = T, sep = ",", dec = ".") # read a trimmed csv
df_trim_vel_acc <- read.csv("./trimmed/data_vel_acc.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

####################################
# Aggregate data
####################################
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
# Velocity plots
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
ggsave("./plot/vel/withoutOrder/p_vel.png", plot = p_vel, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/withoutOrder/p_vel_sub.png", plot = p_vel_sub, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/withoutOrder/p_vel_acc_change.png", plot = p_vel_acc_change, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/withoutOrder/p_vel_first.png", plot = p_vel_first, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/withoutOrder/p_vel_seq.png", plot = p_vel_seq, dpi = 600, width = 15, height = 4)
ggsave("./plot/vel/withoutOrder/p_vel_acc_seq.png", plot = p_vel_acc_seq, dpi = 600, width = 15, height = 4) 

####################################
# Statistics
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
write.csv(vel_anova$ANOVA, file = "./stats/withoutOrder/vel_anova.csv")

# posthoc comparison
vel_posthoc <- aov(Velocity~Condition*Skill, data = df_trim_vel)
vel_posthoc <- TukeyHSD(vel_posthoc)
write.csv(vel_posthoc$`Condition:Skill`, file = "./stats/withoutOrder/vel_posthoc.csv")

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
write.csv(vel_sub_anova$ANOVA, file = "./stats/withoutOrder/vel_sub_anova.csv")

vel_sub_posthoc <- aov(Velocity~Condition*SubSkill, data = df_trim_vel)
vel_sub_posthoc <- TukeyHSD(vel_sub_posthoc)
write.csv(vel_sub_posthoc$`Condition:SubSkill`, file = "./stats/withoutOrder/vel_sub_posthoc.csv")

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
write.csv(vel_acc_change_anova$ANOVA, file = "./stats/withoutOrder/vel_acc_change_anova.csv")
write.csv(vel_acc_change_anova$`Mauchly's Test for Sphericity`, file = "./stats/withoutOrder/vel_acc_change_anova_mau.csv")
write.csv(vel_acc_change_anova$`Sphericity Corrections`, file = "./stats/withoutOrder/vel_acc_change_anova_sph.csv")

vel_acc_change_posthoc <- aov(Acc~Condition*SubSkill, data = subset(df_trim_vel_acc, df_trim_vel_acc$Interval == 8 | df_trim_vel_acc$Interval == 16 | df_trim_vel_acc$Interval == 24 | 
                                                                      df_trim_vel_acc$Interval == 41 | df_trim_vel_acc$Interval == 49 | df_trim_vel_acc$Interval == 57))
vel_acc_change_posthoc <- TukeyHSD(vel_acc_change_posthoc)
write.csv(vel_acc_change_posthoc$`Condition:SubSkill`, file = "./stats/withoutOrder/vel_acc_change_posthoc.csv")

### Normality
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

####################################
# Aggregate data (including Order)
####################################
df_trim_vel <- read.csv("./trimmed/data_vel.csv", header = T, sep = ",", dec = ".") # read a trimmed csv
df_trim_vel_acc <- read.csv("./trimmed/data_vel_acc.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

### Average data
# Overall average
vel <- aggregate(Velocity~Condition*Skill*Order, data = df_trim_vel, 
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for SubSkill
vel_sub <- aggregate(Velocity~Condition*Skill*SubSkill*Order, data = df_trim_vel,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_acc_change <- aggregate(Acc~Condition*Skill*SubSkill*Order, data = subset(df_trim_vel_acc, df_trim_vel_acc$Interval == 8 | df_trim_vel_acc$Interval == 16 | df_trim_vel_acc$Interval == 24 | df_trim_vel_acc$Interval == 41
                                                                        | df_trim_vel_acc$Interval == 49 | df_trim_vel_acc$Interval == 57),
                            FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for the first note
vel_first <- aggregate(Velocity~Condition*Skill*SubSkill*Order, data = subset(df_trim_vel, df_trim_vel$Note == 1 | df_trim_vel$Note == 9 | df_trim_vel$Note == 17 | df_trim_vel$Note == 25
                                                                        | df_trim_vel$Note == 34 | df_trim_vel$Note == 42 | df_trim_vel$Note == 50 | df_trim_vel$Note == 58),
                       FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
vel_seq <- aggregate(Velocity~Note*Condition*Skill*Order, data = df_trim_vel, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
vel_acc_seq <- aggregate(Acc~Interval*Condition*Skill*Order, data = df_trim_vel_acc, 
                         FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Descriptive stats
vel <- cbind(vel, as.data.frame(vel[,4]))
vel_sub <- cbind(vel_sub, as.data.frame(vel_sub[,5]))
vel_acc_change <- cbind(vel_acc_change, as.data.frame(vel_acc_change[,5]))
vel_first <- cbind(vel_first, as.data.frame(vel_first[,5]))
vel_seq <- cbind(vel_seq, as.data.frame(vel_seq[,5]))
vel_acc_seq <- cbind(vel_acc_seq, as.data.frame(vel_acc_seq[,5]))

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
vel_sub$LabelOrder[vel_sub$Skill == "articulation"] <- 1
vel_sub$LabelOrder[vel_sub$Skill == "dynamics"] <- 2
vel_acc_change$LabelOrder[vel_acc_change$Skill == "articulation"] <- 1
vel_acc_change$LabelOrder[vel_acc_change$Skill == "dynamics"] <- 2
vel_first$LabelOrder[vel_first$Skill == "articulation"] <- 1
vel_first$LabelOrder[vel_first$Skill == "dynamics"] <- 2

####################################
# Velocity plots (including Order)
####################################
p_vel <- ggplot(data = vel, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = .2, position=position_dodge(.9)) +
  facet_wrap(Order ~ .) +
  labs(y = "Velocity (0-127)") + coord_cartesian(ylim = c(40, 80)) + 
  theme_classic()
p_vel

p_vel_sub <- ggplot(data = vel_sub, aes(x = reorder(SubSkill, LabelOrder), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  facet_wrap(Order ~ .) +
  labs(x = "SubSkill", y = "Velocity (0-127)") + coord_cartesian(ylim = c(40, 90)) + 
  theme_classic()
p_vel_sub

p_vel_acc_change <- ggplot(data = vel_acc_change, aes(x = reorder(SubSkill, LabelOrder), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  facet_wrap(Order ~ .) +
  labs(x = "Skill change", y = "Acceleration") + #coord_cartesian(ylim = c(0, 20)) + 
  theme_classic()
p_vel_acc_change

p_vel_first <- ggplot(data = vel_first, aes(x = reorder(SubSkill, LabelOrder), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  facet_wrap(Order ~ .) +
  labs(x = "SubSkill", y = "Velocity (0-127)") + #coord_cartesian(ylim = c(40, 90)) + 
  theme_classic()
p_vel_first

p_vel_seq <- ggplot(data = vel_seq, aes(x = Note, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ Order) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(0.05)) + 
  labs(y = "Velocity (0-127)") + scale_x_continuous(breaks=seq(1,67,1)) +
  theme_classic()
p_vel_seq

p_vel_acc_seq <- ggplot(data = vel_acc_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ Order) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "Acceleration") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()
p_vel_acc_seq

# Save plots
# png files
ggsave("./plot/vel/withOrder/p_vel.png", plot = p_vel, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/withOrder/p_vel_sub.png", plot = p_vel_sub, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/withOrder/p_vel_acc_change.png", plot = p_vel_acc_change, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/withOrder/p_vel_first.png", plot = p_vel_first, dpi = 600, width = 5, height = 4)
ggsave("./plot/vel/withOrder/p_vel_seq.png", plot = p_vel_seq, dpi = 600, width = 15, height = 7)
ggsave("./plot/vel/withOrder/p_vel_acc_seq.png", plot = p_vel_acc_seq, dpi = 600, width = 15, height = 7) 

####################################
# Statistics
# (including Order as a between factor)
####################################
# Two-way ANOVA
# vel
vel_anova <- ezANOVA(
  data = df_trim_vel
  , dv = .(Velocity)
  , wid = .(SubNr)
  , between = .(Order)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(vel_anova)
write.csv(vel_anova$ANOVA, file = "./stats/withOrder/vel_anova.csv")

# posthoc comparison
vel_posthoc <- aov(Velocity~Condition*Skill, data = df_trim_vel)
vel_posthoc <- TukeyHSD(vel_posthoc)
write.csv(vel_posthoc$`Condition:Skill`, file = "./stats/withOrder/vel_posthoc.csv")

# vel_sub
vel_sub_anova <- ezANOVA(
  data = df_trim_vel
  , dv = .(Velocity)
  , wid = .(SubNr)
  , between = .(Order)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(vel_sub_anova)
write.csv(vel_sub_anova$ANOVA, file = "./stats/withOrder/vel_sub_anova.csv")

vel_sub_posthoc <- aov(Velocity~Condition*SubSkill, data = df_trim_vel)
vel_sub_posthoc <- TukeyHSD(vel_sub_posthoc)
write.csv(vel_sub_posthoc$`Condition:SubSkill`, file = "./stats/withOrder/vel_sub_posthoc.csv")

# vel_acc_change
vel_acc_change_anova <- ezANOVA(
  data = subset(df_trim_vel_acc, df_trim_vel_acc$Interval == 8 | df_trim_vel_acc$Interval == 16 | df_trim_vel_acc$Interval == 24 | 
                  df_trim_vel_acc$Interval == 41 | df_trim_vel_acc$Interval == 49 | df_trim_vel_acc$Interval == 57)
  , dv = .(Acc)
  , wid = .(SubNr)
  , between = .(Order)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(vel_acc_change_anova)
write.csv(vel_acc_change_anova$ANOVA, file = "./stats/withOrder/vel_acc_change_anova.csv")
write.csv(vel_acc_change_anova$`Mauchly's Test for Sphericity`, file = "./stats/withOrder/vel_acc_change_anova_mau.csv")
write.csv(vel_acc_change_anova$`Sphericity Corrections`, file = "./stats/withOrder/vel_acc_change_anova_sph.csv")

vel_acc_change_posthoc <- aov(Acc~Condition*SubSkill, data = subset(df_trim_vel_acc, df_trim_vel_acc$Interval == 8 | df_trim_vel_acc$Interval == 16 | df_trim_vel_acc$Interval == 24 | 
                                                                      df_trim_vel_acc$Interval == 41 | df_trim_vel_acc$Interval == 49 | df_trim_vel_acc$Interval == 57))
vel_acc_change_posthoc <- TukeyHSD(vel_acc_change_posthoc)
write.csv(vel_acc_change_posthoc$`Condition:SubSkill`, file = "./stats/withOrder/vel_acc_change_posthoc.csv")