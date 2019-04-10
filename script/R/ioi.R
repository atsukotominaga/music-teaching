#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 26/02/2019
# This script aggregate and plot data (IOI)
# GitHub repo (private): https://github.com/atsukotominaga/expertpiano/tree/master/script/R 

####################################
#  Requirements
####################################
# !!! Set working directory to file source location !!!

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

# Create necessary folders if not exist
# plot
if (!file.exists("plot")){
  dir.create("plot")
}
# ioi
if (!file.exists("plot/ioi/")){
  dir.create("plot/ioi")
}
# ioi/withoutOrder
if (!file.exists("plot/ioi/withoutOrder")){
  dir.create("plot/ioi/withoutOrder")
}
# ioi/withOrder
if (!file.exists("plot/ioi/withOrder")){
  dir.create("plot/ioi/withOrder")
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
df_trim_ioi <- read.csv("./trimmed/data_ioi.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

####################################
# Aggregate data
####################################
# Overall average
ioi <- aggregate(IOI~Condition*Skill, data = df_trim_ioi,
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
# Overall average for skill change
ioi_change <- aggregate(IOI~Condition*Skill, data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 |
                                                                          df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57),
                            FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
# Average for SubSkill
ioi_change_sub <- aggregate(IOI~Condition*Skill*SubSkill, data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 |
                                                                   df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57),
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
ioi_seq <- aggregate(IOI~Interval*Condition*Skill, data = df_trim_ioi, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Variability (SD/mean IOI)
df_var <- data.frame()
for (subnr in unique(df_trim_ioi$SubNr)){
  for (block in unique(df_trim_ioi$BlockNr)){
    cond = as.character(unique(df_trim_ioi$Condition[df_trim_ioi$SubNr == subnr & df_trim_ioi$BlockNr == block]))
    skill = as.character(unique(df_trim_ioi$Skill[df_trim_ioi$SubNr == subnr & df_trim_ioi$BlockNr == block]))
    for (trial in unique(df_trim_ioi$TrialNr)){
      df_current <- df_trim_ioi %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      df_var <- rbind(df_var, data.frame(subnr, block, trial, cond, skill, sd(df_current$IOI)/mean(df_current$IOI)))
    }
  }
}
colnames(df_var) <- c("SubNr", "BlockNr", "TrialNr", "Condition", "Skill", "Variability")

ioi_var <- aggregate(Variability~Condition*Skill, data = df_var,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
ioi_var_trial <- aggregate(Variability~TrialNr*Condition*Skill, data = df_var,
                           FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Descriptive stats
ioi <- cbind(ioi, as.data.frame(ioi[,3]))
ioi_change <- cbind(ioi_change, as.data.frame(ioi_change[,3]))
ioi_change_sub <- cbind(ioi_change_sub, as.data.frame(ioi_change_sub[,4]))
ioi_seq <- cbind(ioi_seq, as.data.frame(ioi_seq[,4]))
ioi_var <- cbind(ioi_var, as.data.frame(ioi_var[,3]))
ioi_var_trial <- cbind(ioi_var_trial, as.data.frame(ioi_var_trial[,4]))

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    ioi$Grouping[ioi$Condition == ls_grouping$Condition[cond] & ioi$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_change$Grouping[ioi_change$Condition == ls_grouping$Condition[cond] & ioi_change$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_change_sub$Grouping[ioi_change_sub$Condition == ls_grouping$Condition[cond] & ioi_change_sub$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_seq$Grouping[ioi_seq$Condition == ls_grouping$Condition[cond] & ioi_seq$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_var$Grouping[ioi_var$Condition == ls_grouping$Condition[cond] & ioi_var$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_var_trial$Grouping[ioi_var_trial$Condition == ls_grouping$Condition[cond] & ioi_var_trial$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

# Add order info
ioi_change_sub$LabelOrder[ioi_change_sub$Skill == "articulation"] <- 1
ioi_change_sub$LabelOrder[ioi_change_sub$Skill == "dynamics"] <- 2
ioi_var$LabelOrder[ioi_var$Skill == "articulation"] <- 1
ioi_var$LabelOrder[ioi_var$Skill == "dynamics"] <- 2

####################################
# Plots
####################################
p_ioi <- ggplot(data = ioi, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean IOI (ms)") + coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_ioi

p_ioi_change <- ggplot(data = ioi_change, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(x = "Skill", y = "Mean IOI (ms)") + coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_ioi_change

p_ioi_change_sub <- ggplot(data = ioi_change_sub, aes(x = reorder(SubSkill, LabelOrder), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "Mean IOI (ms)") + coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_ioi_change_sub


p_ioi_seq <- ggplot(data = ioi_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 188, linetype = "dashed") + # Tempo
  facet_grid(Skill ~ .) +
  annotate("text", 0, 188, label = "Tempo (80bpm)", vjust = -1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "Mean IOI (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()
p_ioi_seq

p_ioi_var <- ggplot(data = ioi_var, aes(x = reorder(Skill, LabelOrder), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(x = "Skill", y = "Mean CV (SD/mean IOI)") + coord_cartesian(ylim = c(0, .1)) +
  theme_classic()
p_ioi_var

p_ioi_var_trial <- ggplot(data = ioi_var_trial, aes(x = TrialNr, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Trial Number", y = "Mean CV (SD/mean IOI)") + scale_x_continuous(breaks=seq(1,8,1)) +
  theme_classic()
p_ioi_var_trial

# Save plots
# png files
ggsave("./plot/ioi/withoutOrder/p_ioi.png", plot = p_ioi, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/withoutOrder/p_ioi_change.png", plot = p_ioi_change, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/withoutOrder/p_ioi_change_sub.png", plot = p_ioi_change_sub, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/withoutOrder/p_ioi_seq.png", plot = p_ioi_seq, dpi = 600, width = 15, height = 4)
ggsave("./plot/ioi/withoutOrder/p_ioi_var.png", plot = p_ioi_var, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/withoutOrder/p_ioi_var_trial.png", plot = p_ioi_var_trial, dpi = 600, width = 10, height = 4)

####################################
# Statistics
####################################
# Two-way ANOVA
# ioi
ioi_anova <- ezANOVA(
  data = df_trim_ioi
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(ioi_anova)
write.csv(ioi_anova$ANOVA, file = "./stats/withoutOrder/ioi_anova.csv")

# posthoc comparison
ioi_posthoc <- aov(IOI~Condition*Skill, data = df_trim_ioi)
ioi_posthoc <- TukeyHSD(ioi_posthoc)
print(ioi_posthoc)
write.csv(ioi_posthoc$`Condition:Skill`, file = "./stats/withoutOrder/ioi_posthoc.csv")

# ioi_change
ioi_change_anova <- ezANOVA(
  data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 | 
                  df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57)
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(ioi_change_anova)
write.csv(ioi_change_anova$ANOVA, file = "./stats/withoutOrder/ioi_change_anova.csv")

# posthoc comparison
ioi_change_posthoc <- aov(IOI~Condition*Skill, data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 |
                                                            df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57))
ioi_change_posthoc <- TukeyHSD(ioi_change_posthoc)
print(ioi_change_posthoc)
write.csv(ioi_change_posthoc$`Condition:Skill`, file = "./stats/withoutOrder/ioi_change_posthoc.csv")

# ioi_change_subskill
ioi_change_sub_anova <- ezANOVA(
  data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 | 
                  df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57)
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(ioi_change_sub_anova)
write.csv(ioi_change_sub_anova$ANOVA, file = "./stats/withoutOrder/ioi_change_sub_anova.csv")

ioi_change_sub_posthoc <- aov(IOI~Condition*SubSkill, data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 |
                                                                  df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57))
ioi_change_sub_posthoc <- TukeyHSD(ioi_change_sub_posthoc)
print(ioi_change_sub_posthoc)
write.csv(ioi_change_sub_posthoc$`Condition:SubSkill`, file = "./stats/withoutOrder/ioi_change_sub_posthoc.csv")

# ioi_var
ioi_var_anova <- ezANOVA(
  data = df_var[complete.cases(df_var),]
  , dv = .(Variability)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(ioi_var_anova)
write.csv(ioi_var_anova$ANOVA, file = "./stats/withoutOrder/ioi_var_anova.csv")

# posthoc comparison
ioi_var_posthoc <- aov(Variability~Condition*Skill, data = df_var[complete.cases(df_var),])
ioi_var_posthoc <- TukeyHSD(ioi_var_posthoc)
print(ioi_var_posthoc)
write.csv(ioi_var_posthoc$`Condition:Skill`, file = "./stats/withoutOrder/ioi_var_posthoc.csv")

### Normality
q_plot <- ggplot(df_trim_ioi, aes(sample = IOI, shape = Grouping, color = Grouping)) +
  stat_qq() + theme_classic()
q_plot

# Save plots
# png files
ggsave("./plot/ioi/q_plot.png", plot = q_plot, dpi = 600, width = 5, height = 4)

####################################
# Aggregate data (including Order)
####################################
df_trim_ioi <- read.csv("./trimmed/data_ioi.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

# Overall average
ioi <- aggregate(IOI~Condition*Skill*Order, data = df_trim_ioi,
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
# Overall average for skill change
ioi_change <- aggregate(IOI~Condition*Skill*Order, data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 |
                                                             df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57),
                        FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
# Average for SubSkill
ioi_change_sub <- aggregate(IOI~Condition*Skill*SubSkill*Order, data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 |
                                                                          df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57),
                            FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
ioi_seq <- aggregate(IOI~Interval*Condition*Skill*Order, data = df_trim_ioi, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Variability (SD/mean IOI)
df_var <- data.frame()
for (subnr in unique(df_trim_ioi$SubNr)){
  for (block in unique(df_trim_ioi$BlockNr)){
    cond = as.character(unique(df_trim_ioi$Condition[df_trim_ioi$SubNr == subnr & df_trim_ioi$BlockNr == block]))
    skill = as.character(unique(df_trim_ioi$Skill[df_trim_ioi$SubNr == subnr & df_trim_ioi$BlockNr == block]))
    order = unique(df_trim_ioi$Order[df_trim_ioi$SubNr == subnr & df_trim_ioi$BlockNr == block])
    for (trial in unique(df_trim_ioi$TrialNr)){
      df_current <- df_trim_ioi %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      df_var <- rbind(df_var, data.frame(subnr, block, trial, cond, skill, order, sd(df_current$IOI)/mean(df_current$IOI)))
    }
  }
}
colnames(df_var) <- c("SubNr", "BlockNr", "TrialNr", "Condition", "Skill", "Order", "Variability")

ioi_var <- aggregate(Variability~Condition*Skill*Order, data = df_var,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
ioi_var_trial <- aggregate(Variability~TrialNr*Condition*Skill*Order, data = df_var,
                           FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Descriptive stats
ioi <- cbind(ioi, as.data.frame(ioi[,4]))
ioi_change <- cbind(ioi_change, as.data.frame(ioi_change[,4]))
ioi_change_sub <- cbind(ioi_change_sub, as.data.frame(ioi_change_sub[,5]))
ioi_seq <- cbind(ioi_seq, as.data.frame(ioi_seq[,5]))
ioi_var <- cbind(ioi_var, as.data.frame(ioi_var[,4]))
ioi_var_trial <- cbind(ioi_var_trial, as.data.frame(ioi_var_trial[,5]))

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    ioi$Grouping[ioi$Condition == ls_grouping$Condition[cond] & ioi$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_change$Grouping[ioi_change$Condition == ls_grouping$Condition[cond] & ioi_change$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_change_sub$Grouping[ioi_change_sub$Condition == ls_grouping$Condition[cond] & ioi_change_sub$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_seq$Grouping[ioi_seq$Condition == ls_grouping$Condition[cond] & ioi_seq$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_var$Grouping[ioi_var$Condition == ls_grouping$Condition[cond] & ioi_var$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_var_trial$Grouping[ioi_var_trial$Condition == ls_grouping$Condition[cond] & ioi_var_trial$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

# Add order info
ioi_change_sub$LabelOrder[ioi_change_sub$Skill == "articulation"] <- 1
ioi_change_sub$LabelOrder[ioi_change_sub$Skill == "dynamics"] <- 2
ioi_var$LabelOrder[ioi_var$Skill == "articulation"] <- 1
ioi_var$LabelOrder[ioi_var$Skill == "dynamics"] <- 2

####################################
# Plots (including Order)
####################################
p_ioi <- ggplot(data = ioi, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  facet_wrap(Order ~ .) +
  labs(y = "Mean IOI (ms)") + coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_ioi

p_ioi_change <- ggplot(data = ioi_change, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  facet_wrap(Order ~ .) +
  labs(x = "Skill", y = "Mean IOI (ms)") + coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_ioi_change

p_ioi_change_sub <- ggplot(data = ioi_change_sub, aes(x = reorder(SubSkill, LabelOrder), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  facet_wrap(Order ~ .) +
  labs(x = "SubSkill", y = "Mean IOI (ms)") + coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_ioi_change_sub

p_ioi_seq <- ggplot(data = ioi_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 188, linetype = "dashed") + # Tempo
  facet_grid(Skill ~ Order) +
  annotate("text", 0, 188, label = "Tempo (80bpm)", vjust = -1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "Mean IOI (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()
p_ioi_seq

p_ioi_var <- ggplot(data = ioi_var, aes(x = reorder(Skill, LabelOrder), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  facet_wrap(Order ~ .) +
  labs(x = "Skill", y = "Mean CV (SD/mean IOI)") + coord_cartesian(ylim = c(0, .1)) +
  theme_classic()
p_ioi_var

p_ioi_var_trial <- ggplot(data = ioi_var_trial, aes(x = TrialNr, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) +
  facet_grid(. ~ Order) +
  labs(x = "Trial Number", y = "Mean CV (SD/mean IOI)") + scale_x_continuous(breaks=seq(1,8,1)) +
  theme_classic()
p_ioi_var_trial

# Save plots
# png files
ggsave("./plot/ioi/withOrder/p_ioi.png", plot = p_ioi, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/withOrder/p_ioi_change.png", plot = p_ioi_change, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/withOrder/p_ioi_change_sub.png", plot = p_ioi_change_sub, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/withOrder/p_ioi_seq.png", plot = p_ioi_seq, dpi = 600, width = 15, height = 7)
ggsave("./plot/ioi/withOrder/p_ioi_var.png", plot = p_ioi_var, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/withOrder/p_ioi_var_trial.png", plot = p_ioi_var_trial, dpi = 600, width = 10, height = 4)

####################################
# Statistics
# (including Order as a between factor)
####################################
# Two-way ANOVA
# ioi
ioi_anova <- ezANOVA(
  data = df_trim_ioi
  , dv = .(IOI)
  , wid = .(SubNr)
  , between = .(Order)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(ioi_anova)
write.csv(ioi_anova$ANOVA, file = "./stats/withOrder/ioi_anova.csv")

# posthoc comparison
ioi_posthoc <- aov(IOI~Condition*Skill, data = df_trim_ioi)
ioi_posthoc <- TukeyHSD(ioi_posthoc)
print(ioi_posthoc)
write.csv(ioi_posthoc$`Condition:Skill`, file = "./stats/withOrder/ioi_posthoc.csv")

# ioi_change
ioi_change_anova <- ezANOVA(
  data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 | 
                  df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57)
  , dv = .(IOI)
  , wid = .(SubNr)
  , between = .(Order)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(ioi_change_anova)
write.csv(ioi_change_anova$ANOVA, file = "./stats/withOrder/ioi_change_anova.csv")

# posthoc comparison
ioi_change_posthoc <- aov(IOI~Condition*Skill, data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 |
                                                               df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57))
ioi_change_posthoc <- TukeyHSD(ioi_change_posthoc)
print(ioi_change_posthoc)
write.csv(ioi_change_posthoc$`Condition:Skill`, file = "./stats/withOrder/ioi_change_posthoc.csv")

# ioi_change_subskill
ioi_change_sub_anova <- ezANOVA(
  data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 | 
                  df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57)
  , dv = .(IOI)
  , wid = .(SubNr)
  , between = .(Order)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(ioi_change_sub_anova)
write.csv(ioi_change_sub_anova$ANOVA, file = "./stats/withOrder/ioi_change_sub_anova.csv")

ioi_change_sub_posthoc <- aov(IOI~Condition*SubSkill, data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 |
                                                                      df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57))
ioi_change_sub_posthoc <- TukeyHSD(ioi_change_sub_posthoc)
print(ioi_change_sub_posthoc)
write.csv(ioi_change_sub_posthoc$`Condition:SubSkill`, file = "./stats/withOrder/ioi_change_sub_posthoc.csv")

# ioi_var
ioi_var_anova <- ezANOVA(
  data = df_var[complete.cases(df_var),]
  , dv = .(Variability)
  , wid = .(SubNr)
  , between = .(Order)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(ioi_var_anova)
write.csv(ioi_var_anova$ANOVA, file = "./stats/withOrder/ioi_var_anova.csv")

# posthoc comparison
ioi_var_posthoc <- aov(Variability~Condition*Skill, data = df_var[complete.cases(df_var),])
ioi_var_posthoc <- TukeyHSD(ioi_var_posthoc)
print(ioi_var_posthoc)
write.csv(ioi_var_posthoc$`Condition:Skill`, file = "./stats/withOrder/ioi_var_posthoc.csv")