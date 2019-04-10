#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 26/02/2019
# This script aggregate and plot data (KOT)
# GitHub repo (private): https://github.com/atsukotominaga/expertpiano/tree/master/script/R 

####################################
#  Requirements
####################################
# !!! Set working directory to file source location !!!

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
# kot
if (!file.exists("plot/kot/")){
  dir.create("plot/kot")
}
# kot/withoutOrder
if (!file.exists("plot/kot/withoutOrder")){
  dir.create("plot/kot/withoutOrder")
}
# kot/withOrder
if (!file.exists("plot/kot/withOrder")){
  dir.create("plot/kot/withOrder")
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
df_trim_kot <- read.csv("./trimmed/data_kot.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

####################################
# Aggregate data
####################################
# Overall average
kot <- aggregate(KOT~Condition*Skill, data = df_trim_kot,
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for SubSkill
kot_change <- aggregate(KOT~Condition*Skill*SubSkill, data = subset(df_trim_kot, df_trim_kot$Interval == 8 | df_trim_kot$Interval == 16 | df_trim_kot$Interval == 24 |
                                                                   df_trim_kot$Interval == 41 | df_trim_kot$Interval == 49 | df_trim_kot$Interval == 57),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

kot_sub <- aggregate(KOT~Condition*Skill*SubSkill, data = subset(df_trim_kot, df_trim_kot$Interval != 8 & df_trim_kot$Interval != 16 & df_trim_kot$Interval != 24 &
                                                                      df_trim_kot$Interval != 41 & df_trim_kot$Interval != 49 & df_trim_kot$Interval != 57),
                        FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

kot_firstEnd <- aggregate(KOT~Condition*Skill*SubSkill, data = subset(df_trim_kot, df_trim_kot$Interval == 1 | df_trim_kot$Interval == 7 | df_trim_kot$Interval == 9 | df_trim_kot$Interval == 15 | df_trim_kot$Interval == 17 | df_trim_kot$Interval == 23
                                                                      | df_trim_kot$Interval == 25 | df_trim_kot$Interval == 31 | df_trim_kot$Interval == 34 | df_trim_kot$Interval == 40 | df_trim_kot$Interval == 42 | df_trim_kot$Interval == 48
                                                                      | df_trim_kot$Interval == 50 | df_trim_kot$Interval == 56 | df_trim_kot$Interval == 58 | df_trim_kot$Interval == 64),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
kot_seq <- aggregate(KOT~Interval*Condition*Skill, data = df_trim_kot, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Descriptive stats
kot <- cbind(kot, as.data.frame(kot[,3]))
kot_change <- cbind(kot_change, as.data.frame(kot_change[,4]))
kot_sub <- cbind(kot_sub, as.data.frame(kot_sub[,4]))
kot_firstEnd <- cbind(kot_firstEnd, as.data.frame(kot_firstEnd[,4]))
kot_seq <- cbind(kot_seq, as.data.frame(kot_seq[,4]))

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    kot$Grouping[kot$Condition == ls_grouping$Condition[cond] & kot$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    kot_change$Grouping[kot_change$Condition == ls_grouping$Condition[cond] & kot_change$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    kot_sub$Grouping[kot_sub$Condition == ls_grouping$Condition[cond] & kot_sub$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    kot_firstEnd$Grouping[kot_firstEnd$Condition == ls_grouping$Condition[cond] & kot_firstEnd$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    kot_seq$Grouping[kot_seq$Condition == ls_grouping$Condition[cond] & kot_seq$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

# Add order info
kot_change$Order[kot_change$Skill == "articulation"] <- 1
kot_change$Order[kot_change$Skill == "dynamics"] <- 2
kot_sub$Order[kot_sub$Skill == "articulation"] <- 1
kot_sub$Order[kot_sub$Skill == "dynamics"] <- 2
kot_firstEnd$Order[kot_firstEnd$Skill == "articulation"] <- 1
kot_firstEnd$Order[kot_firstEnd$Skill == "dynamics"] <- 2

####################################
# Plots
####################################
p_kot <- ggplot(data = kot, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean KOI (ms)") + #coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_kot

p_kot_change <- ggplot(data = kot_change, aes(x = reorder(SubSkill, Order), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_kot_change

p_kot_sub <- ggplot(data = kot_sub, aes(x = reorder(SubSkill, Order), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_kot_sub

p_kot_firstEnd <- ggplot(data = kot_firstEnd, aes(x = reorder(SubSkill, Order), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_kot_firstEnd

p_kot_seq <- ggplot(data = kot_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "Mean KOT (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()
p_kot_seq

# Save plots
# png files
ggsave("./plot/kot/withoutOrder/p_kot.png", plot = p_kot, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/withoutOrder/p_kot_change.png", plot = p_kot_change, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/withoutOrder/p_kot_sub.png", plot = p_kot_sub, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/withoutOrder/p_kot_firstEnd.png", plot = p_kot_firstEnd, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/withoutOrder/p_kot_seq.png", plot = p_kot_seq, dpi = 600, width = 15, height = 4)

####################################
### Statistics
####################################
# Two-way ANOVA
# kot
kot_anova <- ezANOVA(
  data = df_trim_kot
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(kot_anova)
write.csv(kot_anova$ANOVA, file = "./stats/withoutOrder/kot_anova.csv")

# posthoc comparison
kot_posthoc <- aov(KOT~Condition*Skill, data = df_trim_kot)
kot_posthoc <-TukeyHSD(kot_posthoc)
write.csv(kot_posthoc$`Condition:Skill`, file = "./stats/withoutOrder/kot_posthoc.csv")

# kot_change
kot_change_anova <- ezANOVA(
  data = subset(df_trim_kot, df_trim_kot$Interval == 8 | df_trim_kot$Interval == 16 | df_trim_kot$Interval == 24 | 
                  df_trim_kot$Interval == 41 | df_trim_kot$Interval == 49 | df_trim_kot$Interval == 57)
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(kot_change_anova)
write.csv(kot_change_anova$ANOVA, file = "./stats/withoutOrder/kot_change_anova.csv")
write.csv(kot_change_anova$`Mauchly's Test for Sphericity`, file = "./stats/withoutOrder/kot_change_anova_mau.csv")
write.csv(kot_change_anova$`Sphericity Corrections`, file = "./stats/withoutOrder/kot_change_anova_sph.csv")

# posthoc comparison
kot_change_posthoc <- aov(KOT~Condition*Skill, data = df_trim_kot)
kot_change_posthoc <-TukeyHSD(kot_change_posthoc)
write.csv(kot_change_posthoc$`Condition:Skill`, file = "./stats/withoutOrder/kot_change_posthoc.csv")

# kot_sub
kot_sub_anova <- ezANOVA(
  data = subset(df_trim_kot, df_trim_kot$Interval != 8 & df_trim_kot$Interval != 16 & df_trim_kot$Interval != 24 & 
                  df_trim_kot$Interval != 41 & df_trim_kot$Interval != 49 & df_trim_kot$Interval != 57)
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(kot_sub_anova)
write.csv(kot_sub_anova$ANOVA, file = "./stats/withoutOrder/kot_sub_anova.csv")
write.csv(kot_sub_anova$`Mauchly's Test for Sphericity`, file = "./stats/withoutOrder/kot_sub_anova_mau.csv")
write.csv(kot_sub_anova$`Sphericity Corrections`, file = "./stats/withoutOrder/kot_sub_anova_sph.csv")

kot_sub_posthoc <- aov(KOT~Condition*SubSkill, data = subset(df_trim_kot, df_trim_kot$Interval != 8 & df_trim_kot$Interval != 16 & df_trim_kot$Interval != 24 & 
                                                               df_trim_kot$Interval != 41 & df_trim_kot$Interval != 49 & df_trim_kot$Interval != 57))
kot_sub_posthoc <- TukeyHSD(kot_sub_posthoc)
write.csv(kot_sub_posthoc$`Condition:SubSkill`, file = "./stats/withoutOrder/kot_sub_posthoc.csv")

# kot_firstEnd
kot_firstEnd_anova <- ezANOVA(
  data = subset(df_trim_kot, df_trim_kot$Interval == 1 | df_trim_kot$Interval == 7 | df_trim_kot$Interval == 9 | df_trim_kot$Interval == 15 | df_trim_kot$Interval == 17 | df_trim_kot$Interval == 23
                | df_trim_kot$Interval == 25 | df_trim_kot$Interval == 31 | df_trim_kot$Interval == 34 | df_trim_kot$Interval == 40 | df_trim_kot$Interval == 42 | df_trim_kot$Interval == 48
                | df_trim_kot$Interval == 50 | df_trim_kot$Interval == 56 | df_trim_kot$Interval == 58 | df_trim_kot$Interval == 64)
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(kot_firstEnd_anova)
write.csv(kot_firstEnd_anova$ANOVA, file = "./stats/withoutOrder/kot_firstEnd_anova.csv")
write.csv(kot_firstEnd_anova$`Mauchly's Test for Sphericity`, file = "./stats/withoutOrder/kot_firstEnd_anova_mau.csv")
write.csv(kot_firstEnd_anova$`Sphericity Corrections`, file = "./stats/withoutOrder/kot_firstEnd_anova_sph.csv")

# posthoc comparison
kot_firstEnd_posthoc <- aov(KOT~Condition*SubSkill, data = subset(df_trim_kot, df_trim_kot$Interval == 1 | df_trim_kot$Interval == 7 | df_trim_kot$Interval == 9 | df_trim_kot$Interval == 15 | df_trim_kot$Interval == 17 | df_trim_kot$Interval == 23
                                                               | df_trim_kot$Interval == 25 | df_trim_kot$Interval == 31 | df_trim_kot$Interval == 34 | df_trim_kot$Interval == 40 | df_trim_kot$Interval == 42 | df_trim_kot$Interval == 48
                                                               | df_trim_kot$Interval == 50 | df_trim_kot$Interval == 56 | df_trim_kot$Interval == 58 | df_trim_kot$Interval == 64))
kot_firstEnd_posthoc <-TukeyHSD(kot_firstEnd_posthoc)
write.csv(kot_firstEnd_posthoc$`Condition:SubSkill`, file = "./stats/withoutOrder/kot_firstEnd_posthoc.csv")

### Normality
q_plot <- ggplot(subset(df_trim_kot, df_trim_kot$Interval != 8 & df_trim_kot$Interval != 16 & df_trim_kot$Interval != 24 & 
                          df_trim_kot$Interval != 41 & df_trim_kot$Interval != 49 & df_trim_kot$Interval != 57), aes(sample = KOT, shape = SubSkill, color = SubSkill)) +
  stat_qq() + theme_classic()
q_plot

# Save plots
# png files
ggsave("./plot/kot/q_plot.png", plot = q_plot, dpi = 600, width = 5, height = 4)

####################################
# Aggregate data (including Order)
####################################
df_trim_kot <- read.csv("./trimmed/data_kot.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

# Overall average
kot <- aggregate(KOT~Condition*Skill*Order, data = df_trim_kot,
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for SubSkill
kot_change <- aggregate(KOT~Condition*Skill*SubSkill*Order, data = subset(df_trim_kot, df_trim_kot$Interval == 8 | df_trim_kot$Interval == 16 | df_trim_kot$Interval == 24 |
                                                                      df_trim_kot$Interval == 41 | df_trim_kot$Interval == 49 | df_trim_kot$Interval == 57),
                        FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

kot_sub <- aggregate(KOT~Condition*Skill*SubSkill*Order, data = subset(df_trim_kot, df_trim_kot$Interval != 8 & df_trim_kot$Interval != 16 & df_trim_kot$Interval != 24 &
                                                                   df_trim_kot$Interval != 41 & df_trim_kot$Interval != 49 & df_trim_kot$Interval != 57),
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

kot_firstEnd <- aggregate(KOT~Condition*Skill*SubSkill*Order, data = subset(df_trim_kot, df_trim_kot$Interval == 1 | df_trim_kot$Interval == 7 | df_trim_kot$Interval == 9 | df_trim_kot$Interval == 15 | df_trim_kot$Interval == 17 | df_trim_kot$Interval == 23
                                                                      | df_trim_kot$Interval == 25 | df_trim_kot$Interval == 31 | df_trim_kot$Interval == 34 | df_trim_kot$Interval == 40 | df_trim_kot$Interval == 42 | df_trim_kot$Interval == 48
                                                                      | df_trim_kot$Interval == 50 | df_trim_kot$Interval == 56 | df_trim_kot$Interval == 58 | df_trim_kot$Interval == 64),
                          FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
kot_seq <- aggregate(KOT~Interval*Condition*Skill*Order, data = df_trim_kot, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Descriptive stats
kot <- cbind(kot, as.data.frame(kot[,4]))
kot_change <- cbind(kot_change, as.data.frame(kot_change[,5]))
kot_sub <- cbind(kot_sub, as.data.frame(kot_sub[,5]))
kot_firstEnd <- cbind(kot_firstEnd, as.data.frame(kot_firstEnd[,5]))
kot_seq <- cbind(kot_seq, as.data.frame(kot_seq[,5]))

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    kot$Grouping[kot$Condition == ls_grouping$Condition[cond] & kot$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    kot_change$Grouping[kot_change$Condition == ls_grouping$Condition[cond] & kot_change$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    kot_sub$Grouping[kot_sub$Condition == ls_grouping$Condition[cond] & kot_sub$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    kot_firstEnd$Grouping[kot_firstEnd$Condition == ls_grouping$Condition[cond] & kot_firstEnd$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    kot_seq$Grouping[kot_seq$Condition == ls_grouping$Condition[cond] & kot_seq$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

# Add order info
kot_change$LabelOrder[kot_change$Skill == "articulation"] <- 1
kot_change$LabelOrder[kot_change$Skill == "dynamics"] <- 2
kot_sub$LabelOrder[kot_sub$Skill == "articulation"] <- 1
kot_sub$LabelOrder[kot_sub$Skill == "dynamics"] <- 2
kot_firstEnd$LabelOrder[kot_firstEnd$Skill == "articulation"] <- 1
kot_firstEnd$LabelOrder[kot_firstEnd$Skill == "dynamics"] <- 2

####################################
# Plots (including Order)
####################################
p_kot <- ggplot(data = kot, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  facet_wrap(Order ~ .) +
  labs(y = "Mean KOI (ms)") + #coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_kot

p_kot_change <- ggplot(data = kot_change, aes(x = reorder(SubSkill, LabelOrder), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  facet_wrap(Order ~ .) +
  labs(x = "SubSkill", y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_kot_change

p_kot_sub <- ggplot(data = kot_sub, aes(x = reorder(SubSkill, LabelOrder), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  facet_wrap(Order ~ .) +
  labs(x = "SubSkill", y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_kot_sub

p_kot_firstEnd <- ggplot(data = kot_firstEnd, aes(x = reorder(SubSkill, LabelOrder), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  facet_wrap(Order ~ .) +
  labs(x = "SubSkill", y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_kot_firstEnd

p_kot_seq <- ggplot(data = kot_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  facet_grid(Skill ~ Order) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) +
  labs(x = "Interval", y = "Mean KOT (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()
p_kot_seq

# Save plots
# png files
ggsave("./plot/kot/withOrder/p_kot.png", plot = p_kot, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/withOrder/p_kot_change.png", plot = p_kot_change, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/withOrder/p_kot_sub.png", plot = p_kot_sub, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/withOrder/p_kot_firstEnd.png", plot = p_kot_firstEnd, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/withOrder/p_kot_seq.png", plot = p_kot_seq, dpi = 600, width = 15, height = 7)

####################################
# Statistics
# (including Order as a between factor)
####################################
# Two-way ANOVA
# kot
kot_anova <- ezANOVA(
  data = df_trim_kot
  , dv = .(KOT)
  , wid = .(SubNr)
  , between = .(Order)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(kot_anova)
write.csv(kot_anova$ANOVA, file = "./stats/withOrder/kot_anova.csv")

# posthoc comparison
kot_posthoc <- aov(KOT~Condition*Skill, data = df_trim_kot)
kot_posthoc <-TukeyHSD(kot_posthoc)
write.csv(kot_posthoc$`Condition:Skill`, file = "./stats/withOrder/kot_posthoc.csv")

# kot_change
kot_change_anova <- ezANOVA(
  data = subset(df_trim_kot, df_trim_kot$Interval == 8 | df_trim_kot$Interval == 16 | df_trim_kot$Interval == 24 | 
                  df_trim_kot$Interval == 41 | df_trim_kot$Interval == 49 | df_trim_kot$Interval == 57)
  , dv = .(KOT)
  , wid = .(SubNr)
  , between = .(Order)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(kot_change_anova)
write.csv(kot_change_anova$ANOVA, file = "./stats/withOrder/kot_change_anova.csv")
write.csv(kot_change_anova$`Mauchly's Test for Sphericity`, file = "./stats/withOrder/kot_change_anova_mau.csv")
write.csv(kot_change_anova$`Sphericity Corrections`, file = "./stats/withOrder/kot_change_anova_sph.csv")

# posthoc comparison
kot_change_posthoc <- aov(KOT~Condition*Skill, data = df_trim_kot)
kot_change_posthoc <-TukeyHSD(kot_change_posthoc)
write.csv(kot_change_posthoc$`Condition:Skill`, file = "./stats/withOrder/kot_change_posthoc.csv")

# kot_sub
kot_sub_anova <- ezANOVA(
  data = subset(df_trim_kot, df_trim_kot$Interval != 8 & df_trim_kot$Interval != 16 & df_trim_kot$Interval != 24 & 
                  df_trim_kot$Interval != 41 & df_trim_kot$Interval != 49 & df_trim_kot$Interval != 57)
  , dv = .(KOT)
  , wid = .(SubNr)
  , between = .(Order)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(kot_sub_anova)
write.csv(kot_sub_anova$ANOVA, file = "./stats/withOrder/kot_sub_anova.csv")
write.csv(kot_sub_anova$`Mauchly's Test for Sphericity`, file = "./stats/withOrder/kot_sub_anova_mau.csv")
write.csv(kot_sub_anova$`Sphericity Corrections`, file = "./stats/withOrder/kot_sub_anova_sph.csv")

kot_sub_posthoc <- aov(KOT~Condition*SubSkill, data = subset(df_trim_kot, df_trim_kot$Interval != 8 & df_trim_kot$Interval != 16 & df_trim_kot$Interval != 24 & 
                                                               df_trim_kot$Interval != 41 & df_trim_kot$Interval != 49 & df_trim_kot$Interval != 57))
kot_sub_posthoc <- TukeyHSD(kot_sub_posthoc)
write.csv(kot_sub_posthoc$`Condition:SubSkill`, file = "./stats/withOrder/kot_sub_posthoc.csv")

# kot_firstEnd
kot_firstEnd_anova <- ezANOVA(
  data = subset(df_trim_kot, df_trim_kot$Interval == 1 | df_trim_kot$Interval == 7 | df_trim_kot$Interval == 9 | df_trim_kot$Interval == 15 | df_trim_kot$Interval == 17 | df_trim_kot$Interval == 23
                | df_trim_kot$Interval == 25 | df_trim_kot$Interval == 31 | df_trim_kot$Interval == 34 | df_trim_kot$Interval == 40 | df_trim_kot$Interval == 42 | df_trim_kot$Interval == 48
                | df_trim_kot$Interval == 50 | df_trim_kot$Interval == 56 | df_trim_kot$Interval == 58 | df_trim_kot$Interval == 64)
  , dv = .(KOT)
  , wid = .(SubNr)
  , between = .(Order)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(kot_firstEnd_anova)
write.csv(kot_firstEnd_anova$ANOVA, file = "./stats/withOrder/kot_firstEnd_anova.csv")
write.csv(kot_firstEnd_anova$`Mauchly's Test for Sphericity`, file = "./stats/withOrder/kot_firstEnd_anova_mau.csv")
write.csv(kot_firstEnd_anova$`Sphericity Corrections`, file = "./stats/withOrder/kot_firstEnd_anova_sph.csv")

# posthoc comparison
kot_firstEnd_posthoc <- aov(KOT~Condition*SubSkill, data = subset(df_trim_kot, df_trim_kot$Interval == 1 | df_trim_kot$Interval == 7 | df_trim_kot$Interval == 9 | df_trim_kot$Interval == 15 | df_trim_kot$Interval == 17 | df_trim_kot$Interval == 23
                                                                  | df_trim_kot$Interval == 25 | df_trim_kot$Interval == 31 | df_trim_kot$Interval == 34 | df_trim_kot$Interval == 40 | df_trim_kot$Interval == 42 | df_trim_kot$Interval == 48
                                                                  | df_trim_kot$Interval == 50 | df_trim_kot$Interval == 56 | df_trim_kot$Interval == 58 | df_trim_kot$Interval == 64))
kot_firstEnd_posthoc <-TukeyHSD(kot_firstEnd_posthoc)
write.csv(kot_firstEnd_posthoc$`Condition:SubSkill`, file = "./stats/withOrder/kot_firstEnd_posthoc.csv")