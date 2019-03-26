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

####################################
# Reading and formatting data
####################################
df_all <- read.csv("./processed/data_analysis.csv", header = T, sep = ",", dec = ".") # clear data without pitch errors
df_exc <- read.csv("./processed/data_errorRate.csv", header = T, sep = ",", dec = ".") # exclusion criteria

# Exclude participants
include <- df_exc$SubNr[df_exc$LessThan10 == "include"]

# Data frame with only included participants
df_analysis <- data.frame()
for (subnr in include){
  df_current <- df_all %>% dplyr::filter(SubNr == subnr)
  df_analysis <- rbind(df_analysis, df_current)
}

####################################
# Key Overlap Time - articulation
####################################
df_onset <- df_analysis %>% dplyr::filter(Key_OnOff == 1)
df_offset <- df_analysis %>% dplyr::filter(Key_OnOff == 0)

# Offset 1 - Onset 2
df_onset$KOT <- NA
for (row in 1:length(df_onset$NoteNr)){
  if (row < length(df_onset$NoteNr)){
    df_onset$KOT[row+1] <- df_offset$TimeStamp[row] - df_onset$TimeStamp[row+1]
  }
}

# Remove the first note
df_kot <- df_onset %>% dplyr::filter(NoteNr != 17)

# Assign a sequence number for each tone
df_kot$Interval <- rep(1:66, length(df_kot$NoteNr)/66)

### Define SubSkills
# For intervals (for df_kot)
ls_legato <- list(c(1:7), c(17:23), c(42:48), c(58:64))
ls_staccato <- list(c(9:15), c(25:31), c(34:40), c(50:56))
ls_forte <- list(c(1:7), c(17:23), c(42:48), c(58:64))
ls_piano <- list(c(9:15), c(25:31), c(34:40), c(50:56))

# For intervals (for df_kot)
# Legato
df_kot$SubSkill <- NA
for (phrase in 1:length(ls_legato)){
  for (note in 1:length(ls_legato[[phrase]])){
    df_kot$SubSkill[df_kot$Skill == "articulation" & df_kot$Interval == ls_legato[[phrase]][note]] <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato)){
  for (note in 1:length(ls_staccato[[phrase]])){
    df_kot$SubSkill[df_kot$Skill == "articulation" & df_kot$Interval == ls_staccato[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte)){
  for (note in 1:length(ls_forte[[phrase]])){
    df_kot$SubSkill[df_kot$Skill == "dynamics" & df_kot$Interval == ls_forte[[phrase]][note]] <- "Forte"
  }
}
# Piano
for (phrase in 1:length(ls_piano)){
  for (note in 1:length(ls_piano[[phrase]])){
    df_kot$SubSkill[df_kot$Skill == "dynamics" & df_kot$Interval == ls_piano[[phrase]][note]] <- "Piano"
  }
}

# Define Skill Change (LtoS, FtoP)
change_1 <- c(8, 24, 49)
# Define Skill Change (StoL, PtoF)
change_2 <- c(16, 41, 57)

for (number in change_1){
  df_kot$SubSkill[df_kot$Skill == "articulation" & df_kot$Interval == number] <- "LtoS"
  df_kot$SubSkill[df_kot$Skill == "dynamics" & df_kot$Interval == number] <- "FtoP"
}
for (number in change_2){
  df_kot$SubSkill[df_kot$Skill == "articulation" & df_kot$Interval == number] <- "StoL"
  df_kot$SubSkill[df_kot$Skill == "dynamics" & df_kot$Interval == number] <- "PtoF"
}

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    df_kot$Grouping[df_kot$Condition == ls_grouping$Condition[cond] & df_kot$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

####################################
### Remove outliers
####################################
df_subset <- subset(df_kot, df_kot$Interval != 32 & df_kot$Interval != 33 & df_kot$Interval != 65 & df_kot$Interval != 66) # Exclude irrelevant notes

# Draw histogram and
p_hist <- ggplot(df_subset, aes(x = KOT, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

# Exclude kot > +- 3SD (within a given condition)
kot_subskill <- aggregate(KOT~SubSkill, data = df_subset,
                      FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
kot_subskill <- cbind(kot_subskill, as.data.frame(kot_subskill[,2]))
df_trim_sd <- data.frame()
for (subskill in unique(df_subset$SubSkill)){
  upper = kot_subskill$mean[kot_subskill$SubSkill == subskill]+3*kot_subskill$sd[kot_subskill$SubSkill == subskill]
  lower = kot_subskill$mean[kot_subskill$SubSkill == subskill]-3*kot_subskill$sd[kot_subskill$SubSkill == subskill]
  df_current <- df_subset %>% dplyr::filter(SubSkill == subskill & KOT < upper & KOT > lower)
  df_trim_sd <- rbind(df_trim_sd, df_current)
}
print(sprintf("Remove %i trials beyond +- 3SD", nrow(df_subset)-nrow(df_trim_sd)))

# Sort by RowNr
df_trim_sd <- df_trim_sd[order(df_trim_sd$RowNr),]

p_hist_sd <- ggplot(df_trim_sd, aes(x = KOT, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

p_box_sd <- ggboxplot(df_trim_sd, x = "Skill", y = "KOT", color = "Condition")
p_box_sd <- ggpar(p_box_sd, ylab = "KOT (ms)")

# Save plots
# png files
ggsave("./plot/kot/p_hist.png", plot = p_hist, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/p_hist_sd.png", plot = p_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/p_box_sd.png", plot = p_box_sd, dpi = 600, width = 5, height = 4)

# Export a csv file for df_trimmed
write.csv(df_trim_sd, file = "./trimmed/data_kot.csv", row.names = F)

####################################
# Aggregate data
####################################
df_trim_kot <- read.csv("./trimmed/data_kot.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

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

# Variability (SD/mean KOT)
df_var <- data.frame()
for (subnr in unique(df_trim_kot$SubNr)){
  for (block in unique(df_trim_kot$BlockNr)){
    cond = as.character(unique(df_trim_kot$Condition[df_trim_kot$SubNr == subnr & df_trim_kot$BlockNr == block]))
    skill = as.character(unique(df_trim_kot$Skill[df_trim_kot$SubNr == subnr & df_trim_kot$BlockNr == block]))
    for (trial in unique(df_trim_kot$TrialNr)){
      df_current <- df_trim_kot %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      df_var <- rbind(df_var, data.frame(subnr, block, trial, cond, skill, sd(df_current$KOT)/mean(df_current$KOT)))
    }
  }
}
colnames(df_var) <- c("SubNr", "BlockNr", "TrialNr", "Condition", "Skill", "Variability")

kot_var <- aggregate(Variability~Condition*Skill, data = df_var,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
kot_var_trial <- aggregate(Variability~TrialNr*Condition*Skill, data = df_var,
                           FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Descriptive stats
kot <- cbind(kot, as.data.frame(kot[,3]))
kot_change <- cbind(kot_change, as.data.frame(kot_change[,4]))
kot_sub <- cbind(kot_sub, as.data.frame(kot_sub[,4]))
kot_firstEnd <- cbind(kot_firstEnd, as.data.frame(kot_firstEnd[,4]))
kot_seq <- cbind(kot_seq, as.data.frame(kot_seq[,4]))
kot_var <- cbind(kot_var, as.data.frame(kot_var[,3]))
kot_var_trial <- cbind(kot_var_trial, as.data.frame(kot_var_trial[,4]))

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
    kot_var$Grouping[kot_var$Condition == ls_grouping$Condition[cond] & kot_var$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    kot_var_trial$Grouping[kot_var_trial$Condition == ls_grouping$Condition[cond] & kot_var_trial$Skill == ls_grouping$Skill[skill]] <-
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
### Plots
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

p_kot_var <- ggplot(data = kot_var, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean CV (SD/mean KOT)") + #coord_cartesian(ylim = c(160, 210)) +
  theme_classic()
p_kot_var

p_kot_var_trial <- ggplot(data = kot_var_trial, aes(x = TrialNr, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Trial Number", y = "Mean CV (SD/mean KOT)") + scale_x_continuous(breaks=seq(1,8,1)) +
  theme_classic()
p_kot_var_trial

# Save plots
# png files
ggsave("./plot/kot/p_kot.png", plot = p_kot, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/p_kot_change.png", plot = p_kot_change, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/p_kot_sub.png", plot = p_kot_sub, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/p_kot_firstEnd.png", plot = p_kot_firstEnd, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/p_kot_seq.png", plot = p_kot_seq, dpi = 600, width = 15, height = 4)
ggsave("./plot/kot/p_kot_var.png", plot = p_kot_var, dpi = 600, width = 5, height = 4)
ggsave("./plot/kot/p_kot_var_trial.png", plot = p_kot_var_trial, dpi = 600, width = 10, height = 4)

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
write.csv(kot_anova$ANOVA, file = "./stats/kot_anova.csv")

# posthoc comparison
kot_posthoc <- aov(KOT~Condition*Skill, data = df_trim_kot)
kot_posthoc <-TukeyHSD(kot_posthoc)
write.csv(kot_posthoc$`Condition:Skill`, file = "./stats/kot_posthoc.csv")

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
write.csv(kot_change_anova$ANOVA, file = "./stats/kot_change_anova.csv")
write.csv(kot_change_anova$`Mauchly's Test for Sphericity`, file = "./stats/kot_change_anova_mau.csv")
write.csv(kot_change_anova$`Sphericity Corrections`, file = "./stats/kot_change_anova_sph.csv")

# posthoc comparison
kot_change_posthoc <- aov(KOT~Condition*Skill, data = df_trim_kot)
kot_change_posthoc <-TukeyHSD(kot_change_posthoc)
write.csv(kot_change_posthoc$`Condition:Skill`, file = "./stats/kot_change_posthoc.csv")

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
write.csv(kot_sub_anova$ANOVA, file = "./stats/kot_sub_anova.csv")
write.csv(kot_sub_anova$`Mauchly's Test for Sphericity`, file = "./stats/kot_sub_anova_mau.csv")
write.csv(kot_sub_anova$`Sphericity Corrections`, file = "./stats/kot_sub_anova_sph.csv")

kot_sub_posthoc <- aov(KOT~Condition*SubSkill, data = subset(df_trim_kot, df_trim_kot$Interval != 8 & df_trim_kot$Interval != 16 & df_trim_kot$Interval != 24 & 
                                                               df_trim_kot$Interval != 41 & df_trim_kot$Interval != 49 & df_trim_kot$Interval != 57))
kot_sub_posthoc <- TukeyHSD(kot_sub_posthoc)
write.csv(kot_sub_posthoc$`Condition:SubSkill`, file = "./stats/kot_sub_posthoc.csv")

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
write.csv(kot_firstEnd_anova$ANOVA, file = "./stats/kot_firstEnd_anova.csv")
write.csv(kot_firstEnd_anova$`Mauchly's Test for Sphericity`, file = "./stats/kot_firstEnd_anova_mau.csv")
write.csv(kot_firstEnd_anova$`Sphericity Corrections`, file = "./stats/kot_firstEnd_anova_sph.csv")

# posthoc comparison
kot_firstEnd_posthoc <- aov(KOT~Condition*SubSkill, data = subset(df_trim_kot, df_trim_kot$Interval == 1 | df_trim_kot$Interval == 7 | df_trim_kot$Interval == 9 | df_trim_kot$Interval == 15 | df_trim_kot$Interval == 17 | df_trim_kot$Interval == 23
                                                               | df_trim_kot$Interval == 25 | df_trim_kot$Interval == 31 | df_trim_kot$Interval == 34 | df_trim_kot$Interval == 40 | df_trim_kot$Interval == 42 | df_trim_kot$Interval == 48
                                                               | df_trim_kot$Interval == 50 | df_trim_kot$Interval == 56 | df_trim_kot$Interval == 58 | df_trim_kot$Interval == 64))
kot_firstEnd_posthoc <-TukeyHSD(kot_firstEnd_posthoc)
write.csv(kot_firstEnd_posthoc$`Condition:SubSkill`, file = "./stats/kot_firstEnd_posthoc.csv")

#Normality
q_plot <- ggplot(subset(df_trim_kot, df_trim_kot$Interval != 8 & df_trim_kot$Interval != 16 & df_trim_kot$Interval != 24 & 
                          df_trim_kot$Interval != 41 & df_trim_kot$Interval != 49 & df_trim_kot$Interval != 57), aes(sample = KOT, shape = SubSkill, color = SubSkill)) +
  stat_qq() + theme_classic()
q_plot

# Save plots
# png files
ggsave("./plot/kot/q_plot.png", plot = q_plot, dpi = 600, width = 5, height = 4)