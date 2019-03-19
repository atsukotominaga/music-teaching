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

# trimmed
if (!file.exists("trimmed")){
  dir.create("trimmed")
}

####################################
# Reading and formatting data
####################################
# Read processed csv files
df_all <- read.csv("./processed/data_analysis.csv", header = T, sep = ",", dec = ".") # clear data without pitch errors
df_exc <- read.csv("./processed/data_errorRate.csv", header = T, sep = ",", dec = ".") # exclusion criteria

# Exclude participants with more than 10% errors
include <- df_exc$SubNr[df_exc$LessThan10 == "include"]

# Data frame with only included participants
df_analysis <- data.frame()
for (subnr in include){
  df_current <- df_all %>% dplyr::filter(SubNr == subnr)
  df_analysis <- rbind(df_analysis, df_current)
}

####################################
# Inter-Onset intervals
####################################
# Calculate IOIs
df_ioi <- df_analysis %>% dplyr::filter(Key_OnOff == 1)
df_ioi$IOI <- diff(c(0, df_ioi$TimeStamp))

# Remove the first note
df_ioi <- df_ioi %>% dplyr::filter(NoteNr != 17)

# Assign a sequence number for each tone
df_ioi$Interval <- rep(1:66, length(df_ioi$NoteNr)/66)

### Define SubSkills
# For intervals
ls_legato <- list(c(1:7), c(17:23), c(42:48), c(58:64))
ls_staccato <- list(c(9:15), c(25:31), c(34:40), c(50:56))
ls_forte <- list(c(1:7), c(17:23), c(42:48), c(58:64))
ls_piano <- list(c(9:15), c(25:31), c(34:40), c(50:56))

# For intervals
# Legato
df_ioi$SubSkill <- NA
for (phrase in 1:length(ls_legato)){
  for (note in 1:length(ls_legato[[phrase]])){
    df_ioi$SubSkill[df_ioi$Skill == "articulation" & df_ioi$Interval == ls_legato[[phrase]][note]] <- "Legato"
  }
}
# Staccato
for (phrase in 1:length(ls_staccato)){
  for (note in 1:length(ls_staccato[[phrase]])){
    df_ioi$SubSkill[df_ioi$Skill == "articulation" & df_ioi$Interval == ls_staccato[[phrase]][note]] <- "Staccato"
  }
}

# Forte
for (phrase in 1:length(ls_forte)){
  for (note in 1:length(ls_forte[[phrase]])){
    df_ioi$SubSkill[df_ioi$Skill == "dynamics" & df_ioi$Interval == ls_forte[[phrase]][note]] <- "Forte"
  }
}
# Piano
for (phrase in 1:length(ls_piano)){
  for (note in 1:length(ls_piano[[phrase]])){
    df_ioi$SubSkill[df_ioi$Skill == "dynamics" & df_ioi$Interval == ls_piano[[phrase]][note]] <- "Piano"
  }
}

# Define LtoS and FtoP
change_1 <- c(8, 24, 49)
# Define StoL and PtoF
change_2 <- c(16, 41, 57)

for (number in change_1){
  df_ioi$SubSkill[df_ioi$Skill == "articulation" & df_ioi$Interval == number] <- "LtoS"
  df_ioi$SubSkill[df_ioi$Skill == "dynamics" & df_ioi$Interval == number] <- "FtoP"
}
for (number in change_2){
  df_ioi$SubSkill[df_ioi$Skill == "articulation" & df_ioi$Interval == number] <- "StoL"
  df_ioi$SubSkill[df_ioi$Skill == "dynamics" & df_ioi$Interval == number] <- "PtoF"
}

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    df_ioi$Grouping[df_ioi$Condition == ls_grouping$Condition[cond] & df_ioi$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

####################################
### Remove outliers
####################################
df_subset <- subset(df_ioi, df_ioi$Interval != 32 & df_ioi$Interval != 33 & df_ioi$Interval != 65 & df_ioi$Interval != 66) # Exclude irrelevant notes

# Exclude ioi > +- 3SD
upper <- mean(df_subset$IOI)+3*sd(df_subset$IOI)
lower <- mean(df_subset$IOI)-3*sd(df_subset$IOI)

p_hist <- ggplot(df_subset, aes(x = IOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

# Aggregate IOIs for each participant
ioi_part <- aggregate(IOI~SubNr, data = df_subset,
                      FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
ioi_part <- cbind(ioi_part, as.data.frame(ioi_part[,2]))

# Exclude whose mean IOIs is more than 3SD
ioi_mean = mean(ioi_part$mean)
ioi_sd = sd(ioi_part$mean)
exclude = c() # excluded participants
for (subnr in unique(ioi_part$SubNr)){
  if (ioi_part$mean[ioi_part$SubNr == subnr] > ioi_mean+3*ioi_sd){
    exclude = c(exclude, subnr)
    print(sprintf("Exclude participant %i", subnr))
  }
}

# Exclude participants
for (subnr in exclude){
  df_trim <- df_subset %>% dplyr::filter(SubNr != subnr)
}

df_trim_sd <- df_trim %>% dplyr::filter(IOI < upper & IOI > lower)
print(sprintf("Remove %i trials beyond +- 3SD", nrow(df_trim)-nrow(df_trim_sd)))

p_hist_sd <- ggplot(df_trim_sd, aes(x = IOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

p_box_sd <- ggboxplot(df_trim_sd, x = "Skill", y = "IOI", color = "Condition")
p_box_sd <- ggpar(p_box_sd, ylab = "IOI (ms)")

# Save plots
# png files
ggsave("./plot/ioi/p_hist.png", plot = p_hist, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_hist_sd.png", plot = p_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_box_sd.png", plot = p_box_sd, dpi = 600, width = 5, height = 4)

# Export a csv file for df_trimmed
write.csv(df_trim, file = "./trimmed/data_ioi.csv", row.names = F)

####################################
# Aggregate data
####################################
df_trim_ioi <- read.csv("./trimmed/data_ioi.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

# Overall average
ioi <- aggregate(IOI~Condition*Skill, data = df_trim_ioi,
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for SubSkill
ioi_sub <- aggregate(IOI~Condition*Skill*SubSkill, data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 |
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
    for (trial in unique(df_trim_sd$TrialNr)){
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
ioi_sub <- cbind(ioi_sub, as.data.frame(ioi_sub[,4]))
ioi_seq <- cbind(ioi_seq, as.data.frame(ioi_seq[,4]))
ioi_var <- cbind(ioi_var, as.data.frame(ioi_var[,3]))
ioi_var_trial <- cbind(ioi_var_trial, as.data.frame(ioi_var_trial[,4]))

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    ioi$Grouping[ioi$Condition == ls_grouping$Condition[cond] & ioi$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_sub$Grouping[ioi_sub$Condition == ls_grouping$Condition[cond] & ioi_sub$Skill == ls_grouping$Skill[skill]] <-
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
ioi_sub$Order[ioi_sub$Skill == "articulation"] <- 1
ioi_sub$Order[ioi_sub$Skill == "dynamics"] <- 2

####################################
### Plots
####################################
p_ioi <- ggplot(data = ioi, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean IOI (ms)") + coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_ioi

p_ioi_sub <- ggplot(data = ioi_sub, aes(x = reorder(SubSkill, Order), y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "Mean IOI (ms)") + coord_cartesian(ylim = c(100, 250)) +
  theme_classic()
p_ioi_sub

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

p_ioi_var <- ggplot(data = ioi_var, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean CV (SD/mean IOI)") + #coord_cartesian(ylim = c(160, 210)) +
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
ggsave("./plot/ioi/p_ioi.png", plot = p_ioi, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_ioi_sub.png", plot = p_ioi_sub, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_ioi_seq.png", plot = p_ioi_seq, dpi = 600, width = 15, height = 4)
ggsave("./plot/ioi/p_ioi_var.png", plot = p_ioi_var, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_ioi_var_trial.png", plot = p_ioi_var_trial, dpi = 600, width = 10, height = 4)

####################################
### Statistics
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

ioi_posthoc <- aov(IOI~Condition*Skill, data = df_trim_ioi)
print(TukeyHSD(ioi_posthoc))

# Two-way ANOVA
# ioi_sub
ioi_sub_anova <- ezANOVA(
  data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 | 
                  df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57)
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
  , return_aov = TRUE
)
print(ioi_sub_anova)

ioi_sub_posthoc <- aov(IOI~Condition*SubSkill, data = subset(df_trim_ioi, df_trim_ioi$Interval == 8 | df_trim_ioi$Interval == 16 | df_trim_ioi$Interval == 24 | 
                                                            df_trim_ioi$Interval == 41 | df_trim_ioi$Interval == 49 | df_trim_ioi$Interval == 57))
print(TukeyHSD(ioi_sub_posthoc))

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

ioi_var_posthoc <- aov(Variability~Condition*Skill, data = df_var[complete.cases(df_var),])
print(TukeyHSD(ioi_var_posthoc))

# Normality
q_plot <- ggplot(df_trim_ioi, aes(sample = IOI, shape = Grouping, color = Grouping)) +
  stat_qq() + theme_classic()

# Save plots
# png files
ggsave("./plot/ioi/q_plot.png", plot = q_plot, dpi = 600, width = 5, height = 4)