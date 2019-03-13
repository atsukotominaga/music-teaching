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
if (!require("RColorBrewer")) {install.packages("RColorBrewer"); require("RColorBrewer")}
# statistics
if (!require("car")) {install.packages("car"); require("car")}
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

####################################
# Aggregate data
####################################
# Overall average
df_subset <- subset(df_ioi, df_ioi$Interval != 32 & df_ioi$Interval != 33 & df_ioi$Interval != 65 & df_ioi$Interval != 66) # Exclude irrelevant notes

ioi <- aggregate(IOI~Condition*Skill, data = df_subset,
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
ioi_seq <- aggregate(IOI~Interval*Condition*Skill, data = df_subset, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
ioi_seq[,4][ioi_seq$Interval == 32 | ioi_seq$Interval == 33 | ioi_seq$Interval == 65 | ioi_seq$Interval == 66] <- NA

# Variability (SD/mean IOI)
df_var <- data.frame()
for (subnr in unique(df_subset$SubNr)){
  for (block in unique(df_subset$BlockNr)){
    cond = as.character(unique(df_subset$Condition[df_subset$SubNr == subnr & df_subset$BlockNr == block]))
    skill = as.character(unique(df_subset$Skill[df_subset$SubNr == subnr & df_subset$BlockNr == block]))
    for (trial in unique(df_subset$TrialNr)){
      df_current <- df_subset %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      df_var <- rbind(df_var, data.frame(subnr, block, trial, cond, skill, sd(df_current$IOI)/mean(df_current$IOI)))
    }
  }
}
colnames(df_var) <- c("SubNr", "BlockNr", "TrialNr", "Condition", "Skill", "Variability")

ioi_var <- aggregate(Variability~Condition*Skill, data = df_var,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))}, na.action = na.omit)

ioi_var_trial <- aggregate(Variability~Condition*Skill*TrialNr, data = df_var,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))}, na.action = na.omit)

# Descriptive stats
ioi <- cbind(ioi, as.data.frame(ioi[,3]))
ioi_seq <- cbind(ioi_seq, as.data.frame(ioi_seq[,4]))
ioi_var <- cbind(ioi_var, as.data.frame(ioi_var[,3]))
ioi_var_trial <- cbind(ioi_var_trial, as.data.frame(ioi_var_trial[,4]))

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    df_subset$Grouping[df_subset$Condition == ls_grouping$Condition[cond] & df_subset$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi$Grouping[ioi$Condition == ls_grouping$Condition[cond] & ioi$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_seq$Grouping[ioi_seq$Condition == ls_grouping$Condition[cond] & ioi_seq$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_var$Grouping[ioi_var$Condition == ls_grouping$Condition[cond] & ioi_var$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_var_trial$Grouping[ioi_var_trial$Condition == ls_grouping$Condition[cond] & ioi_var_trial$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

####################################
### Plots
####################################
# Histogram
p_hist <- ggplot(df_subset, aes(x = IOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  geom_vline(data = ioi, aes(xintercept = mean, color = Grouping), linetype = "dashed") +
  theme_classic()

# Box plot
p_box <- ggboxplot(df_subset, x = "Skill", y = "IOI", color = "Condition")
p_box <- ggpar(p_box, ylab = "IOI (ms)")

p_ioi <- ggplot(data = ioi, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean IOI (ms)") +
  theme_classic()

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

p_ioi_var <- ggplot(data = ioi_var, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean CV (SD/mean IOI)") +
  theme_classic()

p_ioi_var_trial <- ggplot(data = ioi_var_trial, aes(x = TrialNr, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Trial Number", y = "Mean CV (SD/mean IOI)") + scale_x_continuous(breaks=seq(1,8,1)) +
  theme_classic()

# Save plots
# png files
ggsave("./plot/ioi/p_hist.png", plot = p_hist, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_box.png", plot = p_box, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_ioi.png", plot = p_ioi, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_ioi_seq.png", plot = p_ioi_seq, dpi = 600, width = 15, height = 4)
ggsave("./plot/ioi/p_ioi_var.png", plot = p_ioi_var, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_ioi_var_trial.png", plot = p_ioi_var_trial, dpi = 600, width = 10, height = 4)

####################################
### Remove outliers
####################################
# Exclude ioi > +- 3SD
upper <- mean(df_subset$IOI)+3*sd(df_subset$IOI)
lower <- mean(df_subset$IOI)-3*sd(df_subset$IOI)
df_trim_sd <- df_subset %>% dplyr::filter(IOI < upper & IOI > lower)
df_excluded <- df_subset %>% dplyr::filter(IOI > upper | IOI < lower)

p_hist_sd <- ggplot(df_trim_sd, aes(x = IOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

p_box_sd <- ggboxplot(df_trim_sd, x = "Skill", y = "IOI", color = "Condition")
p_box_sd <- ggpar(p_box_sd, ylab = "IOI (ms)")

# Exclude upper 5%
quantiles <- quantile(df_subset$IOI, c(.05, .95))
df_trim_quantile <- df_subset %>% dplyr::filter(IOI < quantiles[2])

p_hist_quantile <- ggplot(df_trim_quantile, aes(x = IOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

p_box_quantile <- ggboxplot(df_trim_quantile, x = "Skill", y = "IOI", color = "Condition")
p_box_quantile <- ggpar(p_box_quantile, ylab = "IOI (ms)")

# The number of exclusion (participant 16)
print(sprintf("Participant 16: included %i, excluded %i",
              (nrow(df_subset[df_subset$SubNr == 16,]) - nrow(df_excluded[df_excluded$SubNr == 16,])),
              nrow(df_excluded[df_excluded$SubNr == 16,])))

# Exclude participant 16 from df_trim_sd (!!! not from df_subset !!!)
df_trimmed <- df_trim_sd %>% dplyr::filter(SubNr != 16)

p_hist_trimmed <- ggplot(df_trimmed, aes(x = IOI, fill = Grouping)) +
  geom_histogram(position = "identity", alpha = .5, binwidth = 5) +
  theme_classic()

p_box_trimmed <- ggboxplot(df_trimmed, x = "Skill", y = "IOI", color = "Condition")
p_box_trimmed <- ggpar(p_box_trimmed, ylab = "IOI (ms)")

# Save plots
# png files
ggsave("./plot/ioi/p_hist_sd.png", plot = p_hist_sd, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_box_sd.png", plot = p_box_sd, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_hist_quantile.png", plot = p_hist_quantile, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_box_quantile.png", plot = p_box_quantile, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_hist_trimmed.png", plot = p_hist_trimmed, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_box_trimmed.png", plot = p_box_trimmed, dpi = 600, width = 5, height = 4)

# Export a csv file for df_trimmed
write.csv(df_trimmed, file = "./trimmed/data_ioi.csv", row.names = F)

####################################
# Aggregate data
####################################
df_trim_ioi <- read.csv("./trimmed/data_ioi.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

# Overall average
ioi_trim <- aggregate(IOI~Condition*Skill, data = df_trim_ioi,
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
ioi_trim_seq <- aggregate(IOI~Interval*Condition*Skill, data = df_trim_ioi, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Variability (SD/mean IOI)
df_trim_var <- data.frame()
for (subnr in unique(df_trim_ioi$SubNr)){
  for (block in unique(df_trim_ioi$BlockNr)){
    cond = as.character(unique(df_trim_ioi$Condition[df_trim_ioi$SubNr == subnr & df_trim_ioi$BlockNr == block]))
    skill = as.character(unique(df_trim_ioi$Skill[df_trim_ioi$SubNr == subnr & df_trim_ioi$BlockNr == block]))
    for (trial in unique(df_trim_sd$TrialNr)){
      df_current <- df_trim_ioi %>% dplyr::filter(SubNr == subnr & BlockNr == block & TrialNr == trial)
      df_trim_var <- rbind(df_trim_var, data.frame(subnr, block, trial, cond, skill, sd(df_current$IOI)/mean(df_current$IOI)))
    }
  }
}
colnames(df_trim_var) <- c("SubNr", "BlockNr", "TrialNr", "Condition", "Skill", "Variability")

ioi_trim_var <- aggregate(Variability~Condition*Skill, data = df_trim_var,
                                FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))}, na.action = na.omit)
ioi_trim_var_trial <- aggregate(Variability~Condition*Skill*TrialNr, data = df_trim_var,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))}, na.action = na.omit)

# When sub-skill changes
# Define LtoS and FtoP
df_trim_change <- df_trim_ioi %>% dplyr::filter(Interval == 8 | Interval == 16 | Interval == 24 | Interval == 41 | Interval == 49 | Interval == 57)
ioi_trim_change <- aggregate(IOI~Condition*Skill, data = df_trim_change,
                        FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Descriptive stats
ioi_trim <- cbind(ioi_trim, as.data.frame(ioi_trim[,3]))
ioi_trim_seq <- cbind(ioi_trim_seq, as.data.frame(ioi_trim_seq[,4]))
ioi_trim_var <- cbind(ioi_trim_var, as.data.frame(ioi_trim_var[,3]))
ioi_trim_var_trial <- cbind(ioi_trim_var_trial, as.data.frame(ioi_trim_var_trial[,4]))
ioi_trim_change <- cbind(ioi_trim_change, as.data.frame(ioi_trim_change[,3]))

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    ioi_trim$Grouping[ioi_trim$Condition == ls_grouping$Condition[cond] & ioi_trim$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_trim_seq$Grouping[ioi_trim_seq$Condition == ls_grouping$Condition[cond] & ioi_trim_seq$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_trim_var$Grouping[ioi_trim_var$Condition == ls_grouping$Condition[cond] & ioi_trim_var$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_trim_var_trial$Grouping[ioi_trim_var_trial$Condition == ls_grouping$Condition[cond] & ioi_trim_var_trial$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_trim_change$Grouping[ioi_trim_change$Condition == ls_grouping$Condition[cond] & ioi_trim_change$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}

####################################
### Plots
####################################
p_ioi_trim <- ggplot(data = ioi_trim, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean IOI (ms)") + coord_cartesian(ylim = c(0, 250)) +
  theme_classic()
p_ioi_trim

p_ioi_trim_seq <- ggplot(data = ioi_trim_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 188, linetype = "dashed") + # Tempo
  facet_grid(Skill ~ .) +
  annotate("text", 0, 188, label = "Tempo (80bpm)", vjust = -1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "Mean IOI (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()
p_ioi_trim_seq

p_ioi_trim_var <- ggplot(data = ioi_trim_var, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean CV (SD/mean IOI)") + #coord_cartesian(ylim = c(160, 210)) +
  theme_classic()
p_ioi_trim_var

p_ioi_trim_var_trial <- ggplot(data = ioi_trim_var_trial, aes(x = TrialNr, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Trial Number", y = "Mean CV (SD/mean IOI)") + scale_x_continuous(breaks=seq(1,8,1)) +
  theme_classic()
p_ioi_trim_var_trial

p_ioi_trim_change <- ggplot(data = ioi_trim_change, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean IOI (ms)") + coord_cartesian(ylim = c(0, 250)) +
  theme_classic()
p_ioi_trim_change

# Save plots
# png files
ggsave("./plot/ioi/p_ioi_trim.png", plot = p_ioi_trim, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_ioi_trim_seq.png", plot = p_ioi_trim_seq, dpi = 600, width = 15, height = 4)
ggsave("./plot/ioi/p_ioi_trim_var.png", plot = p_ioi_trim_var, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/p_ioi_trim_var_trial.png", plot = p_ioi_trim_var_trial, dpi = 600, width = 10, height = 4)
ggsave("./plot/ioi/p_ioi_trim_change.png", plot = p_ioi_trim_change, dpi = 600, width = 5, height = 4)

####################################
### Statistics
####################################
# Two-way ANOVA
# ioi_trim
model_ioi <- lm(IOI~Condition*Skill, data = df_trim_ioi)
ioi_anova <- ezANOVA(
  data = df_trim_ioi
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(ioi_anova)

# ioi_trim_var
model_ioi <- lm(IOI~Condition*Skill, data = df_trim_ioi)
ioi_var_anova <- ezANOVA(
  data = df_trim_var[complete.cases(df_trim_var),]
  , dv = .(Variability)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(ioi_var_anova)

# ioi_trim_change
ioi_change_anova <- ezANOVA(
  data = df_trim_change
  , dv = .(IOI)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , detailed = TRUE
)
print(ioi_change_anova)

# Homogeneity of variance
plot(ioi_change_anova, 1)

# Normality
q_plot <- ggplot(df_trim_ioi, aes(sample = IOI, shape = Grouping, color = Grouping)) +
  stat_qq() + theme_classic()

q_plot_quantile <- ggplot(df_trim_quantile, aes(sample = IOI, shape = Grouping, color = Grouping)) +
  stat_qq() + theme_classic()

# Save plots
# png files
ggsave("./plot/ioi/q_plot.png", plot = q_plot, dpi = 600, width = 5, height = 4)
ggsave("./plot/ioi/q_plot_quantile.png", plot = q_plot_quantile, dpi = 600, width = 5, height = 4)