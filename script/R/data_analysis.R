#!/usr/bin/Rscript
#rm(list=ls(all=TRUE)) - clear all in Environment

####################################
#  Documentation
####################################
# Created: 30/01/2019
# Modified: 30/01/2019
# This script aggregate and plot data.
# GitHub repo (private): https://github.com/atsukotominaga/expertpiano/tree/master/script/R 

####################################
#  Requirements
####################################
# !!! Set working directory to file source location !!!

# Install and load required packages
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}

# Create necessary folders if not exist
# plot
if (!file.exists("plot")){
  dir.create("plot")
}
# eps
if (!file.exists("plot/eps/")){
  dir.create("plot/eps")
}
# png
if (!file.exists("plot/png/")){
  dir.create("plot/png")
}

####################################
### Reading and formatting data
####################################
df <- read.csv('./csv/data_analysis.csv', header = T, sep = ",", dec = '.')

exclude <- c(1, 2, 3, 5, 7, 8) # Exclude participants who has over 10% erorr rate
include <- c(4, 6, 9)

df_analysis <- data.frame()
for (subnr in include){
  df_current <- df %>% dplyr::filter(SubNr == subnr)
  df_analysis <- rbind(df_analysis, df_current)
}

####################################
### Inter-Onset intervals - tempoChange
####################################
# Calculate IOIs
df_ioi <- df %>% dplyr::filter(Key_OnOff == 1)
df_ioi$IOI <- diff(c(0, df_ioi$TimeStamp))

# Remove the first note
df_ioi <- df_ioi %>% dplyr::filter(NoteNr != 17)

# Assign a sequence number for each tone
df_ioi$Interval <- rep(1:66, length(df_ioi$NoteNr)/66)

# Aggregate data
# Overall average
ioi <- aggregate(IOI~Condition*Skill, data = subset(df_ioi, df_ioi$Interval != 32 & df_ioi$Interval != 33 & df_ioi$Interval != 65 & df_ioi$Interval != 66),
                 FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})

# Average for each note
ioi_seq <- aggregate(IOI~Interval*Condition*Skill, data = df_ioi, 
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
ioi_seq[,4][ioi_seq$Interval == 32 | ioi_seq$Interval == 33 | ioi_seq$Interval == 65 | ioi_seq$Interval == 66] <- NA

# Descriptive stats
ioi <- cbind(ioi, as.data.frame(ioi[,3]))
ioi_seq <- cbind(ioi_seq, as.data.frame(ioi_seq[,4]))

# Add a grouping name
ls_grouping <- list(Condition = c('performing', 'teaching'), Skill = c('articulation', 'dynamics'))
for (cond in 1:length(ls_grouping$Condition)){
  for (skill in 1:length(ls_grouping$Skill)){
    ioi$Grouping[ioi$Condition == ls_grouping$Condition[cond] & ioi$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
    ioi_seq$Grouping[ioi_seq$Condition == ls_grouping$Condition[cond] & ioi_seq$Skill == ls_grouping$Skill[skill]] <-
      paste(ls_grouping$Condition[cond], '-', ls_grouping$Skill[skill], sep = '')
  }
}


####################################
### IOIs plots
####################################
plot_ioi <- ggplot(data = ioi, aes(x = Skill, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean IOI (ms)") + coord_cartesian(ylim = c(100, 210)) + 
  theme_classic()

plot_ioi_seq <- ggplot(data = ioi_seq, aes(x = Interval, y = mean, group = Grouping, shape = Grouping, colour = Grouping)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 188, linetype = 'dashed') + # Tempo
  geom_hline(data = ioi, aes(yintercept = mean, colour = Grouping), linetype = 'dashed') + # Mean IOI for each grouping
  annotate('text', 0, 188, label = 'Tempo (80bpm)', vjust = -1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean IOI (ms)") + scale_x_continuous(breaks=seq(1,50,1)) +
  theme_classic()

plot_ioi_seq_f <- ggplot(data = ioi_seq, aes(x = Interval, y = mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 188, linetype = 'dashed') + # Tempo
  facet_grid(Skill ~ .) +
  annotate('text', 0, 188, label = 'Tempo (80bpm)', vjust = -1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width=.2,
                position = position_dodge(.05)) + 
  labs(x = 'Interval', y = "Mean IOI (ms)") + scale_x_continuous(breaks=seq(1,50,1)) +
  theme_classic()

# Save plots
# eps files
ggsave('./plot/eps/plot_ioi.eps', plot = plot_ioi, dpi = 600, width = 5, height = 4)
ggsave('./plot/eps/plot_ioi_seq.eps', plot = plot_ioi_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/eps/plot_ioi_seq_f.eps', plot = plot_ioi_seq_f, dpi = 600, width = 15, height = 4)

# png files
ggsave('./plot/png/plot_ioi.png', plot = plot_ioi, dpi = 600, width = 5, height = 4)
ggsave('./plot/png/plot_ioi_seq.png', plot = plot_ioi_seq, dpi = 600, width = 15, height = 4)
ggsave('./plot/png/plot_ioi_seq_f.png', plot = plot_ioi_seq_f, dpi = 600, width = 15, height = 4)
