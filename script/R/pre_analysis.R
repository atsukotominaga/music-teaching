# For meeting with Natalie 16/07/19

# install and load required packages
# data manipulation
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
# plot
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}

# create a folder
if (!file.exists("pre_analysis")){
  dir.create("pre_analysis")
}

# read csv
df_all <- read.csv("./filtered/data_analysis.csv", header = T, sep = ",", dec = ".")

# define subcomponents - later

# ioi
df_ioi <- df_all %>% dplyr::filter(Key_OnOff == 1)
df_ioi$IOI <- diff(c(0, df_ioi$TimeStamp))

# remove first note
df_ioi <- df_ioi %>% dplyr::filter(NoteNr != 17)

# assign a sequence number for each tone
df_ioi$Interval <- rep(1:71, length(df_ioi$NoteNr)/71)

# individual
ioi_seq <- aggregate(IOI~SubNr*Condition*Skill*Interval, data = df_ioi,
                     FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x)), 4)})
ioi_seq <- cbind(ioi_seq[,1:4], as.data.frame(ioi_seq[,5]))
# change colnames
colnames(ioi_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# SubNr as factor
ioi_seq$SubNr <- as.factor(ioi_seq$SubNr)

# group
ioi_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = ioi_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
ioi_seq_stats <- cbind(ioi_seq_stats[,1:3], as.data.frame(ioi_seq_stats[,4]))
# change colnames
colnames(ioi_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

# plot
p_ioi_seq <- ggplot(data = subset(ioi_seq_stats, ioi_seq_stats$Interval != 27 & ioi_seq_stats$Interval != 28 &
                                  ioi_seq_stats$Interval != 33 & ioi_seq_stats$Interval != 34 & ioi_seq_stats$Interval != 35 &
                                  ioi_seq_stats$Interval != 52 & ioi_seq_stats$Interval != 57 & ioi_seq_stats$Interval != 66 &
                                  ioi_seq_stats$Interval != 67 & ioi_seq_stats$Interval != 68 & ioi_seq_stats$Interval != 69 &
                                  ioi_seq_stats$Interval != 70 & ioi_seq_stats$Interval != 71),
                    aes(x = Interval, y = Mean, group = Condition, shape = Condition, color = Condition)) +
  geom_line() +
  geom_point () +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = .2, position = position_dodge(.05)) +
  labs(x = "Interval", y = "IOI (ms)") +
  scale_x_continuous(breaks = seq(1, 71, 1)) +
  theme_classic() +
  theme(text = element_text(size = 15, family = "Helvetica Neue LT Std 57 Condensed"))
p_ioi_seq

ggsave("./pre_analysis/p_ioi_seq.png", plot = p_ioi_seq, dpi = 600, width = 15, height = 4)

# kot
df_onset <- df_all %>% dplyr::filter(Key_OnOff == 1)
df_offset <- df_all %>% dplyr::filter(Key_OnOff == 0)

# Offset 1 - Onset 2
df_onset$KOT <- NA
for (row in 1:length(df_onset$NoteNr)){
  if (row < length(df_onset$NoteNr)){
    df_onset$KOT[row+1] <- df_offset$TimeStamp[row] - df_onset$TimeStamp[row+1] # offset(n) - onset(n+1)
  }
}

# remove the first note
df_kot <- df_onset %>% dplyr::filter(NoteNr != 17)

# assign a sequence number for each tone
df_kot$Interval <- rep(1:71, length(df_kot$NoteNr)/71)

# individual
kot_seq <- aggregate(KOT~SubNr*Condition*Skill*Interval, data = df_kot,
                     FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x)), 4)})
kot_seq <- cbind(kot_seq[,1:4], as.data.frame(kot_seq[,5]))
# change colnames
colnames(kot_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# SubNr as factor
kot_seq$SubNr <- as.factor(kot_seq$SubNr)

# group
kot_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = kot_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_seq_stats <- cbind(kot_seq_stats[,1:3], as.data.frame(kot_seq_stats[,4]))
# change colnames
colnames(kot_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

# plot
p_kot_seq <- ggplot(data = subset(kot_seq_stats, kot_seq_stats$Interval != 27 & kot_seq_stats$Interval != 28 &
                                    kot_seq_stats$Interval != 33 & kot_seq_stats$Interval != 34 & kot_seq_stats$Interval != 35 &
                                    kot_seq_stats$Interval != 52 & kot_seq_stats$Interval != 57 & kot_seq_stats$Interval != 66 &
                                    kot_seq_stats$Interval != 67 & kot_seq_stats$Interval != 68 & kot_seq_stats$Interval != 69 &
                                    kot_seq_stats$Interval != 70 & kot_seq_stats$Interval != 71),
                    aes(x = Interval, y = Mean, group = Condition, shape = Condition, color = Condition)) +
  geom_line() +
  geom_point () +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = .2, position = position_dodge(.05)) +
  labs(x = "Interval", y = "KOT (ms)") +
  scale_x_continuous(breaks = seq(1, 71, 1)) +
  theme_classic() +
  theme(text = element_text(size = 15, family = "Helvetica Neue LT Std 57 Condensed"))
p_kot_seq

ggsave("./pre_analysis/p_kot_seq.png", plot = p_kot_seq, dpi = 600, width = 15, height = 4)

# vel
# calculate Acc (acceleration - velocity difference between notes)
df_vel <- df_all %>% dplyr::filter(Key_OnOff == 1)
df_vel$Acc <- diff(c(0, df_vel$Velocity))

# Remove the first note
df_vel_acc <- df_vel %>% dplyr::filter(NoteNr != 17)
df_vel$Acc <- NULL # Remove Acc from df_vel

# Assign a sequence number for each tone / interval
df_vel$Note <- rep(1:72, length(df_vel$NoteNr)/72) # for vel_seq
df_vel_acc$Interval <- rep(1:71, length(df_vel_acc$NoteNr)/71) # for vel_acc_seq

# individual
vel_seq <- aggregate(Velocity~SubNr*Condition*Skill*Note, data = df_vel,
                     FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x)), 4)})
vel_seq <- cbind(vel_seq[,1:4], as.data.frame(vel_seq[,5]))
# change colnames
colnames(vel_seq) <- c("SubNr", "Condition", "Skill", "Note", "N", "Mean", "SD")

# SubNr as factor
vel_seq$SubNr <- as.factor(vel_seq$SubNr)

# group
vel_seq_stats <- aggregate(Mean~Condition*Skill*Note, data = vel_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_seq_stats <- cbind(vel_seq_stats[,1:3], as.data.frame(vel_seq_stats[,4]))
# change colnames
colnames(vel_seq_stats) <- c("Condition", "Skill", "Note", "N", "Mean", "SD", "SEM")

# plot
p_vel_seq <- ggplot(data = subset(vel_seq_stats, vel_seq_stats$Note != 27 & vel_seq_stats$Note != 28 &
                                    vel_seq_stats$Note != 33 & vel_seq_stats$Note != 34 & vel_seq_stats$Note != 35 &
                                    vel_seq_stats$Note != 52 & vel_seq_stats$Note != 57 & vel_seq_stats$Note != 66 &
                                    vel_seq_stats$Note != 67 & vel_seq_stats$Note != 68 & vel_seq_stats$Note != 69 &
                                    vel_seq_stats$Note != 70 & vel_seq_stats$Note != 71 & vel_seq_stats$Note != 72),
                    aes(x = Note, y = Mean, group = Condition, shape = Condition, color = Condition)) +
  geom_line() +
  geom_point () +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = .2, position = position_dodge(.05)) +
  labs(x = "Note", y = "Velocity (1-127)") +
  scale_x_continuous(breaks = seq(1, 72, 1)) +
  theme_classic() +
  theme(text = element_text(size = 15, family = "Helvetica Neue LT Std 57 Condensed"))
p_vel_seq

ggsave("./pre_analysis/p_vel_seq.png", plot = p_vel_seq, dpi = 600, width = 15, height = 4)

# individual
vel_acc_seq <- aggregate(Acc~SubNr*Condition*Skill*Interval, data = df_vel_acc,
                     FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x)), 4)})
vel_acc_seq <- cbind(vel_acc_seq[,1:4], as.data.frame(vel_acc_seq[,5]))
# change colnames
colnames(vel_acc_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# SubNr as factor
vel_acc_seq$SubNr <- as.factor(vel_acc_seq$SubNr)

# group
vel_acc_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = vel_acc_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
vel_acc_seq_stats <- cbind(vel_acc_seq_stats[,1:3], as.data.frame(vel_acc_seq_stats[,4]))
# change colnames
colnames(vel_acc_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

# plot
p_vel_acc_seq <- ggplot(data = subset(vel_acc_seq_stats, vel_acc_seq_stats$Interval != 27 & vel_acc_seq_stats$Interval != 28 &
                                    vel_acc_seq_stats$Interval != 33 & vel_acc_seq_stats$Interval != 34 & vel_acc_seq_stats$Interval != 35 &
                                    vel_acc_seq_stats$Interval != 52 & vel_acc_seq_stats$Interval != 57 & vel_acc_seq_stats$Interval != 66 &
                                    vel_acc_seq_stats$Interval != 67 & vel_acc_seq_stats$Interval != 68 & vel_acc_seq_stats$Interval != 69 &
                                    vel_acc_seq_stats$Interval != 70 & vel_acc_seq_stats$Interval != 71),
                    aes(x = Interval, y = Mean, group = Condition, shape = Condition, color = Condition)) +
  geom_line() +
  geom_point () +
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = .2, position = position_dodge(.05)) +
  labs(x = "Interval", y = "Difference (1-127)") +
  scale_x_continuous(breaks = seq(1, 71, 1)) +
  theme_classic() +
  theme(text = element_text(size = 15, family = "Helvetica Neue LT Std 57 Condensed"))
p_vel_acc_seq

ggsave("./pre_analysis/p_vel_acc_seq.png", plot = p_vel_acc_seq, dpi = 600, width = 15, height = 4)
