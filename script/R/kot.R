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
# Install and load required packages
# data manipulation
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
# plot
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}
# statistics
if (!require("car")) {install.packages("car"); require("car")}
if (!require("ez")) {install.packages("ez"); require("ez")}

# Create necessary folders if not exist
# 3_stats
if (!file.exists("3_stats")){
  dir.create("3_stats")
}

# 3_stats/kot - store csv files
if (!file.exists("3_stats/kot")){
  dir.create("3_stats/kot")
}

# 3_stats/plot
if (!file.exists("3_stats/plot")){
  dir.create("3_stats/plot")
}
# 3_stats/plot/kot - store png files
if (!file.exists("3_stats/plot/kot/")){
  dir.create("3_stats/plot/kot")
}

####################################
# Reading and formatting data
####################################
df_kot <- read.csv("./2_trimmed/data_kot.csv", header = T, sep = ",", dec = ".") # read a trimmed csv

# SubNr as a factor
df_kot$SubNr <- as.factor(df_kot$SubNr)

####################################
# Aggregate data
####################################
# 1. Overall KOT
# For each individual
kot <- aggregate(KOT~SubNr*Condition*Skill, data = df_kot,
                 FUN = function(x){c(length(x), mean = mean(x), sd = sd(x))})
kot <- cbind(kot[,1:3], as.data.frame(kot[,4]))
# Change colnames
colnames(kot) <- c("SubNr", "Condition", "Skill", "N", "Mean", "SD")

# Group mean
kot_stats <- aggregate(Mean~Condition*Skill, data = kot,
                       FUN = function(x){round(c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_stats <- cbind(kot_stats[,1:2], as.data.frame(kot_stats[,3]))
# Change colnames
colnames(kot_stats) <- c("Condition", "Skill", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
kot_ezstats <- ezStats(
  data = df_kot
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , check_args = TRUE
)

# 2. Average KOT for skill changes
# For each individual
kot_ch <- aggregate(KOT~SubNr*Condition*Skill, data = subset(df_kot, df_kot$Interval == 8 | df_kot$Interval == 16 | df_kot$Interval == 24 | df_kot$Interval == 41 | df_kot$Interval == 49 | df_kot$Interval == 57),
                    FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kot_ch <- cbind(kot_ch[,1:3], as.data.frame(kot_ch[,4]))
# Change colnames
colnames(kot_ch) <- c("SubNr", "Condition", "Skill", "N", "Mean", "SD")

# Group mean
kot_ch_stats <- aggregate(Mean~Condition*Skill, data = kot_ch,
                          FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_ch_stats <- cbind(kot_ch_stats[,1:2], as.data.frame(kot_ch_stats[,3]))
# Change colnames
colnames(kot_ch_stats) <- c("Condition", "Skill", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
kot_ch_ezstats <- ezStats(
  data = subset(df_kot, df_kot$Interval == 8 | df_kot$Interval == 16 | df_kot$Interval == 24 | df_kot$Interval == 41 | df_kot$Interval == 49 | df_kot$Interval == 57)
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, Skill)
  , type = 3
  , check_args = TRUE
)

# 3. Average KOT for each sub-skill change
# For each individual
kot_ch_sub <- aggregate(KOT~SubNr*Condition*Skill*SubSkill, data = subset(df_kot, df_kot$Interval == 8 | df_kot$Interval == 16 | df_kot$Interval == 24 | df_kot$Interval == 41 | df_kot$Interval == 49 | df_kot$Interval == 57),
                        FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kot_ch_sub <- cbind(kot_ch_sub[,1:4], as.data.frame(kot_ch_sub[,5]))
# Change colnames
colnames(kot_ch_sub) <- c("SubNr", "Condition", "Skill", "SubSkill", "N", "Mean", "SD")

# Group mean
kot_ch_sub_stats <- aggregate(Mean~Condition*Skill*SubSkill, data = kot_ch_sub,
                              FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_ch_sub_stats <- cbind(kot_ch_sub_stats[,1:3], as.data.frame(kot_ch_sub_stats[,4]))
# Change colnames
colnames(kot_ch_sub_stats) <- c("Condition", "Skill", "SubSkill", "N", "Mean", "SD", "SEM")

# 4. Average KOT for each sub-skill
# For each individual
kot_sub <- aggregate(KOT~SubNr*Condition*Skill*SubSkill, data = subset(df_kot, df_kot$Interval != 8 & df_kot$Interval != 16 & df_kot$Interval != 24 & df_kot$Interval != 41 & df_kot$Interval != 49 & df_kot$Interval != 57),
                        FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kot_sub <- cbind(kot_sub[,1:4], as.data.frame(kot_sub[,5]))
# Change colnames
colnames(kot_sub) <- c("SubNr", "Condition", "Skill", "SubSkill", "N", "Mean", "SD")

# Group mean
kot_sub_stats <- aggregate(Mean~Condition*Skill*SubSkill, data = kot_sub,
                              FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_sub_stats <- cbind(kot_sub_stats[,1:3], as.data.frame(kot_sub_stats[,4]))
# Change colnames
colnames(kot_sub_stats) <- c("Condition", "Skill", "SubSkill", "N", "Mean", "SD", "SEM")

# Checking values with ezStats
kot_sub_ezstats <- ezStats(
  data = subset(df_kot, df_kot$Interval  != 8 & df_kot$Interval != 16 & df_kot$Interval != 24 & df_kot$Interval != 41 & df_kot$Interval != 49 & df_kot$Interval != 57)
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , check_args = TRUE
)

# 5. Average KOT for each note
# For each individual
kot_seq <- aggregate(KOT~SubNr*Condition*Skill*Interval, data = df_kot,
                     FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x))})
kot_seq <- cbind(kot_seq[,1:4], as.data.frame(kot_seq[,5]))
# Change colnames
colnames(kot_seq) <- c("SubNr", "Condition", "Skill", "Interval", "N", "Mean", "SD")

# Group mean
kot_seq_stats <- aggregate(Mean~Condition*Skill*Interval, data = kot_seq,
                           FUN = function(x){round(c(length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))), 4)})
kot_seq_stats <- cbind(kot_seq_stats[,1:3], as.data.frame(kot_seq_stats[,4]))
# Change colnames
colnames(kot_seq_stats) <- c("Condition", "Skill", "Interval", "N", "Mean", "SD", "SEM")

# Add order info
kot_ch_stats$LabelOrder[kot_ch_stats$Skill == "articulation"] <- 1
kot_ch_stats$LabelOrder[kot_ch_stats$Skill == "dynamics"] <- 2
kot_ch_sub_stats$LabelOrder[kot_ch_sub_stats$Skill == "articulation"] <- 1
kot_ch_sub_stats$LabelOrder[kot_ch_sub_stats$Skill == "dynamics"] <- 2
kot_sub_stats$LabelOrder[kot_sub_stats$Skill == "articulation"] <- 1
kot_sub_stats$LabelOrder[kot_sub_stats$Skill == "dynamics"] <- 2

# ####################################
# # Diff between first/end and others
# ####################################
# # Define phrases
# # For intervals
# ls_legato <- list(c(1:7), c(17:23), c(42:48), c(58:64))
# ls_staccato <- list(c(9:15), c(25:31), c(34:40), c(50:56))
# ls_forte <- list(c(1:7), c(17:23), c(42:48), c(58:64))
# ls_piano <- list(c(9:15), c(25:31), c(34:40), c(50:56))
# 
# # Difference between first+end and other notes within each phrase
# df_diff <- data.frame()
# df_diff_legato <- data.frame()
# df_diff_staccato <- data.frame()
# for (cond in unique(kot_seq$Condition)){
#   for (skill in unique(kot_seq$Skill)){
#     df_current <- kot_seq %>% dplyr::filter(kot_seq$Condition == cond & kot_seq$Skill == skill)
#     if (skill == "articulation"){
#       for (phrase in 1:length(ls_legato)){
#         first <- df_current$KOT[,2][df_current$Interval == ls_legato[[phrase]][1]]
#         end <- df_current$KOT[,2][df_current$Interval == ls_legato[[phrase]][7]]
#         others <- mean(df_current$KOT[,2][df_current$Interval == ls_legato[[phrase]][2]],
#                        df_current$KOT[,2][df_current$Interval == ls_legato[[phrase]][3]],
#                        df_current$KOT[,2][df_current$Interval == ls_legato[[phrase]][4]],
#                        df_current$KOT[,2][df_current$Interval == ls_legato[[phrase]][5]],
#                        df_current$KOT[,2][df_current$Interval == ls_legato[[phrase]][6]])
#         df_diff_legato <- data.frame(cond, skill, "Legato", phrase, mean(first, end)-others)
#         colnames(df_diff_legato) <- c("Condition", "Skill", "SubSkill", "PhraseNo", "Diff")
#         df_diff <- rbind(df_diff, df_diff_legato)
#       }
#       for (phrase in 1:length(ls_staccato)){
#         first <- df_current$KOT[,2][df_current$Interval == ls_staccato[[phrase]][1]]
#         end <- df_current$KOT[,2][df_current$Interval == ls_staccato[[phrase]][7]]
#         others <- mean(df_current$KOT[,2][df_current$Interval == ls_staccato[[phrase]][2]],
#                        df_current$KOT[,2][df_current$Interval == ls_staccato[[phrase]][3]],
#                        df_current$KOT[,2][df_current$Interval == ls_staccato[[phrase]][4]],
#                        df_current$KOT[,2][df_current$Interval == ls_staccato[[phrase]][5]],
#                        df_current$KOT[,2][df_current$Interval == ls_staccato[[phrase]][6]])
#         df_diff_staccato <- data.frame(cond, skill, "Staccato", phrase, mean(first, end)-others)
#         colnames(df_diff_staccato) <- c("Condition", "Skill", "SubSkill", "PhraseNo", "Diff")
#         df_diff <- rbind(df_diff, df_diff_staccato)
#       }
#     }
#   }
# }
# 
# kot_diff <- aggregate(Diff~Condition*Skill*SubSkill, data = df_diff,
#                       FUN = function(x){c(N = length(x), mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x)))})
# kot_diff <- cbind(kot_diff, as.data.frame(kot_diff[,4]))
# 
# p_kot_diff <- ggplot(data = kot_diff, aes(x = SubSkill, y = mean, fill = Condition)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
#                 width=.2, position = position_dodge(.9)) +
#   labs(y = "Difference (ms)") + #coord_cartesian(ylim = c(100, 250)) +
#   theme_classic()
# p_kot_diff
# ggsave("./plot/kot/p_kot_diff.png", plot = p_kot_diff, dpi = 600, width = 5, height = 4)

####################################
# Plots
####################################
p_kot <- ggplot(data = kot_stats, aes(x = Skill, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(100, 230)) +
  theme_classic()
p_kot

p_kot_ch <- ggplot(data = kot_ch_stats, aes(x = Skill, y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(x = "Skill", y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(100, 230)) +
  theme_classic()
p_kot_ch

p_kot_ch_sub <- ggplot(data = kot_ch_sub_stats, aes(x = reorder(SubSkill, LabelOrder), y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(100, 230)) +
  theme_classic()
p_kot_ch_sub

p_kot_sub <- ggplot(data = kot_sub_stats, aes(x = reorder(SubSkill, LabelOrder), y = Mean, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                width=.2, position = position_dodge(.9)) +
  labs(x = "SubSkill", y = "Mean KOT (ms)") + #coord_cartesian(ylim = c(100, 230)) +
  theme_classic()
p_kot_sub

p_kot_seq <- ggplot(data = kot_seq_stats, aes(x = Interval, y = Mean, group = Condition, shape = Condition, colour = Condition)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 188, linetype = "dashed") + # Tempo
  facet_grid(Skill ~ .) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width=.2,
                position = position_dodge(.05)) + 
  labs(x = "Interval", y = "Mean KOT (ms)") + scale_x_continuous(breaks=seq(1,66,1)) +
  theme_classic()
p_kot_seq

# Save plots
# png files
ggsave("./3_stats/plot/kot/p_kot.png", plot = p_kot, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/kot/p_kot_ch.png", plot = p_kot_ch, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/kot/p_kot_ch_sub.png", plot = p_kot_ch_sub, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/kot/p_kot_sub.png", plot = p_kot_sub, dpi = 600, width = 5, height = 4)
ggsave("./3_stats/plot/kot/p_kot_seq.png", plot = p_kot_seq, dpi = 600, width = 15, height = 4)

####################################
### Statistics
####################################
# # 1. Normality check
# kot_ch_sub_norm <- data.frame(unlist(by(kot_ch_sub$Mean, kot_ch_sub$SubSkill, shapiro.test)))
# colnames(kot_ch_sub_norm) <- c("LtoS", "StoL", "FtoP", "PtoF")
# 
# kot_sub_norm <- data.frame(cbind(unlist(shapiro.test(kot_sub$Mean[kot_sub$SubSkill == "Legato"])),
#                       unlist(shapiro.test(kot_sub$Mean[kot_sub$SubSkill == "Staccato"])),
#                       unlist(shapiro.test(kot_sub$Mean[kot_sub$SubSkill == "Forte"])),
#                       unlist(shapiro.test(kot_sub$Mean[kot_sub$SubSkill == "Piano"]))))
# 
# colnames(kot_sub_norm) <- c("Legato", "Staccato", "Forte", "Piano")
# 
# # Transpose
# kot_norm <- t(kot_norm)
# kot_ch_norm <- t(kot_ch_norm)
# kot_ch_sub_norm <- t(kot_ch_sub_norm)
# kot_sub_norm <- t(kot_sub_norm)
# 
# # Export the results
# write.csv(kot_norm, file = "./3_stats/kot/kot_norm.csv", row.names = FALSE)
# write.csv(kot_ch_norm, file = "./3_stats/kot/kot_ch_norm.csv", row.names = FALSE)
# write.csv(kot_ch_sub_norm, file = "./3_stats/kot/kot_ch_sub_norm.csv", row.names = FALSE)
# write.csv(kot_sub_norm, file = "./3_stats/kot/kot_sub_norm.csv", row.names = FALSE)

# Two-way ANOVA
# kot_ch_sub
kot_ch_sub_aov <- ezANOVA(
  data = subset(df_kot, df_kot$Interval == 8 | df_kot$Interval == 16 & df_kot$Interval == 24 & 
                  df_kot$Interval == 41 & df_kot$Interval == 49 & df_kot$Interval == 57)
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(kot_ch_sub_aov)
write.csv(kot_ch_sub_aov$ANOVA, file = "./3_stats/kot/kot_sub_aov.csv")

# kot_sub
kot_sub_aov <- ezANOVA(
  data = subset(df_kot, df_kot$Interval != 8 & df_kot$Interval != 16 & df_kot$Interval != 24 & 
                  df_kot$Interval != 41 & df_kot$Interval != 49 & df_kot$Interval != 57)
  , dv = .(KOT)
  , wid = .(SubNr)
  , within = .(Condition, SubSkill)
  , type = 3
  , detailed = TRUE
)
print(kot_sub_aov)
write.csv(kot_sub_aov$ANOVA, file = "./3_stats/kot/kot_sub_aov.csv")
write.csv(kot_sub_aov$`Mauchly's Test for Sphericity`, file = "./3_stats/kot/kot_sub_aov_mau.csv")
write.csv(kot_sub_aov$`Sphericity Corrections`, file = "./3_stats/kot/kot_sub_aov_sph.csv")

# posthoc comparison
kot_sub_ph <- aov(KOT~Condition*SubSkill, data = subset(df_kot, df_kot$Interval != 8 & df_kot$Interval != 16 & df_kot$Interval != 24 & 
                                                               df_kot$Interval != 41 & df_kot$Interval != 49 & df_kot$Interval != 57))
kot_sub_ph <- TukeyHSD(kot_sub_ph)
print(kot_sub_ph)
write.csv(kot_sub_ph$`Condition:SubSkill`, file = "./3_stats/kot/kot_sub_ph.csv")

# kot_firstEnd
kot_firstEnd_anova <- ezANOVA(
  data = subset(df_kot, df_kot$Interval == 1 | df_kot$Interval == 7 | df_kot$Interval == 9 | df_kot$Interval == 15 | df_kot$Interval == 17 | df_kot$Interval == 23
                | df_kot$Interval == 25 | df_kot$Interval == 31 | df_kot$Interval == 34 | df_kot$Interval == 40 | df_kot$Interval == 42 | df_kot$Interval == 48
                | df_kot$Interval == 50 | df_kot$Interval == 56 | df_kot$Interval == 58 | df_kot$Interval == 64)
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
kot_firstEnd_posthoc <- aov(KOT~Condition*SubSkill, data = subset(df_kot, df_kot$Interval == 1 | df_kot$Interval == 7 | df_kot$Interval == 9 | df_kot$Interval == 15 | df_kot$Interval == 17 | df_kot$Interval == 23
                                                               | df_kot$Interval == 25 | df_kot$Interval == 31 | df_kot$Interval == 34 | df_kot$Interval == 40 | df_kot$Interval == 42 | df_kot$Interval == 48
                                                               | df_kot$Interval == 50 | df_kot$Interval == 56 | df_kot$Interval == 58 | df_kot$Interval == 64))
kot_firstEnd_posthoc <-TukeyHSD(kot_firstEnd_posthoc)
write.csv(kot_firstEnd_posthoc$`Condition:SubSkill`, file = "./stats/withoutOrder/kot_firstEnd_posthoc.csv")

### Normality
q_plot <- ggplot(subset(df_kot, df_kot$Interval != 8 & df_kot$Interval != 16 & df_kot$Interval != 24 & 
                          df_kot$Interval != 41 & df_kot$Interval != 49 & df_kot$Interval != 57), aes(sample = KOT, shape = SubSkill, color = SubSkill)) +
  stat_qq() + theme_classic()
q_plot

# Save plots
# png files
ggsave("./plot/kot/q_plot.png", plot = q_plot, dpi = 600, width = 5, height = 4)
