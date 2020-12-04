## ----setup, include = FALSE--------------------------------------------------------
# packages
# data manipulation
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
# plot
if (!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}


## ----file, include = FALSE---------------------------------------------------------
filename_q = "./questionnaire.csv"


## ----extract, echo = FALSE---------------------------------------------------------
# read csv files
dt <- fread(filename_q, header = T, sep = ",", dec = ".", na.string = "NA")

# change some colnames
colnames(dt)[2] <- "SubNr"
colnames(dt)[3] <- "Age"
colnames(dt)[4] <- "Gender"
colnames(dt)[7] <- "Handedness"

# change some characteristics
dt$Age <- as.numeric(dt$Age)

# extract the total number of years of training in piano (input manually based on the original csv file)
dt$PianoTotalPractice <- c(14, 20, 27, 21, 10, 16, 9, 13, 25, 14, 13, 11, 6, 4, 6, 8, 21, 15, 18, 7, 11, 12, 12, 18, 12, 5, 5, 10, 16, 10, 8, 22, 10, 5, 4, 14)

# exclude participants
dt_included <- dt[SubNr != 1 & SubNr != 3 & SubNr != 8 & SubNr != 14 & SubNr != 16]

print(dt_included)


## ----1, echo = FALSE---------------------------------------------------------------
data.table("Answer" = dt_included$`Did you try to perform differently when you are asked to play as a teacher during the experiment? If so, please describe how you changed your performance?`)


## ----2, echo = FALSE---------------------------------------------------------------
data.table("Answer" = dt_included$`Have you noticed anything special regarding the tasks in the experiment? If any, please describe below.`)


## ----export, include = FALSE-------------------------------------------------------
knitr::purl("questionnaire.Rmd")

