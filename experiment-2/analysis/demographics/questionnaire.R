## ----setup, include = FALSE---------------------------------------------------
# packages
# data manipulation
if (!require("data.table")) {install.packages("data.table"); require("data.table")}


## ----file, include = FALSE----------------------------------------------------
filename_q = "./questionnaire.csv"


## ----extract, echo = FALSE----------------------------------------------------
# read csv files
dt_2 <- fread(filename_q, header = T, sep = ",", dec = ".", na.string = "NA")
dt_2 <- dt_2[!21]

# change some colnames
colnames(dt_2)[2] <- "SubNr"
colnames(dt_2)[3] <- "Age"
colnames(dt_2)[4] <- "Gender"
colnames(dt_2)[7] <- "Handedness"
colnames(dt_2)[18] <- "PianoTotalPractice"
colnames(dt_2)[28] <- "TeachingMusic"

# extract the total number of years of training in piano (input manually based on the original csv file)
dt_2$PianoTotalPractice <- c(13, 15, 15, 16, 9, 10, 24, 8, 19, 6, 12, 10, 21, 23, 15, 22, 22, 24, 17, 15, 12)
dt_2$TeachingPianoYears <- c(NA, 4, NA, 2, NA, NA, NA, NA, 5, NA, NA, NA, 0.5, 9, 11, 2, NA, 8, 2, 1, 1)
dt_2$TeachingPiano <- "No"
dt_2[!is.na(TeachingPianoYears)]$TeachingPiano <- "Yes"

# change some characteristics
dt_2$Age <- as.numeric(dt_2$Age)
dt_2$PianoTotalPractice <- as.numeric(dt_2$PianoTotalPractice)

# exclude participants
dt_2_included <- dt_2[SubNr != 3]

print(dt_2_included)


## ----1, echo = FALSE----------------------------------------------------------
data.table("Answer" = dt_2_included$`Did you try to perform differently when you are asked to play as a teacher during the experiment? If so, please describe how you changed your performance?`)


## ----2, echo = FALSE----------------------------------------------------------
data.table("Answer" = dt_2_included$`Have you noticed anything special regarding the tasks in the experiment? If any, please describe below.`)


## ----export, include = FALSE--------------------------------------------------
knitr::purl("questionnaire.Rmd")

