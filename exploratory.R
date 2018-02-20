library(lubridate)
library(dplyr)
library(readxl)

#cycling <- read.csv("~/datathon/Cycling/cycling.csv", sep=";")
#unique(cycling$qid) # 11 bikers

credit <- read_excel("~/datathon/Credit/Credit_DataSet.xlsx", sheet=2,
                     col_types=c("skip", "text", "text",
                                 "text", "text", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "text", "text",
                                 "text", "text", "skip",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric", "numeric",
                                 "numeric", "numeric"))

# Convert selected columns to factors. 




