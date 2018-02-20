library(dplyr)
library(readxl)

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
                                 "numeric", "numeric")) %>%
      mutate_if(is.character, as.factor) # Convert all character columns to factor
