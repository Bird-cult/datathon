library(dplyr)
library(readxl)
library(MCMCpack)

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
      mutate_if(is.character, as.factor) %>% # Convert all character columns to factor
      mutate(DEFAULT_PROB = DEFAULT_PROB / 100)

# print NA percentage per column
credit %>% summarize_all(function(x) sum(is.na(x))/length(x)) %>% select_if(function(x) x>.05)

# drop na, and columns with sufficiently many NAs
credit_smaller_non_na <- credit %>% select(-c(CURRENCY,PCT_WOMEN_EMPLOYEES, PCT_WOMEN_MGT, WHISTLE_BLOWER_POLICY, ETHICS_POLICY, BRIBERY_POLICY)) %>% drop_na
# Complete case GLM
glm_fit <- glm(DEFAULT_PROB ~ ., data=credit_smaller_non_na, na.action = na.omit)
summary(glm_fit)
 
# Don't fit on INDUSTRY GROUP and INDUSTRY, as multicolinearity perhaps?
glm_bayesian_fit <- MCMCregress(DEFAULT_PROB ~ . -INDUSTRY_GROUP -INDUSTRY, data=credit_smaller_non_na, na.action = na.omit)
