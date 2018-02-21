library(readxl)
require("httr")
library(tidyr) # drop_na
library(dplyr)

companies <- read.csv("~/datathon/extra_data/companies.csv")

# Read the companies' identifier
tickers <- companies$TICKER[21:(length(tickers))]
tickers_test <- tickers[1:3]

#https://api.intrinio.com/historical_data.csv?identifier=AAPL&item=marketcap
get_url <- function(identifier) {
  paste("https://api.intrinio.com/historical_data.csv?identifier=", identifier, "&item=marketcap",sep="")
}

try_get_historical <- function(identifier) {
  out <- tryCatch(
    {
      res <- get_historical(identifier)
      res
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", identifier))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", identifier))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(res)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      message(paste("Processed URL:", identifier))
      message("Some other message at the end")
    }
  )    
  return(out)
}

get_historical <- function(identifier) {
  username <- "f2d6e8097e724756eb67d2c10cf08041"
  password <- "2fa2489c645a8218cd12e4fa665e6bb4"
  identifier_url <- get_url(identifier)
  tp <- GET(identifier_url, authenticate(username, password, type = "basic"))
  z <- read.csv(text=content(tp, "text"), skip=1)
  
  bla <- z %>% filter( DATE=="2017-12-29")
  bla[1,2]
}

get_from_web <- function() {
  market_cap_chosen_date <- lapply(tickers, try_get_historical)

  df <- data.frame(tickers=tickers, market_cap=unlist(market_cap_chosen_date)) %>% drop_na
  write.csv(df, file="extra_data/market_cap_names.csv")
}

get_from_disk <- function() {
  read.csv("extra_data/market_cap_names.csv")
}