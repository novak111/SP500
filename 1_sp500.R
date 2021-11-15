
# Prerequisites ----------------------------------------------------------------------------------------------
library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(plotly)     # Interactive plots

# Web Scraping: Get the List of S&P500 Stocks ----------------------------------------------------------------
# Web-scrape S&P500 stock list
sp_500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
    html_node("table.wikitable") %>%
    html_table() %>%
    select('Symbol', 'Security', 'GICS Sector', 'GICS Sub-Industry') %>%
    as_tibble() %>%
    mutate(company.size = "Large") #add variable company size based on market capitaliyation

# Format names
names(sp_500) <- sp_500 %>%
    names() %>%
    str_to_lower() %>%
    make.names()

# Rewrite "." by "-", for ticker symbols with "." getSymbols() returns an error 
sp_500[c(sapply("\\.", function(y) grep(y,sp_500$symbol))), ]
sp_500$symbol <- (sapply("\\.", function(y) str_replace(sp_500$symbol,y,"-")))
sp_500[c(sapply("\\-", function(y) grep(y,sp_500$symbol))), ]

sp_500 %>% 
  lapply(function(x) x %>% unique() %>% length()) %>%
  unlist()

# check for duplicities
sp_500 %>%
  group_by(security) %>%
  summarize(count = n()) %>%
  filter(count > 1)

# Creating Functions to Map -----------------------------------------------------------------------------------         
get_stock_prices <- function(ticker, return_format = "tibble", ...) {
    # Get stock prices
    stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...)
    # Rename
    names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    # Return in xts format if tibble is not specified
    if (return_format == "tibble") {
        stock_prices <- stock_prices_xts %>%
          as_tibble(rownames = "Date") %>%
          mutate(Date = ymd(Date))
    } else {
        stock_prices <- stock_prices_xts
    }
    stock_prices
}

get_log_returns <- function(x, return_format = "tibble", period = 'daily', ...) {
    # Convert tibble to xts
    if (!is.xts(x)) {
        x <- xts(x[,-1], order.by = x$Date)
    }
    # Get stock prices
    log_returns_xts <- periodReturn(x = x$Adjusted, type = 'log', period = period, ...)
    # Rename
    names(log_returns_xts) <- "Log.Returns"
    # Return in xts format if tibble is not specified
    if (return_format == "tibble") {
        log_returns <- log_returns_xts %>%
            as_tibble(rownames = "Date") 
    } 
    else {
        log_returns <- log_returns_xts
    }
    log_returns
}

# Getting stock prices (might take few minutes) -------------------------------------------------------------------
from <- "2010-01-01"
to   <- "2021-09-30"
sp_500 <- sp_500 %>%
    mutate(
        stock.prices = map(symbol,
                           function(.x) get_stock_prices(.x,
                                                         return_format = "tibble",
                                                         from = from,
                                                         to   = to)
        )
        )

# Within nested tables (dates and stock prices for each ticker symbol) might be some dates missing-----------------         
# Checking for mising dates and filtering them out
NA_date <- function(x){
  TRUE %in% is.na(x[,1])
}
         
NA_sub <- !unlist(map(sp_500$stock.prices, function(.x) NA_date(.x)))
sp_500 <- sp_500[NA_sub,]
rm(NA_sub)
# Calculating log.returns -------------------------------------------------------------------------------------------
sp_500 <- sp_500 %>%
    mutate(
        log.returns  = map(stock.prices,
                           function(.x) get_log_returns(.x, return_format = "tibble")),
        mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
        sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
        n.trade.days = map_dbl(stock.prices, nrow)
    )
#reducing and exporting data -----------------------------------------------------------------------------------------
sp_500_rd <- sp_500[,colnames(sp_500)[c(-6,-7)]]
write.csv(sp_500_rd, file = "sp_500_rd")
