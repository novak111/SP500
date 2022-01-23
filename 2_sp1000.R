# Prerequisites ----------------------------------------------------------------
library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(plotly)     # Interactive plots


# Web Scraping: Get the List of S&P1000 Stocks ------------------------------------------------------------------------------------

# Web-scrape S&P1000 stock list, on this page are more tables, therefore html_nodes is used instead of html_node
#I am interested in the third table, that is in format list of list, I need to extract the first list from this "table" and add it as an argument to html_table function
#the variables has different names in this htlm table 

sp_400 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_400_companies") %>%
  html_node("table.wikitable") %>%
  html_table() %>%
  select('Ticker symbol') %>%
  as_tibble()

sp_1000 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_1000_companies") %>%
  html_nodes("table.wikitable") 


sp_1000=sp_1000[3][[1]] %>%
  html_table() %>%
  select('Ticker Symbol', 'Company', 'GICS Economic Sector', 'GICS Sub-Industry') %>%
  as.tibble() %>%
  mutate(company.size = "Small")

#relevel company size, companies in sp_400 has Medium size

sp_1000[sp_1000$`Ticker Symbol` %in% sp_400$`Ticker symbol`,  'company.size'] = "Medium"
rm(sp_400)

#change the variables names to be same as in sp_1000

names(sp_1000) <- c('Symbol', 'Security', 'GICS Sector', 'GICS Sub Industry', 'company.size')

# Format names
names(sp_1000) <- sp_1000 %>%
  names() %>%
  str_to_lower() %>%
  make.names 

# zamena tecky za pomlcku u symbolu spolecnosti, tecka zpusobovala nasledne chyby pri stahovani dat
sp_1000[sapply("\\.", function(y) grep(y,sp_1000$symbol)), ]
sp_1000$symbol <- (sapply("\\.", function(y) str_replace(sp_1000$symbol,y,"-")))
sp_1000[sapply("\\-", function(y) grep(y,sp_1000$symbol)), ]

#kontrola poctu symbolu a poctu spolecnosti
sp_1000 %>% 
  lapply(function(x) x %>% unique() %>% length()) %>%
  unlist()
#995 symbolu, 1001 spolecnosti, 1002 pozorovani

sp_1000 %>%
  group_by(security) %>%
  summarize(count = n()) %>%
  filter(count > 1)

sp_1000 %>% 
  filter(security == "Central Garden & Pet Company")
#Central Garden & Pet Company two tickers CENT and CENTA
#remove CENTA
sp_1000 <- sp_1000 %>% 
  filter(symbol != "CENTA")

#kontrola poctu symbolu a poctu spolecnosti
sp_1000 %>% 
  lapply(function(x) x %>% unique() %>% length()) %>%
  unlist()

#duplicated symbols
remsym <- sp_1000 %>%
  group_by(symbol) %>%
  summarize(count = n()) %>%
  filter(count > 1)

#securities to be removed
remsec <- sp_1000 %>% 
  filter(symbol %in% unlist((remsym)[1]))

remsec <- remsec[ ,2] %>%
  unlist() %>%
  sort()
#duplicies that will be removed
remsec= remsec[seq(2,14,2)]

sp_1000 <- sp_1000 %>% 
  filter(!security %in% remsec)
#now there are just unique values in the dataset
#kontrola poctu symbolu a poctu spolecnosti
sp_1000 %>% 
  lapply(function(x) x %>% unique() %>% length()) %>%
  unlist()
rm(remsec, remsym)

#ACXM and others caused download error
sp_1000 <- sp_1000 %>% 
  filter(!symbol %in% c("ACXM", "AHL", "BOFI","BEAT","CVG", "DNB","EDR", "ESIO", "ENG","EGN", "ESND", "EGL", "GNBC","GOV","HBHC", "HYH","ILG", "KLXI", "KS", "LHO", "LPNT","OCLR"
                        ,"PAY","PERY","SONC","SVU", "VVC", "XOXO"))


# Mapping the Functions (might take few minutes) -----------------------------------------------------------------------------------------------------------------
from <- "2010-01-01"
to   <- "2019-03-14"
sp_1000 <- sp_1000 %>%
  mutate(
    stock.prices = map(symbol,
                       function(.x) get_stock_prices(.x,
                                                     return_format = "tibble",
                                                     from = from,
                                                     to   = to)
    )
  )

#u nekterych nested tabulek chybi datum a pak funkce get_log_returns vraci chybu kvuli order.by(Date)
#musim mapovat funkci pres stock prices, takovou, aby mi vratila true, kdyz ve sloupci datum, chybihodnota a pak smazala prislusny symbol(cely radek)

TRUE %in% is.na(sp_1000$stock.prices[[1]][,1]) #vraci true/false pro 1. radek

#funkce vraci true, kdyz pro nejaou firmu ve vnorene tabulce chybi nejaka hodnota ve sloupci datum(prvni sloupec)
NA_date <- function(x){
  TRUE %in% is.na(x[,1])
}

#Funkce je mapovana pres vsechny vnorene tabulky, vysledek logicke hodnoty v listu, list preveden na vektor a logicke hodnoty invertovany
#do NA_sub se ulozi logicky vektor, TRUE pro radky z tabulky sp_1000, kde nejsou chybejici hodnoty a chci je zachovat
#False pro radky kde vnorene tabulka mela v datu chybejici hodnota a proto bude odstarena
NA_sub <- !unlist(map(sp_1000$stock.prices, function(.x) NA_date(.x)))
sp_1000 <- sp_1000[NA_sub,]
rm(NA_sub)
#aplikace funkce get_log_returns
sp_1000 <- sp_1000 %>%
  mutate(
    log.returns  = map(stock.prices,
                       function(.x) get_log_returns(.x, return_format = "tibble")),
    mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
    sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
    n.trade.days = map_dbl(stock.prices, nrow)
  )

#reducing and exporting data -----------------------------------------------------------------------------------------------------------------------

sp_1000_rd <- sp_1000[,colnames(sp_500)[c(-6,-7)]]
write.csv(sp_1000_rd, file = "sp_1000_rd")

