# Prerequisites ----------------------------------------------------------------
library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(plotly)     # Interactive plots

# packages for creating the models
library(forecast)
library(rugarch)
library(tseries)



setwd(getwd())
sp_1500_rd <- read.csv("sp_1500_rd", row.names = 1)

# selecting companies with the best reward/risk ratio (just the companies with positive mean log returns) with adequate number of trading days

sp_best <- sp_1500_rd %>%
  dplyr::filter(mean.log.returns >= 0) %>%
  mutate(sd.log.returns/mean.log.returns)


# DPZ was selected, because the better companies has fewer trading days -----------------------------------------------------


#geting prices and log.returns for DPZ --------------------------------------------------------------------------------------
DPZ %>%
from <- "2010-01-01"
to   <- "2019-03-14"
DPZ <- getSymbols(Symbols = 'DPZ', auto.assign = FALSE, from = from, to = to) 
log.returns <- as_tibble(periodReturn(DPZ$DPZ.Adjusted, type = 'log', period = 'daily'))

#filtering just the variables needed and setting right format ---------------------------------------------------------------
DPZ <- as_tibble(DPZ, rownames = 'Date') %>%
  select(Date, DPZ.Adjusted) %>%
  mutate(as_vector(log.returns))
names(DPZ) <- c('Date', 'Adjusted', 'Log.Returns')
DPZ$Date <- as.Date(DPZ$Date) 


# Plots -------------------------------------------------------------------------------------------------------------------------------
DPZ %>%
  ggplot(aes(x = Date, y = Adjusted, group = 1)) +
  geom_line() +
  scale_x_date(date_labels = "%m-%Y") +
  labs(title = "Domino's Pizza, Inc. Adjusted Price", y = "Adjusted Price", x = "") 

DPZ %>%
  ggplot(aes(x = Date, y = Log.Returns, group = 1)) +
  geom_line() +
  scale_x_date(date_labels = "%m-%Y") +
  labs(title = "Domino's Pizza, Inc. Log  Returns", y = "Log  Returns", x = "") 

# count of possitive Log. Returns
DPZ %>%
  filter(Log.Returns >0) %>%
  count()
#transfer log returns to xts object in order to plot properly x-axis
log.ret <- xts(DPZ[,"Log.Returns"], order.by = DPZ$Date)
colnames(log.ret) <- "Log.Returns"

arimaModel = auto.arima(log.ret, ic = 'aic',
                        stepwise = F)
plot(fitted(arimaModel))
#arima 4 0 1
Acf(resid(arimaModel))
Acf(resid(arimaModel)^2)

garchModel = garch(DPZ$Log.Returns, trace = TRUE)
Acf(garchModel$res[-1])
Acf(garchModel$res[-1]^2)


## Fit the model
ourSpec = ugarchspec(mean.model = list(armaOrder = arimaModel$arma[1:2], include.mean = FALSE), 
                     variance.model = list(garchOrder = c(1, 1)))

fit = ugarchfit(spec = ourSpec, data = log.ret)
plot(fit)

coef(fit) # estimated parameters
sigma(fit) # conditional standard deviations


DPZ <- cbind(DPZ, as_tibble(fitted(fit)))
colnames(DPZ)[4] <- "Fitted"
#polt of fitted values using ggplot2
DPZ %>%
  ggplot(aes(x = Date, y = Fitted, group = 1)) +
  geom_line() +
  scale_x_date(date_labels = "%m-%Y") +
  labs(title = "Domino's Pizza, Inc. - Fitted log returns", y = "Log  Returns", x = "") 



plot(fitted(fit)) # fitted values

plot(residuals(fit)) # residuals
plot(residuals(fit, standardize = TRUE)) # standardized residuals

#forecast
#forec = ugarchforecast(fit)
#plot(forec)



