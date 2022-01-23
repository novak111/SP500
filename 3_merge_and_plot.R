# Prerequisites ----------------------------------------------------------------
library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(plotly)     # Interactive plots



# The reduced data frames can be loaded from csv in case of occurrence of  any error during downloading the data -----------------------
#
setwd(getwd())
sp_500_rd <- read.csv("sp_500_rd", row.names = 1)
sp_1000_rd <- read.csv("sp_1000_rd", row.names = 1)
#

# Merging sp_500_rd and sp_1000_rd ------------------------------------------------------------------------------------------------------

sp_1500_rd <- rbind(sp_500_rd, sp_1000_rd)
rm(sp_500_rd, sp_1000_rd)
# saving the sp_1500_rd
write.csv(sp_1500_rd, file = "sp_1500_rd")
# frequency of companies with mean log returns higher than 0
sp_1500_rd %>%
  filter(company.size == "Large" & mean.log.returns > 0) %>%
  count()

sp_1500_rd %>%
  filter(company.size == "Medium" & mean.log.returns > 0) %>%
  count()

sp_1500_rd %>%
  filter(company.size == "Small" & mean.log.returns > 0) %>%
  count()


# SP1500 Visualizing the Results with Plotly ----------------------------------------------------------------------------------------------------
plot_ly(data   = sp_1500_rd,
        type   = "scatter",
        mode   = "markers",
        x      = ~ sd.log.returns,
        y      = ~ mean.log.returns,
        color  = ~ company.size,
        colors = c('#e41a1c','#377eb8','#4daf4a'),
        size   = ~ n.trade.days,
        text   = ~ str_c("<em>", security, "</em><br>",
                         "Ticker: ", symbol, "<br>",
                         "Sector: ", gics.sector, "<br>",
                         "Sub Sector: ", gics.sub.industry, "<br>",
                         "No. of Trading Days: ", n.trade.days),
        marker = list(opacity = 0.8,
                      symbol = 'circle',
                      sizemode = 'diameter',
                      sizeref = 4.0,
                      line = list(width = 2, color = '#FFFFFF'))
) %>%
  layout(title   = 'S&P1500 Analysis: Stock Risk vs Reward',
         xaxis   = list(title = 'Risk: StDev Log Returns',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwidth = 2),
         yaxis   = list(title = 'Reward: Mean Log Returns',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwith = 2),
         margin = list(l = 100,
                       t = 100,
                       b = 100),
         font   = list(color = '#FFFFFF'),
         paper_bgcolor = 'rgb(0, 0, 0)',
         plot_bgcolor = 'rgb(0, 0, 0)')

#SP_500 ------------------------------------------------------------------------------------------------------------------------------------

plot_ly(data   = sp_1500_rd[which(sp_1500_rd$company.size == "Large"), ],
        type   = "scatter",
        mode   = "markers",
        x      = ~ sd.log.returns,
        y      = ~ mean.log.returns,

        size   = ~ n.trade.days,
        text   = ~ str_c("<em>", security, "</em><br>",
                         "Ticker: ", symbol, "<br>",
                         "Sector: ", gics.sector, "<br>",
                         "Sub Sector: ", gics.sub.industry, "<br>",
                         "No. of Trading Days: ", n.trade.days),
        marker = list(opacity = 0.8,
                      color = '#e41a1c',
                      symbol = 'circle',
                      sizemode = 'diameter',
                      sizeref = 4.0,
                      line = list(width = 2, color = '#FFFFFF'))
) %>%
  layout(title   = 'S&P500 Analysis: Stock Risk vs Reward',
         xaxis   = list(title = 'Risk: StDev Log Returns',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwidth = 2),
         yaxis   = list(title = 'Reward: Mean Log Returns',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwith = 2),
         margin = list(l = 100,
                       t = 100,
                       b = 100),
         font   = list(color = '#FFFFFF'),
         paper_bgcolor = 'rgb(0, 0, 0)',
         plot_bgcolor = 'rgb(0, 0, 0)')

#SP_400 ------------------------------------------------------------------------------------------------------------------------------------

plot_ly(data   = sp_1500_rd[which(sp_1500_rd$company.size == "Medium"), ],
        type   = "scatter",
        mode   = "markers",
        x      = ~ sd.log.returns,
        y      = ~ mean.log.returns,

        size   = ~ n.trade.days,
        text   = ~ str_c("<em>", security, "</em><br>",
                         "Ticker: ", symbol, "<br>",
                         "Sector: ", gics.sector, "<br>",
                         "Sub Sector: ", gics.sub.industry, "<br>",
                         "No. of Trading Days: ", n.trade.days),
        marker = list(opacity = 0.8,
                      color = '#377eb8',
                      symbol = 'circle',
                      sizemode = 'diameter',
                      sizeref = 4.0,
                      line = list(width = 2, color = '#FFFFFF'))
) %>%
  layout(title   = 'S&P400 Analysis: Stock Risk vs Reward',
         xaxis   = list(title = 'Risk: StDev Log Returns',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwidth = 2),
         yaxis   = list(title = 'Reward: Mean Log Returns',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwith = 2),
         margin = list(l = 100,
                       t = 100,
                       b = 100),
         font   = list(color = '#FFFFFF'),
         paper_bgcolor = 'rgb(0, 0, 0)',
         plot_bgcolor = 'rgb(0, 0, 0)')

#SP_600 ------------------------------------------------------------------------------------------------------------------------------------

plot_ly(data   = sp_1500_rd[which(sp_1500_rd$company.size == "Small"), ],
        type   = "scatter",
        mode   = "markers",
        x      = ~ sd.log.returns,
        y      = ~ mean.log.returns,
        
        size   = ~ n.trade.days,
        text   = ~ str_c("<em>", security, "</em><br>",
                         "Ticker: ", symbol, "<br>",
                         "Sector: ", gics.sector, "<br>",
                         "Sub Sector: ", gics.sub.industry, "<br>",
                         "No. of Trading Days: ", n.trade.days),
        marker = list(opacity = 0.8,
                      color = '#4daf4a',
                      symbol = 'circle',
                      sizemode = 'diameter',
                      sizeref = 4.0,
                      line = list(width = 2, color = '#FFFFFF'))
) %>%
  layout(title   = 'S&P600 Analysis: Stock Risk vs Reward',
         xaxis   = list(title = 'Risk: StDev Log Returns',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwidth = 2),
         yaxis   = list(title = 'Reward: Mean Log Returns',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwith = 2),
         margin = list(l = 100,
                       t = 100,
                       b = 100),
         font   = list(color = '#FFFFFF'),
         paper_bgcolor = 'rgb(0, 0, 0)',
         plot_bgcolor = 'rgb(0, 0, 0)')
