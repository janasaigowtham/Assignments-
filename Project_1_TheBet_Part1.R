

# Deliverable ===================================  
# Project 1: The Bet, Part 1 of 3

# The “Bet” between Julian Simon and Paul Ehrlich, 1980
# During the late 70s business professor Julian Simon challenged biologist 
# Paul Ehrlich to choose any raw material he wanted and a date more than a year away.  
# The wager: the inflation-adjusted prices would decrease rather than increase. 
# The bet was formalized in 1980, with 1990 as the payoff date. 

# 
# Part 1  ======================  #
# Download data on tungsten - from the US Geological Survey.  URL is below.
# Calculate the real price of Tungsten by dividing the Nominal Price Series by 
# the Consumer Price Index for All Urban Consumers: All Items from FRED. ("CPIAUCSL")

# Generate a properly documented dygraph of the nominal price of tungsten together
# with the cpi.  

# Generate a properly documented dygraph of the real price of tungsten.  
# How did it perform over the decade (1980-1990) in question?  

# Save your graph as an html file.  Upload it.
# ========================================================================#
# https://pubs.usgs.gov/sir/2012/5188/
# https://pubs.usgs.gov/sir/2012/5188/tables/

#load libraries
suppressWarnings({
  suppressPackageStartupMessages({
    library(tidyverse)
    library(pdfetch)
    library(lubridate)  
    library(ggplot2)
    library(forecast)
    
    library(tsbox)
    library(tsibble)
    library(timetk)
    library(TSstudio)
    
    library(rio)
    
    library(tidyr)
    library(stringr)
    library(dygraphs)
    library(quantmod)
  })
})



#load tungsten
tungsten <- import("https://pubs.usgs.gov/sir/2012/5188/tables/tungsten.xlsx", skip = 2)

head(tungsten); tail(tungsten)
View(tungsten)
tungsten[1:2] = NULL
str(tungsten)

tungsten = na.omit(tungsten)  # to remove the NAs
tungsten.ts <- ts(tungsten, start=c(1959), end=c(2010), frequency=1) #Convert to annual ts
dim(tungsten)

getSymbols("CPIAUCSL", src = "FRED")  # download the consumer price index
cpi = CPIAUCSL
head(cpi)
tail(cpi)
str(cpi)

# convert to time series
cpi.ts = ts(cpi, start = c(1947,1), end = c(2024,7), frequency = 12)  

#convert to annual series
cpi.annual = window(cpi.ts, start = c(1959,1), end = c(2010, 12), frequency = 1)
dim(cpi.annual)
str(cpi.annual)

# convert nominal to real by dividing nominal price by CPI and multiplying by 100
tungsten_real <- (tungsten / cpi.annual) * 100

#Plot the two series (tungsten and the inflation adjusted price 
# of tungsten) with autoplot and autolayer

autoplot(tungsten.ts) + autolayer(tungsten_real)

# Plot the tungstern real price series 
autoplot(tungsten_real)


# Used dygraph to plot 
dygraph(tungsten_real)

# repeat the dygraph of real price of tungsten; identify the period of "the bet:"
# 1980-1990; add bells and whistles
dygraph(tungsten_real,main = "Real Price of Tungsten",
                  ylab = "Real Price") %>%
  dyOptions(
    fillGraph=FALSE, 
    drawGrid = TRUE, 
    colors=c("navyblue")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyCrosshair(direction = "horizontal") %>%
  dyRoller(rollPeriod = 6, showRoller = TRUE) %>%
  dyShading(from = "1980-1-1", 
            to = "1990-12-31", 
            color = "lightpink")%>% 
  dyLegend(show = "follow") %>%
  dyHighlight(
    highlightCircleSize = 5, 
    highlightSeriesBackgroundAlpha = 0.5, 
    hideOnMouseOut = FALSE
  )  

# save to web page (html).  and upload to the Canvas bucket labeled Tungsten.
# How did tungsten perform in the bet portfolio?

# ===================== #