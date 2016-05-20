rm(list = ls())
setwd("~/Documents/Jobs Paper")
library(foreign); library(dplyr)

load("Data/Saved Datasets/firmDataCleaned.RDA")

##Summarise at the isic year level to total empl and revenue
isicData <- ungroup(firmData) %>% group_by(year, isic) %>% summarise(numFirms = n_distinct(number_id), totEmp = sum(numberofemployees, na.rm = T), revenue = sum(operatingrevenueturnover, na.rm = T))

## Load and join the Unido data with global trends
unidoTrend <- read.dta("Data/unidotrend.dta")
isicData <- left_join(isicData, unidoTrend, by = "isic")

## A russian sector is considered to be in a surge if the (Actual totRev - expected tot Rev)/Expected tot Rev is in the 75th percentile of global long term trend. The mean beta is calculated from a long term global trend for each isic.
isicData <- ungroup(isicData) %>% group_by(isic) %>% mutate(lagRev = lag(revenue, 1)) %>% ungroup() %>% mutate(trend_rev = lagRev * (1 + beta_mean)) %>% mutate(trend_dev = (revenue - trend_rev)/trend_rev, grTrend = ifelse(trend_dev > quartile75, "surge", ifelse(trend_dev < quartile25, "slump", "normal"))) %>% select(isic, year, grTrend) %>% filter(year > 2003)
isicData$grTrend <- as.factor(isicData$grTrend)

save(isicData, file = "Data/Saved Datasets/isicData.RDA")