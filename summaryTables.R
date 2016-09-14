rm(list = ls())
setwd("~/Documents/Past WB Projects/Jobs Paper")
library(dplyr); library(lfe); library(ggplot2); library(stargazer); library(RColorBrewer)

load("Data/Saved Datasets/firmDataCleaned.RDA")
load("Data/Saved Datasets/firmIDs.RDA")

firmData <- ungroup(firmData) %>% filter(year != 2011, year > 2003) 

%>% filter(number_id %in% uniqueIds)
# 

##Summarising firm level data
firmTypeSummary <- firmData %>% group_by(year) %>% summarise(nFirms = n(), inSlump = sum(as.character(grTrend) == "slump", na.rm = T), inSurge = sum(as.character(grTrend) == "surge", na.rm = T), newEntrants = sum(as.character(firmType) == "Entering Firm" | as.character(firmType) == "First Year in Ruslana", na.rm = T), firmExits = sum(as.character(firmType) == "Ghost Firm", na.rm = T))

stargazer(firmTypeSummary, summary = F, covariate.labels = c("Sl. No.","Year", "No. of firms", "Firms in slump sectors", "Firms in surge sectors", "Firms Entering", "Firms Exiting"), digits = 1, title = "Summary of firm types", out = "Results/Tables/firmStats.html")


## Summarizing stats for incumbent firms
firmData <- ungroup(firmData) %>% filter(as.character(firmType) == "Incumbent")
incumFirmStats <- firmData %>% group_by(year) %>% summarise(nFirms = n(), nFirm50 = sum(numberofemployees > 50, na.rm = T), meanEmp = round(mean(numberofemployees, na.rm = T), 1), sdEmp = paste("(",round(sd(numberofemployees, na.rm = T), 1),")", sep=""), meanChangeinEmp = round(mean(deltaEmp, na.rm = T), 1), sdChangeEmp = paste("(",round(sd(deltaEmp, na.rm = T), 1), ")", sep=""), meanAge = round(mean(age, na.rm = T), 1), sdAge = paste("(",round(sd(age, na.rm = T), 1), ")", sep=""), meanLogKL = round(mean(logKperLmean, na.rm = T), 1), sdLogKL = paste("(",round(sd(logKperLmean, na.rm = T), 1), ")", sep=""), meanChangeinRev = round(mean(diffRevenue, na.rm = T)/1000, 1), sdChangeinRev = paste("(", round(sd(diffRevenue, na.rm = T)/1000, 1), ")", sep=""), meanChangeinProf = round(mean(diffProfit, na.rm = T)/1000, 1), sdChangeinProf = paste("(", round(sd(diffProfit, na.rm = T)/1000, 1), ")", sep=""))

stargazer(incumFirmStats, summary = F, covariate.labels = c("Sl. No.","Year", "No. of firms", "Large Firms (>50 workers)", "Mean no. of workers", "SD no. of workers","Mean change in empl.", "SD change in empl.", "Mean age of firms", "SD age of firms","Mean log(K/l)", "SD log(K/l)", "Mean change in rev.", "SD change in rev.","Mean change in profit", "SD change in profit"), digits = 1, title = "Incumbent Firm Characteristics",out = "Results/Tables/incumFirmSummaryStats.html")
