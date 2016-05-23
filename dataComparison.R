rm(list = ls())
setwd("~/Documents/Jobs Paper")
library(foreign); library(dplyr);

load("Data/Saved Datasets/firmDataCleaned.RDA")
load("Data/Saved Datasets/testData.RDA")

exitFirmsTest <- unique(filter(testData, !is.na(exit_year))$number_id)
exitFirmsR <- as.numeric(unique(filter(firmData, !is.na(exit_year))$number_id))
nonMatches <- exitFirmsTest[!(exitFirmsTest %in% exitFirmsR)]
##Recreate th firm data variables created in R from the Stata output
testData <- tbl_df(testData) %>% mutate(jobFlowTest = sum(incum_neg + incum_pos + job_entry - exit_job), grTrendTest = ifelse(surge == 1, "surge", ifelse(slump == 1, "slump", "normal")))
testData$grTrendTest <- as.factor(testData$grTrendTest)

testData <- ungroup(testData) %>% group_by(number_id) %>% mutate(exit_year = max(exit_year, na.rm = T))

firmData <- group_by(firmData, number_id) %>% mutate(opLead1 = lead(operatingrevenueturnover, 1), opLead2 = lead(operatingrevenueturnover, 2)) %>% ungroup() %>%  mutate(exit_year = ifelse((status_reason != 1 & year < 2011 & (opLead1 == 0 | is.na(opLead1)) & (opLead2 == 0 | is.na(opLead2))), year, NA)) %>% group_by(number_id) %>% mutate(exit_year = max(exit_year, na.rm = T)) %>% filter(year <= (exit_year) | is.na(exit_year)) %>% select(-opLead1, -opLead2) %>% ungroup()



firmData <- ungroup(firmData) %>% filter(year != 2011, year > 2003, !is.na(jobFlow), !is.na(region), !is.na(isic), !is.na(grTrend), !is.na(laborreg2), !is.na(numberofemployees), !is.na(age), !is.na(logKperLmean), !is.na(diffRevenue), !is.na(diffProfit))
