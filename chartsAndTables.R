                                        #!!!!!!!!!!!!!To DO!!!!!!!!!!!!!!#                                                                                                                    The list of all the changes that are being made in this file that need to be either adequately explained or changed before proceeding
                                        #!!!!!!!!!!!!!ALERT LIST!!!!!!!!!!!!!!#

rm(list = ls())
setwd("~/Documents/Jobs Paper")
library(dplyr); library(lfe); library(ggplot2); library(stargazer); library(RColorBrewer); library(ggthemes)

                                        ############ PreProcessing ###########                                                                                           Replicate the steps used for the regression analysis                                                                                           ######################################

load("Data/Saved Datasets/firmDataCleaned.RDA")


##Dropping obs with missing controls. Selecting the years. Keeping only incumbents.
firmData <- ungroup(firmData) %>% filter(year != 2011, year > 2003, !is.na(deltaEmp), !is.na(region), !is.na(isic), !is.na(grTrend), !is.na(laborreg2), !is.na(lagEmp), !is.na(age), !is.na(logKperLmean), !is.na(diffRevenue), !is.na(diffProfit)) %>% mutate(regYrIsicID = paste(as.character(region), as.character(year), as.character(isic), sep=''), secYrId = paste(as.character(isic), as.character(year), sep = ''), regYrId = paste(as.character(region), as.character(year), sep = ''))

## Winsorizing the data
lowBound <- quantile(firmData$deltaEmp, 0.01, na.rm = T)
upBound <- quantile(firmData$deltaEmp, 0.99, na.rm = T)
firmData <- ungroup(firmData) %>% mutate(deltaEmp = ifelse(is.na(deltaEmp), NA, ifelse(deltaEmp > upBound, upBound, ifelse(deltaEmp < lowBound, lowBound, deltaEmp))))


                                                ############ Summary Stats ###########                                                                                  Replicate all the summary tables from current version of the jobs paper                                                                                 ######################################

#summarybyType <- firmData %>% group_by(firmType, year) %>% summarise(nFirms = n(), totEmp = sum(numberofemployees, na.rm = T), totChange = sum(deltaEmp, na.rm = T))

firmData <- ungroup(firmData) %>% filter(as.character(firmType) == "Incumbent")

summaryOfVars <- firmData %>% group_by(year) %>% summarise(nFirms = n(), meanChangeinEmp = mean(deltaEmp, na.rm = T), inSlump = sum(as.character(grTrend) == "slump", na.rm = T), inSurge = sum(as.character(grTrend) == "surge", na.rm = T))
