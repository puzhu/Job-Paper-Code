                                        #!!!!!!!!!!!!!!!Info!!!!!!!!!!!!!!#                                                                                        This file creates the summary tables and charts for the paper
                                        #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

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

summaryOfVars <- firmData %>% group_by(year) %>% summarise(nFirms = n(), inSlump = sum(as.character(grTrend) == "slump", na.rm = T), inSurge = sum(as.character(grTrend) == "surge", na.rm = T), nFirm50 = sum(numberofemployees > 50, na.rm = T), meanChangeinEmp = round(mean(deltaEmp, na.rm = T), 2), meanAge = round(mean(age, na.rm = T), 2), meanLogKL = round(mean(logKperLmean, na.rm = T), 2), meanChangeinRev = round(mean(diffRevenue, na.rm = T), 2), meanChangeinProf = round(mean(diffProfit, na.rm = T), 2))

stargazer(summaryOfVars, summary = F, covariate.labels = c("Sl. No.","Year", "No. of firms", "Firms in slump sectors", "Firms in surge sectors", "Large Firms (>50 workers)","Mean change in empl.", "Mean age", "Mean log(K/l)", "Mean change in rev.", "Mean change in profit"), digits = 1, out = "Results/Tables/SummaryStats.html")


                                                ############ Regional Characteristics ###########                                                                                     Replicate the steps used for the regression analysis                                                                                      #################################################
rm(list = ls())
setwd("~/Documents/Jobs Paper")
library(dplyr);library(tidyr); library(stargazer)

load("Data/Saved Datasets/regionalALL.RDA")

##Creating the overall correlation correlations
correlationData <- as.data.frame(cor(regionAll$laborreg2, regionAll[,3:54], use="pairwise.complete.obs"))
correlationData <- tbl_df(correlationData) %>% gather(variable, correlation) %>% mutate(correlation = round(correlation, 2)) %>% arrange(desc(abs(correlation)))

correlationData <- mutate(correlationData, correlation = round(correlation, 2))
stargazer(correlationData, summary = FALSE, out = "Results/Tables/Correlation.html")



# 
# ############ SECTION: 1 ###########                                                                                                             CORRELATION PLOTS                                                                                                               ###################################
# 
# ##Plotting GDP and similar characteristics
# plotData <- select(regionAll, laborreg2, grp, grppc, firms, fixedassets)
# colnames(plotData) <- c("Labor Regulation", "GRP", "GRPperCap", "N Firms", "Fixed Assets")
# 
# png(file = "./Results/Plots/Regional Correlations/incomeVsLaborReg.png", height = 800, width = 800, res = 100)
# ggcorr(plotData, label = T, legend.position = 'bottom', label_alpha = 0.8)
# dev.off()
# 
# ##Plotting Industry Shares
# plotData <- select(regionAll, laborreg2, contains("chapter"))
# colnames(plotData) <- c("Labor Regulation", "Agri", "Fishery", "Mining", "Mfg", "Energy", "Building", "Trade", "Hotels", "Transport", "Finance", "Real Estate","Pub Admi", "Education", "Health", "Other")
# 
# png(file = "./Results/Plots/Regional Correlations/sectorShareVsLaborReg.png", height = 1000, width = 1000, res = 100)
# ggcorr(plotData, label = T, label_alpha = 0.8, layout.exp = 1)
# dev.off()
# 
# ##Plotting industrial output and dominant firms
# plotData <- select(regionAll, laborreg2, contains("indoutput"), dominant_n, dominant_out)
# colnames(plotData) <- c("Labor Regulation", "Tot. Indus Output", "State Output", "Pvt Output", "Mix Output", "Foreign Output", "Joint Output", "N Dominant Firms", "Dominant Share")
# 
# png(file = "./Results/Plots/Regional Correlations/industryOutputVsLaborReg.png", height = 800, width = 800, res = 100)
# ggcorr(plotData, label = T, label_alpha = 0.8, layout.exp = 1)
# dev.off()
# 
# ##Plotting geographical and wage characteristics
# plotData <- select(regionAll, laborreg2, city, labor, unemp, strikedays, wagepc, livwage_share)
# colnames(plotData) <- c("Labor Regulation", "Urban%", "Labor Force", "Unempl%", "Strike (Man-days)", "Wage", "Below Living Wage")
# 
# png(file = "./Results/Plots/Regional Correlations/geoCharWageVsLaborReg.png", height = 800, width = 800, res = 100)
# ggcorr(plotData, label = T, label_alpha = 0.8, layout.exp = 1)
# dev.off()
# 
# 
# 
# 
# testData <- data[,3:41]
# 
