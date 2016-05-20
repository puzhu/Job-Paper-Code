rm(list = ls())
setwd("~/Documents/Jobs Paper")
library(foreign); library(dplyr); library(GGally); library(tidyr); library(stargazer)

load("Data/Saved Datasets/regionalALL.RDA")
                                                ############ SECTION: 1 ###########                                                                                                              OVERALL CORRELATIONS                                                                                                           ###################################

##Creating the overall correlation values
correlationData <- as.data.frame(cor(regionAll$laborreg2, regionAll[,3:54], use="pairwise.complete.obs"))
correlationData <- tbl_df(correlationData) %>% gather(variable, value) %>% mutate(value = round(value, 2))

                                                ############ SECTION: 1 ###########                                                                                                             CORRELATION PLOTS                                                                                                               ###################################

##Plotting GDP and similar characteristics
plotData <- select(regionAll, laborreg2, grp, grppc, firms, fixedassets)
colnames(plotData) <- c("Labor Regulation", "GRP", "GRPperCap", "N Firms", "Fixed Assets")

png(file = "./Results/Plots/Regional Correlations/incomeVsLaborReg.png", height = 800, width = 800, res = 100)
ggcorr(plotData, label = T, legend.position = 'bottom', label_alpha = 0.8)
dev.off()

##Plotting Industry Shares
plotData <- select(regionAll, laborreg2, contains("chapter"))
colnames(plotData) <- c("Labor Regulation", "Agri", "Fishery", "Mining", "Mfg", "Energy", "Building", "Trade", "Hotels", "Transport", "Finance", "Real Estate","Pub Admi", "Education", "Health", "Other")

png(file = "./Results/Plots/Regional Correlations/sectorShareVsLaborReg.png", height = 1000, width = 1000, res = 100)
ggcorr(plotData, label = T, label_alpha = 0.8, layout.exp = 1)
dev.off()

##Plotting industrial output and dominant firms
plotData <- select(regionAll, laborreg2, contains("indoutput"), dominant_n, dominant_out)
colnames(plotData) <- c("Labor Regulation", "Tot. Indus Output", "State Output", "Pvt Output", "Mix Output", "Foreign Output", "Joint Output", "N Dominant Firms", "Dominant Share")

png(file = "./Results/Plots/Regional Correlations/industryOutputVsLaborReg.png", height = 800, width = 800, res = 100)
ggcorr(plotData, label = T, label_alpha = 0.8, layout.exp = 1)
dev.off()

##Plotting geographical and wage characteristics
plotData <- select(regionAll, laborreg2, city, labor, unemp, strikedays, wagepc, livwage_share)
colnames(plotData) <- c("Labor Regulation", "Urban%", "Labor Force", "Unempl%", "Strike (Man-days)", "Wage", "Below Living Wage")

png(file = "./Results/Plots/Regional Correlations/geoCharWageVsLaborReg.png", height = 800, width = 800, res = 100)
ggcorr(plotData, label = T, label_alpha = 0.8, layout.exp = 1)
dev.off()

correlationData <- mutate(correlationData, value = round(value, 2))
stargazer(correlationData, summary = FALSE, out = "Results/Tables/Correlation.html")


testData <- data[,3:41]

