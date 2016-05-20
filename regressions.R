                                                #!!!!!!!!!!!!!ALERT LIST!!!!!!!!!!!!!!#                                                                                                                    The list of all the changes that are being made in this file that need to be either adequately explained or changed before proceeding
                                                #!!!!!!!!!!!!!ALERT LIST!!!!!!!!!!!!!!#

rm(list = ls())
setwd("~/Documents/Jobs Paper")
library(dplyr); library(lfe); library(ggplot2); library(stargazer); library(RColorBrewer)
source("~/Documents/R Helper Files/fte_theme.R")


load("Data/Saved Datasets/firmDataCleaned.RDA")


##Dropping obs with missing controls. Selecting the years. Keeping only incumbents.
firmData <- ungroup(firmData) %>% filter(year != 2011, year > 2003, !is.na(jobFlow), !is.na(region), !is.na(isic), !is.na(grTrend), !is.na(laborreg2), !is.na(numberofemployees), !is.na(age), !is.na(logKperLmean), !is.na(diffRevenue), !is.na(diffProfit)) %>% mutate(regYrIsicID = paste(as.character(region), as.character(year), as.character(isic), sep=''), secYrId = paste(as.character(isic), as.character(year), sep = '')) %>% filter(as.character(firmType) == "Incumbent")


                                                ############ Regression: 1 ###########                                                                                  BASIC SECIFICATION: Year FE for models 1 and 2 and sector*year FE for 3, 4, 5                                                                         ######################################

# Reg 1
model1 <- felm(jobFlow ~ grTrend | factor(year) | 0 |regYrIsicID, data = firmData)

# Reg 2
model2 <- felm(jobFlow ~ grTrend + grTrend*(laborreg2) | factor(year) | 0 | regYrIsicID, data = firmData)

# Reg 3
model3 <- felm(jobFlow ~  grTrend + grTrend*(laborreg2) | factor(secYrId) | 0 | regYrIsicID, data = firmData)

# Reg 4
model4 <- felm(jobFlow ~ grTrend + grTrend*(laborreg2) + grTrend*(numberofemployees)| factor(secYrId)| 0 | regYrIsicID, data = firmData)

#Reg 5
model5 <- felm(jobFlow ~ grTrend + grTrend*(laborreg2) + grTrend*(numberofemployees) + grTrend*age + grTrend*logKperLmean| factor(secYrId)| 0 | regYrIsicID, data = firmData)

stargazer(model1, model2, model3, model4, model5, add.lines = list(c("Fixed effects", "Year", "Year", "Sector * Year", "Sector * Year", "Sector * Year"), c("Error Clustering", "Region*Yr*Sector", "Region*Yr*Sector", "Region*Yr*Sector", "Region*Yr*Sector")), title = "Basic Specification", out = "Results/Tables/Basic Specification.html")

                                                ############ Regression: 2 ###########                                                                                                            Firm FE + Year FE                                                                                                           ######################################
# Reg 1
model1 <- felm(jobFlow ~ grTrend | factor(year) + factor(number_id) | 0 |regYrIsicID, data = firmData)
#
# # Reg 2
model2 <- felm(jobFlow ~ grTrend + grTrend*(laborreg2) | factor(year) + factor(number_id) | 0 | regYrIsicID, data = firmData)
#
# # Reg 3
model3 <- felm(jobFlow ~  grTrend + grTrend*(laborreg2) | factor(year) + factor(number_id)| 0 | regYrIsicID, data = firmData)
#
# # Reg 4
model4 <- felm(jobFlow ~ grTrend + grTrend*(laborreg2) + grTrend*(numberofemployees)| factor(year) + factor(number_id)| 0 | regYrIsicID, data = firmData)
#
# #Reg 5
model5 <- felm(jobFlow ~ grTrend + grTrend*(laborreg2) + grTrend*(numberofemployees) + grTrend*age + grTrend*logKperLmean| factor(year) + factor(number_id)| 0 | regYrIsicID, data = firmData)

stargazer(model1, model2, model3, model4, model5, add.lines = list(c("Fixed effects", "Firm + Year", "Firm + Year", "Firm + Year", "Firm + Year", "Firm + Year"), c("Error Clustering", "Region*Yr*Sector", "Region*Yr*Sector", "Region*Yr*Sector", "Region*Yr*Sector")), title = "Firm + Year Fixed Effects", out = "Results/Tables/regression2.html")

                                                ############ Regression: 3 ###########                                                                                            With delta revenue and profit as dependent vars                                                                                             ######################################
# Reg 1
model1 <- felm(diffRevenue ~ grTrend | factor(year) | 0 |regYrIsicID, data = firmData)
#
# Reg 2
model2 <- felm(diffRevenue ~  grTrend + grTrend*(laborreg2) | factor(secYrId) | 0 | regYrIsicID, data = firmData)

# Reg 3
model3 <- felm(diffRevenue ~ grTrend + grTrend*(laborreg2) + grTrend*(numberofemployees)| factor(secYrId)| 0 | regYrIsicID, data = firmData)
#
# Reg 4
model4 <- felm(diffProfit ~ grTrend | factor(year) | 0 |regYrIsicID, data = firmData)

# Reg 5
model5 <- felm(diffProfit ~  grTrend + grTrend*(laborreg2) | factor(secYrId) | 0 | regYrIsicID, data = firmData)

# Reg 6
model6 <- felm(diffProfit ~ grTrend + grTrend*(laborreg2) + grTrend*(numberofemployees)| factor(secYrId)| 0 | regYrIsicID, data = firmData)

stargazer(model1, model2, model3, model4, model5,model6, add.lines = list(c("Fixed effects", "Year", "Year", "Sector * Year", "Sector * Year", "Sector * Year", "Sector * Year"), c("Error Clustering", "Region*Yr*Sector", "Region*Yr*Sector", "Region*Yr*Sector", "Region*Yr*Sector", "Region*Yr*Sector", "Region*Yr*Sector")), title = "Profit and Revenue", out = "Results/Tables/regression3.html")

                                                ############ Regression: 4 ###########                                                                                               With exit dummy as the dependent variable                                                                                                ######################################
rm(list = ls())
load("Data/Saved Datasets/firmDataCleaned.RDA")

##FIltering out obs with missing controls and creating clustering vars
firmData <- ungroup(firmData) %>% filter(year != 2011, year > 2003, !is.na(jobFlow), !is.na(region), !is.na(isic), !is.na(grTrend), !is.na(laborreg2), !is.na(numberofemployees), !is.na(age), !is.na(logKperLmean)) %>% mutate(regYrIsicID = paste(as.character(region), as.character(year), as.character(isic), sep=''), secYrId = paste(as.character(isic), as.character(year), sep = ''), exitDummy = ifelse(as.character(firmType) == "Ghost Firm", 1, 0)) %>% filter(as.character(firmType) %in% c("Incumbent", "Ghost Firm"))

# Reg 1
model1 <- felm(exitDummy ~ grTrend | factor(year) | 0 |regYrIsicID, data = firmData)

# Reg 2
model2 <- felm(exitDummy ~ grTrend + grTrend*(laborreg2) | factor(year) | 0 | regYrIsicID, data = firmData)

# Reg 3
model3 <- felm(exitDummy ~  grTrend + grTrend*(laborreg2) | factor(secYrId) | 0 | regYrIsicID, data = firmData)

# Reg 4
model4 <- felm(exitDummy ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp)| factor(secYrId)| 0 | regYrIsicID, data = firmData)

#Reg 5
model5 <- felm(exitDummy ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp) + grTrend*age + grTrend*logKperLmean| factor(secYrId)| 0 | regYrIsicID, data = firmData)

stargazer(model1, model2, model3, model4, model5, add.lines = list(c("Fixed effects", "Year", "Year", "Sector * Year", "Sector * Year", "Sector * Year"), c("Error Clustering", "Region*Yr*Sector", "Region*Yr*Sector", "Region*Yr*Sector", "Region*Yr*Sector")), title = "Basic Specification with ExitDummy", out = "Results/Tables/exitDummy_Basic Specification.html")



                                                ############ SECTION: 2 ###########                                                                                                       TEMP                                                                                                                            ###################################

firmData <- mutate(firmData, large = ifelse(numberofemployees > 50, 1, 0))
# Reg 1
model1 <- felm(jobFlow ~ grTrend | factor(year) | 0 |regYrIsicID, data = firmData)

# Reg 2
model2 <- felm(jobFlow ~ grTrend + grTrend*(laborreg2) | factor(year) | 0 | regYrIsicID, data = firmData)

# Reg 3
model3 <- felm(jobFlow ~  grTrend + grTrend*(laborreg2) | factor(secYrId) | 0 | regYrIsicID, data = firmData)

# Reg 4
model4 <- felm(jobFlow ~ grTrend + grTrend*(laborreg2) + grTrend*(numberofemployees) + grTrend*(laborreg2)*large | factor(secYrId)| 0 | regYrIsicID, data = firmData)

#Reg 5
model5 <- felm(jobFlow ~ grTrend + grTrend*(laborreg2) + grTrend*(numberofemployees) + grTrend*(laborreg2)*large + grTrend*(laborreg2)*(numberofemployees) | factor(secYrId)| 0 | regYrIsicID, data = firmData)


stargazer(model1, model2, model3, model4, model5, add.lines = list(c("Fixed effects", "Year", "Year", "Sector * Year", "Sector * Year", "Sector * Year", "Sector * Year"), c("Error Clustering", "Region*Yr", "Region*Yr", "Region*Yr", "Region*Yr", "Region*Yr")), title = "Testing for Discontinuity", out = "Results/Tables/3TripleInteractionsNew.html")



                                                ############ SECTION: 2 ###########                                                                                                       REGRESSIONS D: Reallocation rate                                                                                                ###################################
rm(list = ls())
library(broom)
load("Data/Saved Datasets/reallocationData.RDA")

model = lm(R ~ factor(region) + factor(isic), data = reallocationData)
coefTable <- tidy(model) 
coefTable <- filter(coefTable, grepl("Intercept|region", term)) %>% mutate(term = gsub("factor\\(region\\)", "", term))
interceptValue = filter(coefTable, term == "(Intercept)")$estimate
plotData <- data.frame(region = unique(reallocationData$region))
plotData <- left_join(plotData, coefTable, by = c("region" = "term"))

plotData <- select(plotData, region, estimate) %>% mutate(estimate = ifelse(is.na(estimate), interceptValue, estimate + interceptValue))

load("Data/Saved Datasets/regional.RDA")

plotData <- left_join(plotData, regionData, by = "region")


png(file = "./Results/Plots/reallocationVsLaborReg.png", height = 800, width = 800, res = 100)
ggplot(plotData, aes(laborreg2, estimate)) + geom_point() + geom_smooth(method = 'lm') + fte_theme() + labs(y = "Estimated Labor Reallocation Rate in Different Regions", x = "Labor Regulation") + theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
dev.off()



