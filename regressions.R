                                                        ####################                                                                                                           This file runs all the regressions.
                                                        ####################
rm(list = ls())
setwd("~/Documents/Jobs Paper")
library(dplyr); library(lfe); library(ggplot2); library(stargazer); library(RColorBrewer)
source("~/Documents/R Helper Files/fte_theme.R")

load("Data/Saved Datasets/firmDataCleaned.RDA")


##Dropping obs with missing controls. Selecting the years. Keeping only incumbents.
firmData <- ungroup(firmData) %>% filter(year != 2011, year > 2003, !is.na(deltaEmp), !is.na(region), !is.na(isic), !is.na(grTrend), !is.na(laborreg2), !is.na(lagEmp), !is.na(age), !is.na(logKperLmean), !is.na(diffRevenue), !is.na(diffProfit)) %>% mutate(regYrIsicID = paste(as.character(region), as.character(year), as.character(isic), sep=''), secYrId = paste(as.character(isic), as.character(year), sep = ''))

## Winsorizing the data
lowBound <- quantile(firmData$deltaEmp, 0.01, na.rm = T)
upBound <- quantile(firmData$deltaEmp, 0.99, na.rm = T)
firmData <- ungroup(firmData) %>% mutate(deltaEmp = ifelse(is.na(deltaEmp), NA, ifelse(deltaEmp > upBound, upBound, ifelse(deltaEmp < lowBound, lowBound, deltaEmp))))

firmData <- ungroup(firmData) %>% filter(as.character(firmType) == "Incumbent")

                                                ############ Regression: 1 ###########                                                                                  BASIC SECIFICATION: Year FE for models 1 and 2 and sector*year FE for 3, 4, 5                                                                         ######################################
# Reg 1
model1 <- felm(deltaEmp ~ grTrend | factor(year) | 0 |regYrIsicID, data = firmData)

# Reg 2
model2 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg2) | factor(year) | 0 | regYrIsicID, data = firmData)

# Reg 3
model3 <- felm(deltaEmp ~  grTrend + grTrend*(laborreg2) | factor(secYrId) | 0 | regYrIsicID, data = firmData)

# Reg 4
model4 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp)| factor(secYrId)| 0 | regYrIsicID, data = firmData)

#Reg 5
model5 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp) + grTrend*age + grTrend*logKperLmean| factor(secYrId)| 0 | regYrIsicID, data = firmData)

stargazer(model1, model2, model3, model4, model5, add.lines = list(c("Fixed effects", "Year", "Year", "Sector * Year", "Sector * Year", "Sector * Year")), title = "Basic Specification", dep.var.labels = "Change in Employment", dep.var.caption = "", covariate.labels = c("Slump", "Surge", "Enforcement", "Lagged Employment", "Age","log(K/L)", "Slump*Enforcement", "Surge*Enforcement", "Slump*LagEmp", "Surge*LagEmp","Slump*Age", "Surge*Age", "Slump*log(K/L)","Surge*log(K/L)"), notes = "Errors clustered by Region*Year*Sector", type = "text", out = "Results/Tables/Basic Specification.txt")
                                                ############ Regression: 2 ###########                                                                                                            Firm FE + Year FE                                                                                                           ######################################
# Reg 1
model1 <- felm(deltaEmp ~ grTrend | factor(year) + factor(number_id) | 0 |regYrIsicID, data = firmData)
#
# # Reg 2
model2 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg2) | factor(year) + factor(number_id) | 0 | regYrIsicID, data = firmData)
#
# # Reg 3
model3 <- felm(deltaEmp ~  grTrend + grTrend*(laborreg2) | factor(year) + factor(number_id)| 0 | regYrIsicID, data = firmData)
#
# # Reg 4
model4 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp)| factor(year) + factor(number_id)| 0 | regYrIsicID, data = firmData)
#
# #Reg 5
model5 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp) + grTrend*age + grTrend*logKperLmean| factor(year) + factor(number_id)| 0 | regYrIsicID, data = firmData)

stargazer(model1, model2, model3, model4, model5, add.lines = list(c("Fixed effects", "Year", "Year", "Firm + Year", "Firm + Year", "Firm + Year")), title = "Basic Specification ()", dep.var.labels = "Change in Employment", dep.var.caption = "", covariate.labels = c("Slump", "Surge", "Enforcement", "Lagged Employment", "Age","log(K/L)", "Slump*Enforcement", "Surge*Enforcement", "Slump*LagEmp", "Surge*LagEmp","Slump*Age", "Surge*Age", "Slump*log(K/L)","Surge*log(K/L)"), notes = "Errors clustered by Region*Year*Sector",out = "Results/Tables/FirmYearFactors.html")

                                                ############ Regression: 3 ###########                                                                                            With delta revenue and profit as dependent vars                                                                                             ######################################
# Reg 1
model1 <- felm(diffRevenue ~ grTrend | factor(year) | 0 |regYrIsicID, data = firmData)
#
# Reg 2
model2 <- felm(diffRevenue ~  grTrend + grTrend*(laborreg2) | factor(secYrId) | 0 | regYrIsicID, data = firmData)

# Reg 3
model3 <- felm(diffRevenue ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp)| factor(secYrId)| 0 | regYrIsicID, data = firmData)
#
# Reg 4
model4 <- felm(diffProfit ~ grTrend | factor(year) | 0 |regYrIsicID, data = firmData)

# Reg 5
model5 <- felm(diffProfit ~  grTrend + grTrend*(laborreg2) | factor(secYrId) | 0 | regYrIsicID, data = firmData)

# Reg 6
model6 <- felm(diffProfit ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp)| factor(secYrId)| 0 | regYrIsicID, data = firmData)

stargazer(model1, model2, model3, model4, model5,model6, add.lines = list(c("Fixed effects", "Year", "Sector * Year", "Sector * Year", "Year", "Sector * Year", "Sector * Year")), digits = 1,title = "Profit and Revenue", dep.var.labels = c("Change in Revenue", "Change in Profit"),notes = "Errors clustered by Region*Year*Sector", dep.var.caption = "", covariate.labels = c("Slump", "Surge", "Enforcement", "Lagged Employment", "Slump*Enforcement", "Surge*Enforcement", "Slump*LagEmp", "Surge*LagEmp"), out = "Results/Tables/ProfitandRevenue.html")

                                                ############ REGRESSION: 4 ###########                                                                                     Basic Regression with only large firms: Testing for discontinuity                                                                                  ######################################

firmData <- mutate(firmData, large50 = ifelse(lagEmp > 50, 1, 0))

# Reg 4
model1 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp) + grTrend*(laborreg2)*large50 | factor(secYrId)| 0 | regYrIsicID, data = firmData)

#Reg 5
model2 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp) + grTrend*(laborreg2)*large50 + grTrend*(laborreg2)*(lagEmp) | factor(secYrId)| 0 | regYrIsicID, data = firmData)

model3 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp) + grTrend*(laborreg2)*large50 + grTrend*(laborreg2)*(lagEmp) +  grTrend*laborreg2*age + grTrend*laborreg2*logKperLmean| factor(secYrId)| 0 | regYrIsicID, data = firmData)

stargazer(model1, model2, model3, add.lines = list(c("Fixed effects", "Year", "Year", "Sector * Year", "Sector * Year")), title = "Testing for discontinuity for firms with more than 50 workers", dep.var.labels = "Change in Employment", dep.var.caption = "", covariate.labels = c("Slump", "Surge", "Enforcement", "Lagged Employment", "Large", "Age", "log(K/L)", "Slump*Enforcement", "Surge*Enforcement", "Slump*LagEmp", "Surge*LagEmp", "Slump*Large", "Surge*Large", "Enforcement*Large", "Enforcement*LagEmp", "Slump*Age", "Surge*Age", "Enforcement*Age", "Slump*log(K/L)", "Surge*log(K/L)", "Enforcement*log(K/L)", "Slump*Enforcement*Large", "Surge*Enforcement*Large", "Slump*Enforcement*LagEmp", "Surge*Enforcement*LagEmp", "Slump*Enforcement*Age", "Surge*Enforcement*Age", "Slump*Enforcement*log(K/l)", "Surge*Enforcement*log(K/L)"),notes = "Errors clustered by Region*Year*Sector", out = "Results/Tables/largeFirmsBasicReg.html")


                                                ############ Regression: 5 ###########                                                                                         BASIC SECIFICATION:With laborreg8 instead of laborreg2                                                                                         ######################################
# Reg 1
model1 <- felm(deltaEmp ~ grTrend | factor(year) | 0 |regYrIsicID, data = firmData)

# Reg 2
model2 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg8) | factor(year) | 0 | regYrIsicID, data = firmData)

# Reg 3
model3 <- felm(deltaEmp ~  grTrend + grTrend*(laborreg8) | factor(secYrId) | 0 | regYrIsicID, data = firmData)

# Reg 4
model4 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg8) + grTrend*(lagEmp)| factor(secYrId)| 0 | regYrIsicID, data = firmData)

#Reg 5
model5 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg8) + grTrend*(lagEmp) + grTrend*age + grTrend*logKperLmean| factor(secYrId)| 0 | regYrIsicID, data = firmData)

stargazer(model1, model2, model3, model4, model5, add.lines = list(c("Fixed effects", "Year", "Year", "Sector * Year", "Sector * Year", "Sector * Year")), title = "Basic Specification With Alternate Measure for Labor Enforcement", dep.var.labels = "Change in Employment", dep.var.caption = "", covariate.labels = c("Slump", "Surge", "Enforcement", "Lagged Employment", "Age","log(K/L)", "Slump*Enforcement", "Surge*Enforcement", "Slump*LagEmp", "Surge*LagEmp","Slump*Age", "Surge*Age", "Slump*log(K/L)","Surge*log(K/L)"), notes = "Errors clustered by Region*Year*Sector",out = "Results/Tables/Basic Specificationwith8.html")

                                                ############ Regression: 6 ###########                                                                                               With exit dummy as the dependent variable                                                                                                ######################################
rm(list = ls())
load("Data/Saved Datasets/firmDataCleaned.RDA")

##FIltering out obs with missing controls and creating clustering vars
firmData <- ungroup(firmData) %>% filter(year != 2011, year > 2003, !is.na(deltaEmp), !is.na(region), !is.na(isic), !is.na(grTrend), !is.na(laborreg2), !is.na(lagEmp), !is.na(age), !is.na(logKperLmean)) %>% mutate(regYrIsicID = paste(as.character(region), as.character(year), as.character(isic), sep=''), secYrId = paste(as.character(isic), as.character(year), sep = ''), exitDummy = ifelse(as.character(firmType) == "Ghost Firm", 1, 0)) %>% filter(as.character(firmType) %in% c("Incumbent", "Ghost Firm"))

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

stargazer(model1, model2, model3, model4, model5, add.lines = list(c("Fixed effects", "Year", "Year", "Sector * Year", "Sector * Year", "Sector * Year")), title = "Relationship between firm exits and enforcement", dep.var.labels = "Firm Exits", dep.var.caption = "", covariate.labels = c("Slump", "Surge", "Enforcement", "Lagged Employment", "Age","log(K/L)", "Slump*Enforcement", "Surge*Enforcement", "Slump*LagEmp", "Surge*LagEmp","Slump*Age", "Surge*Age", "Slump*log(K/L)","Surge*log(K/L)"), notes = "Errors clustered by Region*Year*Sector", out = "Results/Tables/exitRegressions.html")



                                                


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



