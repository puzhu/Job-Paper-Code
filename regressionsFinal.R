
rm(list = ls())
setwd("~/Documents/Past WB Projects/Jobs Paper")
library(dplyr); library(lfe); library(ggplot2); library(stargazer); library(RColorBrewer)

load("Data/Saved Datasets/firmDataCleaned.RDA")

                                                ############ Filtering and Summarizing data ###########                                                         Filter the data sequentially since some of the filters remove variables that we are interested in summarizing                                                                         ######################################


# ##Dropping only the years in the first step of data cleaning (additional steps drop all firmtypes other than incumbent, so we won't be able to summarise those variables)
# firmData <- ungroup(firmData) %>% filter(year != 2011, year > 2003)
# 
# ##Dropping obs with missing controls. Selecting the years. Keeping only incumbents.
# firmData <- ungroup(firmData) %>% filter(!is.na(deltaEmp), !is.na(region), !is.na(isic), !is.na(grTrend), !is.na(laborreg2), !is.na(lagEmp), !is.na(age), !is.na(logKperLmean), !is.na(diffRevenue), !is.na(diffProfit)) %>% mutate(regYrIsicID = paste(as.character(region), as.character(year), as.character(isic), sep=''), secYrId = paste(as.character(isic), as.character(year), sep = ''), regYrId = paste(as.character(region), as.character(year), sep = ''))
# 
# ## Winsorizing the data
# lowBound <- quantile(firmData$deltaEmp, 0.01, na.rm = T)
# upBound <- quantile(firmData$deltaEmp, 0.99, na.rm = T)
# firmData <- ungroup(firmData) %>% mutate(deltaEmp = ifelse(is.na(deltaEmp), NA, ifelse(deltaEmp > upBound, upBound, ifelse(deltaEmp < lowBound, lowBound, deltaEmp))))
# 
# ##Keeping only the incumbent firms
# firmData <- ungroup(firmData) %>% filter(as.character(firmType) == "Incumbent")
# 
# 

                                                ############ Regional Correlation ###########                                                                          This section creates a correlation table between regional characteristics and the enforcement variable                                                               #################################################

load("Data/Saved Datasets/regionalALL.RDA")
##Clean up the regional dataset with only variables we are interested in
regionAll <- tbl_df(regionAll) %>% group_by(region) %>% mutate(services = sum(contains("chapter_"), na.rm = T) - sum(chapter_a, chapter_b, chapter_c, chapter_d, chapter_e, na.rm = T))%>% ungroup() %>% select(laborreg2, unemp, firms, wagepc, city, grp, grppc, indoutput, indoutput_state, chapter_d, services)

##Creating the overall correlation correlations
correlationData <- as.data.frame(cor(regionAll$laborreg2, regionAll[,2:11], use="pairwise.complete.obs"))
correlationData <- tbl_df(correlationData) %>% gather(variable, correlation) %>% mutate(correlation = round(correlation, 2))
correlationData$variable <- c("Unemployment Rate", "No. of Firms", "Nominal Wage", "Urbanization Rate", "Gross Regional Product", "Gross Regional Product per Capita", "Total Industrial Output", "Industrial Output from State Enterpises", "Manufacturing Sector Output", "Services Sector Output")
stargazer(correlationData, summary = F, digits = 1, title = "Correlation between Enforcement and other regional characteristics", out = "Results/Tables/regionalCorrelTable.html")




                                                ############ Regression: 1 ###########                                                                                  First Table: 6 columns. deltaEmp | deltaProfit | deltaRev ~ grtrend (surge/slump)                                                                                     ######################################
## Reg 1: year FE
model1 <- felm(deltaEmp ~ grTrend | factor(year) | 0 | isic, data = firmData)

# Reg 2: firmID + year FE
model2 <- felm(deltaEmp ~ grTrend | factor(number_id) + factor(year) | 0 | isic, data = firmData)

## Reg 3: year FE
model3 <- felm(diffRevenue ~ grTrend | factor(year) | 0 | isic, data = firmData)

# Reg 4: firmID + year FE
model4 <- felm(diffRevenue ~ grTrend | factor(number_id) + factor(year) | 0 | isic, data = firmData)

## Reg 1: year FE
model5 <- felm(diffProfit ~ grTrend | factor(year) | 0 | isic, data = firmData)

# Reg 2: firmID + year FE
model6 <- felm(diffProfit ~ grTrend | factor(number_id) + factor(year) | 0 | isic, data = firmData)

stargazer(model1, model2, model3, model4, model5, model6, add.lines = list(c("Fixed effects", "Year", "Firm + Year", "Year", "Firm + Year", "Year", "Firm + Year")), digits = 1,title = "Effect of Surges and Slumps", dep.var.labels = c("Change in Employment","Change in Revenue", "Change in Profit"),notes = "Errors clustered by Sector", dep.var.caption = "", covariate.labels = c("Slump", "Surge"),out = "Results/Tables/Table1.html")

                                                ############ Regression: 2 ###########                                                                                  BASIC SPECIFICATION: 4 columns. First column firm + year FE, others Firm + Sector*yr FE                                                                                ######################################

# Reg 1
model1 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg2) | factor(number_id) + factor(year) | 0 | isic, data = firmData)

# Reg 2
model2 <- felm(deltaEmp ~  grTrend + grTrend*(laborreg2) | factor(number_id) + factor(secYrId) | 0 | isic, data = firmData)

# Reg 3
model3 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp)| factor(number_id) + factor(secYrId)| 0 | isic, data = firmData)

#Reg 4
model4 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp) + grTrend*age + grTrend*logKperLmean| factor(number_id) + factor(secYrId)| 0 | isic, data = firmData)

stargazer(model1, model2, model3, model4, add.lines = list(c("Fixed effects", "Firm + Year", "Firm + Sector * Year", "Firm + Sector * Year", "Firm + Sector * Year")), title = "Basic Specification",dep.var.labels = "Change in Employment", dep.var.caption = "", covariate.labels = c("Slump", "Surge", "Enforcement", "Lagged Employment", "Age","log(K/L)", "Slump*Enforcement", "Surge*Enforcement", "Slump*LagEmp", "Surge*LagEmp","Slump*Age", "Surge*Age", "Slump*log(K/L)","Surge*log(K/L)"), notes = "Errors clustered by Sector", out = "Results/Tables/Table2-BasicSpec.html")

                                                ############ Regression: 3 ###########                                                                                  Table 3: 4 columns. deltaRev | deltaProfit ~ 1st column table 2 and 4th column table2                                                                                 ######################################
# Reg 1
model1 <- felm(diffRevenue ~ grTrend + grTrend*(laborreg2) | factor(number_id) + factor(year) | 0 | isic, data = firmData)

#Reg 2
model2 <- felm(diffRevenue ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp) + grTrend*age + grTrend*logKperLmean| factor(number_id) + factor(secYrId)| 0 | isic, data = firmData)

# Reg 1
model3 <- felm(diffProfit ~ grTrend + grTrend*(laborreg2) | factor(number_id) + factor(year) | 0 | isic, data = firmData)

#Reg 2
model4 <- felm(diffProfit ~ grTrend + grTrend*(laborreg2) + grTrend*(lagEmp) + grTrend*age + grTrend*logKperLmean| factor(number_id) + factor(secYrId)| 0 | isic, data = firmData)

stargazer(model1, model2, model3, model4, add.lines = list(c("Fixed effects", "Firm + Year", "Firm + Sector * Year", "Firm + Year", "Firm + Sector * Year")), title = "Revenue and Profit", digits = 1, dep.var.caption = "", dep.var.labels = c("Change in Revenue", "Change in Profit"),covariate.labels = c("Slump", "Surge", "Enforcement", "Lagged Employment", "Age","log(K/L)", "Slump*Enforcement", "Surge*Enforcement", "Slump*LagEmp", "Surge*LagEmp","Slump*Age", "Surge*Age", "Slump*log(K/L)","Surge*log(K/L)"), notes = "Errors clustered by Sector", out = "Results/Tables/Table3-RevandProfit.html")


                                                ############ Regression: 4 ###########                                                                                         Repeat the basic specification with alternate measure of labor regulations                                                                                     ######################################

# Reg 1
model1 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg8) | factor(number_id) + factor(year) | 0 | isic, data = firmData)

# Reg 2
model2 <- felm(deltaEmp ~  grTrend + grTrend*(laborreg8) | factor(number_id) + factor(secYrId) | 0 | isic, data = firmData)

# Reg 3
model3 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg8) + grTrend*(lagEmp)| factor(number_id) + factor(secYrId)| 0 | isic, data = firmData)

#Reg 4
model4 <- felm(deltaEmp ~ grTrend + grTrend*(laborreg8) + grTrend*(lagEmp) + grTrend*age + grTrend*logKperLmean| factor(number_id) + factor(secYrId)| 0 | isic, data = firmData)

stargazer(model1, model2, model3, model4, add.lines = list(c("Fixed effects", "Firm + Year", "Firm + Sector * Year", "Firm + Sector * Year", "Firm + Sector * Year")), title = "Basic Specification With Alternate Measure",dep.var.labels = "Change in Employment", dep.var.caption = "", covariate.labels = c("Slump", "Surge", "Enforcement", "Lagged Employment", "Age","log(K/L)", "Slump*Enforcement", "Surge*Enforcement", "Slump*LagEmp", "Surge*LagEmp","Slump*Age", "Surge*Age", "Slump*log(K/L)","Surge*log(K/L)"), notes = "Errors clustered by Sector", out = "Results/Tables/Table4-BasicSpec-altMeasure.html")

                                                ############ Regression: 5 ###########                                                                                         Testing for discontinuity for firms that have 50 or more employees                                                                                           ######################################
firmData <- firmData %>%mutate(large50 = ifelse(lagEmp > 50, 1, 0))

# Reg 1
model1 <- felm(deltaEmp ~ grTrend*(laborreg2)*large50 | factor(number_id) + factor(secYrId)| 0 | isic, data = firmData)

#Reg 2
model2 <- felm(deltaEmp ~  grTrend*(laborreg2)*large50 + grTrend*(laborreg2)*(lagEmp) | factor(number_id) + factor(secYrId)| 0 | isic, data = firmData)

#Reg 3
model3 <- felm(deltaEmp ~ grTrend*(laborreg2)*large50 + grTrend*(laborreg2)*(lagEmp) +  grTrend*laborreg2*age + grTrend*laborreg2*logKperLmean| factor(number_id) + factor(secYrId)| 0 | isic, data = firmData)

stargazer(model1, model2, model3, add.lines = list(c("Fixed effects", "Firm + Sector * Year", "Firm + Sector * Year", "Firm + Sector * Year")), title = "Testing for discontinuity for firms with more than 50 workers", dep.var.labels = "Change in Employment", covariate.labels = c("Slump", "Surge", "Enforcement","Large", "Lagged Employment", "Age", "log(K/L)", "Slump*Enforcement", "Surge*Enforcement", "Slump*Large", "Surge*Large", "Enforcement*Large", "Slump*LagEmp", "Surge*LagEmp", "Enforcement*LagEmp", "Slump*Age", "Surge*Age", "Enforcement*Age", "Slump*log(K/L)", "Surge*log(K/L)", "Enforcement*log(K/L)", "Slump*Enforcement*Large", "Surge*Enforcement*Large", "Slump*Enforcement*LagEmp", "Surge*Enforcement*LagEmp", "Slump*Enforcement*Age", "Surge*Enforcement*Age", "Slump*Enforcement*log(K/l)", "Surge*Enforcement*log(K/L)"),dep.var.caption = "", notes = "Errors clustered by Sector", out = "Results/Tables/Table5-Largefirms.html")


