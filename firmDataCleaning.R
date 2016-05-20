                                                #!!!!!!!!!!!!!ALERT LIST!!!!!!!!!!!!!!#                                                                                                                    The list of all the changes that are being made in this file that need to be either adequately explained or changed before proceeding
                                                #!!!!!!!!!!!!!ALERT LIST!!!!!!!!!!!!!!#

# 4. A few specific firm ids are removed manually. This seems sketchy. It should ideally be filtered out using a criteria.asdfs


rm(list = ls())
setwd("~/Documents/Jobs Paper")
library(foreign); library(dplyr);

## LOADING THE STATA FILE
firmData <- read.dta("Data/global_detail_all.dta")

                                                ############ SECTION: 1 ###########                                                                                                    INITIAL CLEANING AND FILTERING DATA                                                                                                    ###################################
##Keeping only the variables we need for the analysis
firmData <- tbl_df(firmData) %>% select(number_id, year, dateofincorporation, age, region, isic, status_reason, size, operatingrevenueturnover, tangiblefixedassets, operatingprofitloss, numberofemployees) %>% group_by(number_id, year) %>% arrange(year) %>% ungroup()

##Cleaning the variables
firmData <- mutate(firmData, number_id = as.factor(number_id), year = as.integer(year), dateofincorporation = as.integer(dateofincorporation), region = as.factor(region), isic = as.integer(isic), size = as.factor(size))

## REMOVING SPECIFIC FIRMS (CHECK ALERTS)
firmData <- tbl_df(firmData) %>% filter(!number_id %in% c(998655, 12213, 58027, 10736, 23430, 148, 276, 310, 9278, 10737, 10920, 11001, 11163, 11164, 11184, 11195, 30452, 46016, 10852, 12034, 12204, 18291, 130910, 11914))

                                                ############ SECTION: 2 ###########                                                                                                              CREATE NEW VARIABLES                                                                                                         ###################################
##EXIT_YEAR VARIABLE. Exit variable that marks the year that a firm become inactive. An additional year is added to the end of every firm that exited in order to calculate job flows.
firmData <- group_by(firmData, number_id) %>% mutate(opLead1 = lead(operatingrevenueturnover, 1), opLead2 = lead(operatingrevenueturnover, 2)) %>% ungroup() %>%  mutate(exit_year = ifelse((status_reason != 1 & year < 2011 & (opLead1 == 0 | is.na(opLead1)) & (opLead2 == 0 | is.na(opLead2))), year, NA)) %>% group_by(number_id) %>% mutate(exit_year = max(exit_year, na.rm = T)) %>% filter(year <= (exit_year + 1) | is.na(exit_year)) %>% select(-opLead1, -opLead2) %>% ungroup()

# firmData <-  ungroup(firmData) %>%  mutate(exit_year = ifelse((status_reason != 1 & year < 2011), year, NA)) %>% group_by(number_id) %>% mutate(exit_year = max(exit_year, na.rm = T)) %>% filter(year <= exit_year | is.na(exit_year)) %>% ungroup()

##Creating a ghost observation for the year after exit. With NA values for all variables other than the list below.
temp <- group_by(firmData, number_id) %>% filter(year == exit_year) %>% mutate(year = year+1) %>% ungroup()
                                                
nonZeroVars <- c("number_id", "year", "dateofincorporation", "region", "isic", "numberofemployees", "exit_year", "age")

temp[,setdiff(colnames(temp), nonZeroVars)] = NA
temp$numberofemployees = 0

firmData <- bind_rows(firmData, temp) %>% arrange(number_id, year) %>% ungroup()
rm(list = c('temp', "nonZeroVars"))

##FIRM TYPE VARIABLE. Firms are cut into 4 mutually exclusive categories: 1) Entering firms: Firms that are incorporated that particular year. 2) First Year in Ruslana: Firms that entered the datset that particular year 3) Ghost Firms: Firms that exited the last year. 4) Incumbents: Firms that are in the dataset for more than a year and have not exited.
firmData <- group_by(firmData, number_id) %>% mutate(firmType = ifelse((year == min(year) & year == dateofincorporation), "Entering Firm", ifelse(year == min(year), "First Year in Ruslana", ifelse((year == exit_year + 1 & !is.na(exit_year)), "Ghost Firm", "Incumbent"))))

firmData$firmType <- as.factor(firmData$firmType)

## JOB FLOW VARIABLE. Note that firms types "First Year in Ruslana" will have a zero job flow.
firmData <- ungroup(firmData) %>% group_by(number_id) %>% mutate(lagEmp = lag(numberofemployees, 1), missingYear = ((year - lag(year, 1)) != 1 & as.character(firmType) == "Incumbent")) %>% ungroup() %>% mutate(jobFlow = ifelse(as.character(firmType) == "Entering Firm", numberofemployees, numberofemployees - lagEmp)) %>% mutate(jobFlow = ifelse(missingYear, NA, jobFlow))

## AVE. CAPITAL PER LABOUR (K/L) VARIABLE. This is the mean over the panel of the capital divided by the number of employees per year. Other option #log(tangiblefixedassets + 1) - log(numberofemployees + 1)
firmData <- ungroup(firmData) %>% mutate(KperL = tangiblefixedassets/numberofemployees) %>% group_by(number_id) %>% mutate(KperLmean = mean(KperL, na.rm = T)) %>% ungroup() %>% mutate(logKperLmean = ifelse(KperLmean < 1, log(1), log(KperLmean))) %>% select(-KperL, -KperLmean)

## CHANGE IN REVENUE AND PROFIT VARIABLES.
firmData <- ungroup(firmData) %>% group_by(number_id) %>% mutate(lagOperRevenue = lag(operatingrevenueturnover, 1), lagProfit = lag(operatingprofitloss, 1)) %>% ungroup() %>% mutate(diffRevenue = operatingrevenueturnover - lagOperRevenue, diffProfit = operatingprofitloss - lagProfit) %>% select(-lagOperRevenue, -lagProfit)


                                                ############ SECTION: 3 ###########                                                                                              ADDING THE REGULATION AND SPURT/SLUMP VARIABLES                                                                                                ###################################
##Joining the labor regulations and spurts
load("Data/Saved Datasets/regionalALL.RDA")
load("Data/Saved Datasets/isicData.RDA")
firmData <- left_join(firmData, regionAll, by = c("region", "year"))
firmData <- left_join(firmData, isicData, by = c("isic", "year"))

                                                ############ SECTION: 4 ###########                                                                                                   CREATE THE LABOR REALLOCATION DATASET                                                                                                   ###################################
reallocationData <- group_by(firmData, isic, region, year) %>% mutate(noobs = sum(!is.na(jobFlow))) %>% filter(noobs > 5) %>% summarise(Rind = sum(abs(jobFlow), na.rm = T)/sum(numberofemployees, na.rm = T)) %>% ungroup() %>% group_by(isic, region) %>% summarise(R = mean(Rind))

save(reallocationData, file = "Data/Saved Datasets/reallocationData.RDA")


                                                ############ SECTION: 5 ###########                                                                                                  GETTING THE DATA READY FOR REGRESSIONS                                                                                                   ###################################
##Replacing NA values with lagged values for exit regression using basic specification, (have to do this here to prevent ghost firms from being filtered out during the data filtering process.)

##Keeping only variables used for current analysis
firmData <- ungroup(firmData) %>% select(number_id, year, exit_year, age, region, isic, firmType, grTrend, numberofemployees, lagEmp,laborreg2, laborreg8, logKperLmean, diffRevenue, diffProfit, grp, grppc, firms, fixedassets, chapter_a, chapter_c, chapter_d, chapter_l, chapter_m, unemp, wagepc, capture, jobFlow)


## Winsorizing the data
lowBound <- quantile(firmData$jobFlow, 0.01, na.rm = T)
upBound <- quantile(firmData$jobFlow, 0.99, na.rm = T)
firmData <- ungroup(firmData) %>% mutate(jobFlow = ifelse(is.na(jobFlow), NA, ifelse(jobFlow > upBound, upBound, ifelse(jobFlow < lowBound, lowBound, jobFlow))))

# lowKperLmean <- quantile(firmData$KperLmean, 0.01, na.rm = T)
# upKperLmean <- quantile(firmData$KperLmean, 0.99, na.rm = T)
# firmData <- ungroup(firmData) %>% mutate(KperLmean = ifelse(is.na(KperLmean), NA, ifelse(KperLmean > upKperLmean, upKperLmean, ifelse(KperLmean < lowKperLmean, lowKperLmean, KperLmean))))

## Saving the dataset
save(firmData, file = "Data/Saved Datasets/firmDataCleaned.RDA")


                                                        



## OLD CODE CHUNKS COMMENTED OUT AND KEPT FOR REFERENCE

# testData <- read.dta("Data/firm_level_dataset_july24.dta")
# testData <- tbl_df(testData) %>% filter(number_id %in% sample(1:100000, 1000))
# save(testData, file = "Data/testDataLarge.RDA")
## Loading the data from Siddharth(Currently loading a truncated version with 1000 randomly selected firm)

# firmData <- tbl_df(firmData) %>% filter(number_id %in% sample(1:100000, 10000))
# save(firmData, file = "Data/testRandSample1000.RDA")
#load("Data/testRandSample1000.RDA")
#load("Data/testData.RDA")



                                                        #!!!!!!!!!!!!!ALERT!!!!!!!!!!!!!!#                                                                      This is an unnecessary specification since no firms in the data have more than two consecutive years of reported zero or na operating revenue. The current specification can be replaced with last year in panel without any loss (check).
                                                        #!!!!!!!!!!!!!ALERT!!!!!!!!!!!!!!#




# ##VALUE ADD VARIABLE. Takes the log of the lagged valueaddedperworker variable. Note that lagged values that are lower than 1 are set as 1.
#firmData <- ungroup(firmData) %>% group_by(number_id) %>% mutate(lp = lag(valueaddperworker, 1), loglp = ifelse((lp < 1 & !is.na(lp)), log(1), log(lp)))

# ##LAGGED DEVIATION OF VALUE ADD FROM INDUSTRY MEAN. This creates two variables, the dfference in the actual value from the industry year mean and the difference between the logged value and the industry mean of the logged value. The lagged values of these two variables are recorded (DOES NOT MATCH)
#!!!!!!!!!!!!!ALERT!!!!!!!!!!!!!!#                                                                      Grouping isic year return NA for 2004 despite there being values for that year in the dataset. The NAs disappear when grouping year, isic. Seems to be a bug in dplyr. The test data and the variable in this script don't match currently.
#!!!!!!!!!!!!!ALERT!!!!!!!!!!!!!!#
# firmData <- ungroup(firmData) %>% mutate(lm = ifelse((valueaddperworker < 1 & !is.na(valueaddperworker)), log(1), log(valueaddperworker))) %>% group_by(year, isic) %>% mutate(meanlm = mean(lm, na.rm = T), meanm = mean(valueaddperworker, na.rm = T)) %>% ungroup() %>% mutate(dlm = lm - meanlm, dm = valueaddperworker - meanm) %>% group_by(number_id) %>% mutate(dev_lp = lag(dm, 1), dev_loglp = lag(dlm, 1)) %>% select(-dlm, -lm, -dm, -meanlm, -meanm)
# 
# # ##LOG OF LAGGED CURRENT ASSETS, LOG OF LAGGED CURRENT LIABILITIES, LOG OF AGE, SQUARE OF LAGGED EMPLOYMENT
# firmData <- ungroup(firmData) %>% group_by(number_id) %>% mutate(empl = lag(numberofemployees, 1), k = lag(tangiblefixedassets, 1), logk = log(k + 1), asset = lag(currentassets, 1), logasset = log(asset + 1), cliability = lag(currentliabilities, 1)) %>% ungroup() %>% mutate(ncliability = cliability - asset, emplsq = empl^2, logage = log(age))
# 
# # ## LAGGED DEVIATION FROM INDUSTRY MEAN OF K PER WORKER (dev_logkl DOES NOT MATCH!!)
# firmData <- mutate(firmData, lm = log(tangiblefixedassets + 1) - log(numberofemployees + 1)) %>% group_by(year, isic) %>% mutate(meanlm = mean(lm, na.rm = T)) %>% ungroup() %>% mutate(dlm = lm - meanlm) %>% group_by(number_id) %>% mutate(dev_logkl = lag(dlm, 1)) %>% select(-lm, -dlm)
