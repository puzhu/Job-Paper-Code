                                                ############ SECTION: 1 ###########                                                             This file cleans the output dataset from the stata do file for the pupose of comparing with the output from the R dataset                                                         ###################################
rm(list = ls())
setwd("~/Documents/Jobs Paper")
library(foreign); library(dplyr);

testData <- read.dta("Data/firm_level_dataset_july24.dta")

testData <- select(testData, number_id, year, dateofincorporation, age, region, isic, laborreg2, surge, slump, exit_year, firstyr, lastyr, numberofemployees, status_reason, operatingrevenueturnover, incum, ghostfirm, entrymark, continuer, incum_pos, incum_neg, job_entry, exit_job, djob_co, djob_incumb) %>% arrange(number_id, year)

save(testData, file = "Data/Saved Datasets/testData.RDA")
