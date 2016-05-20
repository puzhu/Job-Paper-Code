rm(list = ls())
setwd("~/Documents/Jobs Paper")
library(foreign); library(dplyr);
                                                ############ SECTION: 1 ###########                                                                                                              LABOR REGUATIONS                                                                                                               ###################################
##Loading Labor Regs
regionData <- read.csv("Data/Regional Datasets/laborreg data.csv")

##Selecting variables and changing their names
regionData <- tbl_df(regionData) %>% select(regname, t2c1, t2c2, t2c4, t2c5, t2c7, t2c8, 2:7)
colnames(regionData) <- c("regname",paste('laborreg', 1:12, sep = ''))

##Comparing the names in the csv with those in the dataset
nameMatch <- read.csv("Data/Regional Datasets/laborreg region name match.csv")
regionData <- left_join(regionData, nameMatch, by = "regname")

##Dropping regions that are not present in ruslana and replacing regname with ruslana regions
regionData <- filter(regionData, !is.na(ruslana)) %>% select(ruslana, 2:13)
colnames(regionData)[1] <- "region"

                                                ############ SECTION: 2 ###########                                                                                                                   CAPTURE                                                                                                                   ###################################

captureData <- read.csv("Data/Regional Datasets/captureYeltzin.csv")

##Mathcing Names (One region name could not be matched)
nameMatch <- read.csv("Data/Regional Datasets/capture name match.csv")
captureData <- left_join(captureData, nameMatch, by = "region")

##Creating the dataset with correct names
captureData <- tbl_df(captureData) %>% na.omit() %>% select(ruslanaNames, concentrationOfPrivelege)
colnames(captureData) <-  c("region", "capture")

##Merge with region data
regionData <- left_join(regionData, captureData, by="region")
save(regionData, file = "Data/Saved Datasets/regional.RDA")

                                                ############ SECTION: 2 ###########                                                                                                           REGIONAL CHARACTERISTICS                                                                                                          ###################################
#Identify industrial data
industriesData <- read.dta("Data/Regional Datasets/ICPSR/industries.dta")
industriesData <- tbl_df(industriesData) %>% select(NAME, YEAR, GRP, GRPPC, FIRMS, FIXEDASSETS, DIPC, CHAPTER_A, CHAPTER_B, CHAPTER_C, CHAPTER_D, CHAPTER_E, CHAPTER_F, CHAPTER_G, CHAPTER_H, CHAPTER_I, CHAPTER_J, CHAPTER_K, CHAPTER_L, CHAPTER_M, CHAPTER_N, CHAPTER_O, INDOUTPUT, INDOUTPUT_STATE, INDOUTPUT_PRIV, INDOUTPUT_MIX, INDOUTPUT_FOR, INDOUTPUT_JOINT, DOMINANT_N, DOMINANT_OUT) ## GO TO PAGE 593 IN the codebook for variable descriptions ### Dominant_N: Number of enterprises with dominant market position, Share of dominating enterprises output in total industrial output, %, DIPC, Fixed asset investment per capita

##identify wage information and urbanization measures
population <- read.dta("Data/Regional Datasets/ICPSR/population.dta")
population <- tbl_df(population) %>% select(NAME, YEAR, CITY, MURD_M, MURD_W, MIGR, LABOR, UNEMP, STRIKEDAYS, INCOMEPC, WAGEPC, EXPENCEPC, LIVWAGE_SHARE)##UNEMP: % OF UNEMPL, LIVWAGE_SHARE, Share of population with income less than living wage, %

regionAll <- left_join(industriesData, population, by = c("NAME", "YEAR"))

##Matching Names
nameMatch <- read.csv("Data/Regional Datasets/regionALL name match.csv")
regionAll <- left_join(regionAll, nameMatch, by = c("NAME" = "charNames"))
regionAll <- select(regionAll, ruslanaNames, YEAR, GRP, GRPPC, FIRMS, FIXEDASSETS, DIPC, CHAPTER_A, CHAPTER_B, CHAPTER_C, CHAPTER_D, CHAPTER_E, CHAPTER_F, CHAPTER_G, CHAPTER_H, CHAPTER_I, CHAPTER_J, CHAPTER_K, CHAPTER_L, CHAPTER_M, CHAPTER_N, CHAPTER_O, INDOUTPUT, INDOUTPUT_STATE, INDOUTPUT_PRIV, INDOUTPUT_MIX, INDOUTPUT_FOR, INDOUTPUT_JOINT, DOMINANT_N, DOMINANT_OUT, CITY, MURD_M, MURD_W, MIGR, LABOR, UNEMP, STRIKEDAYS, INCOMEPC, WAGEPC, EXPENCEPC, LIVWAGE_SHARE)

##Cleaning Names
colnames(regionAll) <- tolower(colnames(regionAll))
colnames(regionAll)[1] <- "region"

##joining the labor regulation and capture data
regionAll <- left_join(regionAll, regionData, by="region")

save(regionAll, file = "Data/Saved Datasets/regionalALL.RDA")
