# Set up
#-------------------------------------------------------------------------
# Load libraries
library(tidyverse)
library(fixest)
library(modelsummary)
library(randomcoloR)
library(scales)
library(PerformanceAnalytics)
#library(countries)
#-------------------------------------------------------------------------
# Set graphic parameters
axisTitleSize <- 8
axisTextSize <- 7
legendKeySize <- 0.3
legendTextSize <- axisTextSize
facetTitleSize <- axisTextSize
plotTitleSize = axisTextSize
labelSize <- 2
#-------------------------------------------------------------------------
# Define folders
# Working directory:
wrkDir <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/DALYs/"
# All the data required should be stored here:
dataFolder <- paste0(wrkDir, "Data/")
# Outputs--graphics and summary stats--should be written here:
outFolder <- paste0(wrkDir, "Graphics and sumStats/")
#=========================================================================
# Import IHME GBD DALY raw data files
# Source: https://vizhub.healthdata.org/gbd-results/
# GBD Methdological Appendix search engine (very useful to see how key hunger components are defined.):
# https://www.healthdata.org/gbd/methods-appendices-2021
thisFilename <- "IHME-GBD_2010-2021_cNutDef_cty.csv"
thisFilepath <- paste0(dataFolder, thisFilename)
dfCauseRaw <- read.csv(thisFilepath, stringsAsFactors = F)
thisFilename <- "IHME-GBD_2010-2021_rNutDef_ctyB.csv"
thisFilepath <- paste0(dataFolder, thisFilename)
dfRiskRaw <- read.csv(thisFilepath, stringsAsFactors = F)
#-------------------------------------------------------------------------
# Harmonize cty names in the risk factor and cause data frames.
setdiff(unique(dfCauseRaw$location), unique(dfRiskRaw$location))
# The cause data has "Turkey" while the risk data has "Türkiye"
# Harmonize the two and get rid of the umlaut to avoid UTF-8 problems
dfRiskRaw$location[grep("rkiye", dfRiskRaw$location)] <- "Turkiye"
dfCauseRaw$location[grep("Turkey", dfCauseRaw$location)] <- "Turkiye"
# The name "Taiwan (Province of China)" suggests it is included in "China"
# Therefore, drop Taiwan to avoid double counting.
dfRiskRaw <- subset(dfRiskRaw, location != "Taiwan (Province of China)")
dfCauseRaw <- subset(dfCauseRaw, location != "Taiwan (Province of China)")
#-------------------------------------------------------------------------
# Create separate data frames for chronic, hidden, and overnutrition DALYs 
# Chronic hunger DALYs
# Alternate definition
# dfChr <- dfCauseRaw %>%
#   subset(cause == "Protein-energy malnutrition" &
#            age == "All ages" &
#            metric == "Rate" &
#            measure == "DALYs (Disability-Adjusted Life Years)") %>%
#   rename(area = location) %>%
#   mutate(Cat = "Chronic hunger") %>%
#   select(area, year, val, Cat)
# Original definition
dfChr <- dfRiskRaw %>%
  subset(rei == "Child underweight" &
           age == "All ages" &
           metric == "Rate" &
           measure == "DALYs (Disability-Adjusted Life Years)") %>%
  rename(area = location) %>%
  mutate(Cat = "Chronic hunger") %>%
  # select(area, year, val, Cat)
  select(area, year, lower, Cat) %>%
  rename(val = lower)
# Hidden hunger DALYs
hidHcauses <- c("Iodine deficiency",
                "Dietary iron deficiency",
                "Vitamin A deficiency",
                "Other nutritional deficiencies")
dfHid <- dfCauseRaw %>%
  subset(cause %in% hidHcauses &
           age == "All ages" &
           metric == "Rate" &
           measure == "DALYs (Disability-Adjusted Life Years)") %>%
  rename(area = location) %>%
  group_by(area, year) %>%
  summarize(val = sum(val)) %>%
  mutate(Cat = "Hidden hunger") %>%
  select(area, year, val, Cat)
# Overnutrition DALYs
overNutRiskFactors <- c("Diet high in sugar-sweetened beverages",
                        "Diet high in processed meat",
                        #"Diet low in fiber",
                        # "Diet low in legumes",
                        # "Diet low in fruits",
                        # "Diet low in vegetables",
                        #"Diet low in whole grains",
                        #"Diet low in nuts and seeds",
                        "Diet low in seafood omega-3 fatty acids",
                        # "Diet Low in Omega-6 Polyunsaturated Fatty Acids",
                        "Diet low in polyunsaturated fatty acids",
                        "Diet high in trans fatty acids",
                        "Diet high in sodium")
dfOve <- dfRiskRaw %>%
  subset(rei %in% overNutRiskFactors &
           age == "All ages" &
           metric == "Rate" &
           measure == "DALYs (Disability-Adjusted Life Years)") %>%
  rename(area = location) %>%
  group_by(area, year) %>%
  summarize(val = sum(val)) %>%
  mutate(Cat = "Overnutrition") %>%
  select(area, year, val, Cat)
#-------------------------------------------------------------------------
# Unite the hunger DALYs data frames in one data frame
listDf <- list(dfChr, dfHid, dfOve)
dfDaly <- do.call(rbind, listDf) %>% as.data.frame() %>%
  rename(`DALYs / 100,000 capita` = val)
#=========================================================================
# Get IHME's Socio-demographic index (SDI), merge with dfDaly
# Source: https://ghdx.healthdata.org/record/global-burden-disease-study-2021-gbd-2021-socio-demographic-index-sdi-1950%E2%80%932021
#thisFile <- "IHME_GBD_2019_SDI_1990_2019_Y2020M10D15.xlsx" # 2019 release
thisFile <- "IHME_GBD_SDI_2021_SDI_1950_2021_Y2024M05D16.csv" # 2021 release
thisFilePath <- paste0(dataFolder, thisFile)
dfRaw <- read.csv(thisFilePath, stringsAsFactors = F)
dfRaw <- dfRaw %>% rename(area = location_name,
                          sdi = mean_value,
                          year = year_id) %>%
  select(area, year, sdi)
# For some reason, some region level areas are duplicated
dfRaw %>% group_by(year) %>%
  mutate(dup = duplicated(area)) %>%
  subset(dup == T) %>% .$area %>% unique()
# Remove these, except for Georgia. Georgia is not duplicate
# Georgia refers to the US state and the cty
# Deal with Georgia separately
dfRaw <- dfRaw %>% group_by(year) %>%
  mutate(dup = duplicated(area))
dfRaw$dup[which(dfRaw$area == "Georgia")] <- F
dfRaw <- dfRaw %>% subset(dup == F) %>% select(-dup)
# Dealing with Georgia
# In the previous 2019 release (which was an .xlsx not .csv file),
# The two Georgias were clearly differentiated by region headings.
# In that file, the 2019 values for Georgia the cty and Georgia the US state
# were 0.702 and 0.841, respectively.
# By this we can determine which Georgia is the cty in the 2021 SDI data.
# (I.e., it's the one with the lower SDI in recent years)
dfGeorgia <- dfRaw %>% subset(area == "Georgia") %>%
  mutate(dup = as.integer(duplicated(year))) %>%
  mutate(area = paste(area, dup)) %>% select(-dup) %>%
  spread(area, sdi) 
# Visual inspection of dfGeorgia reveals that "Georgia 0" has the lower SDI
# in recent years and so this must refer to the cty.
dfGeorgia <- dfGeorgia %>% select(-`Georgia 1`) %>%
  rename(sdi = `Georgia 0`) %>% mutate(area = "Georgia") %>%
  select(area, year, sdi)
dfSDI <- dfRaw %>% subset(area != "Georgia") %>%
  rbind(dfGeorgia) %>% as.data.frame()
rm(dfRaw, dfGeorgia)
# Unlike the 2019 SDI data, 2021 cty names are mostly harmonized with the names
# in dfDaly. Just harmonize Turkiye.
setdiff(unique(dfDaly$area), unique(dfSDI$area))
u <- dfSDI$area
unique(u[grep("rkiye", u)])
dfSDI$area[grep("rkiye", u)] <- "Turkiye"
dfSDI$sdi <- 100 * dfSDI$sdi
dfDaly <- dfDaly %>% merge(dfSDI)
#========================================================================
# Get population under 25 data from UN Population Data Portal, merge with dfDaly
# Source: https://population.un.org/wpp/Download/Standard/Population/
thisFile <- "WPP2024_POP_F06_1_POPULATION_PERCENTAGE_SELECT_AGE_GROUPS_BOTH_SEXES.xlsx"
thisFilepath <- paste0(dataFolder, thisFile)
dfRaw <- readxl::read_xlsx(thisFilepath)
dfRaw <- dfRaw[-c(1:11), ]
colnames(dfRaw) <- dfRaw[1, ]
dfPopYoung <- dfRaw[-1, ] %>% 
  rename(area = `Region, subregion, country or area *`,
         year = Year,
         `Pct Pop < 15` = `0-14`,
         `Pct Pop < 25` = `0-24`) %>%
  select(area, year, `Pct Pop < 15`, `Pct Pop < 25`) %>%
  subset(year >= 2010)
rm(dfRaw)
dfPopYoung$`Pct Pop < 15` <- as.numeric(dfPopYoung$`Pct Pop < 15`)
dfPopYoung$`Pct Pop < 25` <- as.numeric(dfPopYoung$`Pct Pop < 25`)
# Check for duplicates
dfPopYoung %>% group_by(year) %>% mutate(dup = duplicated(area)) %>%
  subset(dup == T) %>% .$area %>% unique()
# Harmonize cty names
u <- dfPopYoung$area
setdiff(unique(dfDaly$area), unique(u))
dfPopYoung$area[grep("rkiye", u)] <- "Turkiye"
dfPopYoung$area[grep("Dem. People's Republic of Korea", u)] <- "Democratic People's Republic of Korea"
dfPopYoung$area[grep("Micronesia \\(Fed. States of\\)", u)] <- "Micronesia (Federated States of)"
# Make sure there are no NA (0 if none)
sum(is.na(dfPopYoung$`Pct Pop < 15`))
sum(is.na(dfPopYoung$`Pct Pop < 25`))
# Merge with dfDaly
dfDaly <- dfDaly %>% merge(dfPopYoung)
#========================================================================
# Get GDP / capita data
# Source: FAOSTAT
thisFile <- "Macro-Statistics_Key_Indicators_E_All_Data.csv"
thisFilePath <- paste0(dataFolder, thisFile)
dfRaw <- read.csv(thisFilePath, stringsAsFactors = F)
theseElems <- "Value US$ per capita, 2015 prices"
dfRaw <- dfRaw %>% select(-c(contains("Code"), ends_with("F"), ends_with("N"))) %>% subset(Element %in% theseElems)
yrCols <- colnames(dfRaw)[grep("20|19", colnames(dfRaw))]
dfRaw <- dfRaw %>% gather_("year", "value", yrCols)
dfRaw$year <- gsub("Y", "", dfRaw$year)
colnames(dfRaw) <- tolower(colnames(dfRaw))
dfGDP <- dfRaw %>% select(area, year, value) %>%
  rename(`GDP / capita` = value) %>%
  subset(year >= 2010)
rm(dfRaw)
# Harmonize cty names with those in dfDaly
u <- dfGDP$area
setdiff(unique(dfDaly$area), unique(u))
dfGDP$area[grep("United Kingdom", dfGDP$area)] <- "United Kingdom"
dfGDP$area[grep("Netherlands \\(Kingdom", dfGDP$area)] <- "Netherlands"
dfGDP$area[grep("rkiye", dfGDP$area)] <- "Turkiye"
dfGDP$area[grep("Ivoire", dfGDP$area)] <- "Côte d'Ivoire"
notThese <- c("China, Hong Kong SAR", "China, Macao SAR",
              "China, Taiwan Province of", "China, mainland")
dfGDP <- subset(dfGDP, !(area %in% notThese))
# Check for NA, NaN, infinite values
apply(dfGDP, 2, function(x) sum(is.na(x))) %>% table()
apply(dfGDP, 2, function(x) sum(is.nan(x))) %>% table()
apply(dfGDP, 2, function(x) sum(is.infinite(x))) %>% table()
# There are a few NAs for ctys that will be dropped in merge, not important
dfGDP$area[which(is.na(dfGDP$`GDP / capita`))] %>% unique()
#View(dfGDP[which(is.na(dfGDP$`GDP / capita`)), ])
# Merge with dfDaly
dfDaly <- dfDaly %>% merge(dfGDP)
#========================================================================
# Get FAO Food Balance Sheet kcal data
# Source: FAOSTAT
thisFile <- "FoodBalanceSheets_E_All_Data.csv"
thisFilePath <- paste0(dataFolder, thisFile)
dfRaw <- read.csv(thisFilePath, stringsAsFactors = F)
theseElems <- c("Food supply (kcal/capita/day)",
                "Protein supply quantity (g/capita/day)",
                "Fat supply quantity (g/capita/day)",
                "Total Population - Both sexes")
dfRaw <- dfRaw %>% select(-c(contains("Code"), ends_with("F"), ends_with("N"))) %>%
  subset(Element %in% theseElems)
yrCols <- colnames(dfRaw)[grep("20", colnames(dfRaw))]
dfRaw <- dfRaw %>% gather_("year", "value", yrCols)
dfRaw$year <- gsub("Y", "", dfRaw$year)
colnames(dfRaw) <- tolower(colnames(dfRaw))
dfFBSraw <- dfRaw; #rm(dfRaw)
# Harmonize cty names with dfDaly
dfFBSraw$area[grep("United Kingdom", dfFBSraw$area)] <- "United Kingdom"
dfFBSraw$area[grep("Netherlands \\(Kingdom", dfFBSraw$area)] <- "Netherlands"
dfFBSraw$area[grep("rkiye", dfFBSraw$area)] <- "Turkiye"
dfFBSraw$area[grep("Ivoire", dfFBSraw$area)] <- "Côte d'Ivoire"
notThese <- c("China, Hong Kong SAR", "China, Macao SAR",
              "China, Taiwan Province of", "China, mainland")
dfFBSraw <- subset(dfFBSraw, !(area %in% notThese))
# Check for duplicates
dfFBSraw %>% group_by(area, year, element, item) %>%
  mutate(dup = duplicated(item)) %>%
  subset(dup == T) %>% .$item %>% unique()
# There are duplicates for milk, eggs, and miscellaneous. Drop duplicates.
dfFBSraw <- dfFBSraw %>% group_by(area, year, element, item) %>%
  mutate(dup = duplicated(item)) %>%
  subset(dup == F) %>%
  select(-dup)
#-----------------------------------------------------------------------
# Select which commodities you want available for model specification
# u <- dfFBSraw$item
# unique(u[grep("Sugar", u)])
# Note "Fruit, other" and "Vegetables, other" both include
# 567 "Watermelons" and 568 "Melons, other (inc.cantaloupes)"
theseItems <- c("Cereals - Excluding Beer",
                "Starchy Roots",
                "Vegetables",
                "Vegetable Oils",
                #"Oilcrops", #consumed oil crop oils are included under vegetable oils
                "Fruits - Excluding Wine",
                "Pulses",
                "Animal Products",
                #"Animal fats",
                #"Meat",
                #"Milk - Excluding Butter",
                #"Sugar Crops",
                "Sugar & Sweeteners",
                #"Sugar (Raw Equivalent)",
                #"Alcoholic Beverages",
                "Grand Total")

dfFBS <- dfFBSraw %>% select(area, year, item, element, value) %>%
  subset(item %in% theseItems) %>%
  # Merge fruits and vegetables
  mutate(item = gsub("Fruits - Excluding Wine", "F&V", item)) %>%
  mutate(item = gsub("Vegetables", "F&V", item)) %>%
  mutate(item = gsub("Cereals - Excluding Beer", "Cereals", item)) %>%
  group_by(area, year, item, element) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  spread(item, value) %>% as.data.frame()
#merge(dfPop, by = c("area", "year")) %>% as.data.frame()
# Check for NAs, NaNs, and infinite values
apply(dfFBS[, -c(1:3)], 2, function(x) sum(is.na(x))) %>% table()
apply(dfFBS[, -c(1:3)], 2, function(x) sum(is.nan(x))) %>% table()
apply(dfFBS[, -c(1:3)], 2, function(x) sum(is.infinite(x))) %>% table()
# Convert to diet share terms by dividing by grand total
colSkip <- which(colnames(dfFBS) %in% c("area", "year", "element"))
dfFBS[, -colSkip] <- dfFBS[, -colSkip] / dfFBS$`Grand Total` * 100
# Calculate residual kcal category
colSkip <- which(colnames(dfFBS) %in% c("area", "year", "element", "Grand Total"))
dfFBS$Residual <- dfFBS$`Grand Total` - rowSums(dfFBS[, -colSkip])
# Make sure dfDaly and dfFBS ctys match
# The FAO data is missing about 12 countries that are included in the IHME DALY data.
# These will be lost in the merge. Not too important since they are all very small
# population countries.
setdiff(dfDaly$area, dfFBS$area)
#setdiff(dfFBS$area, dfDaly$area)
# unique(dfFBS$area[grep("Greenland", dfFBS$area)])
# unique(dfDaly$area[grep("Greenland", dfDaly$area)])
# Separate into FBS commodity and FBS macnut dfs
dfFBScom <- dfFBS %>% subset(element == "Food supply (kcal/capita/day)") %>% select(-c(element, `Grand Total`))
#=======================================================================
# Model estimation
dfMod <- dfDaly %>% merge(dfFBScom)
#-----------------------------------------------------------------------
# Final tidying up
# Check for NAs, NaNs, infinite values, and zero values
#colnames(dfMod)
notCols <- which(colnames(dfMod) %in% c("area", "year", "Cat"))
apply(dfMod[, -notCols], 2, function(x) sum(is.na(x))) %>% table()
apply(dfMod[, -notCols], 2, function(x) sum(is.nan(x))) %>% table()
apply(dfMod[, -notCols], 2, function(x) sum(is.infinite(x))) %>% table()
apply(dfMod[, -notCols], 2, function(x) sum(x == 0, na.rm = T)) %>% table()
apply(dfMod[, -notCols], 2, function(x) sum(x < 0, na.rm = T)) %>% table()
# A handful of ctys have NaN values for all years and food categories.
# This is because they were zero and then turned NaN when dividing by Grand Total.
# Drop these.
dfMod$area[which(is.nan(dfMod$`Animal Products`))] %>% unique()
notThese <- dfMod$area[which(is.nan(dfMod$`Animal Products`))] %>% unique()
dfMod <- dfMod %>% subset(!(area %in% notThese))
# There is also one negative chronic hunger (child underweight) DALY value
# for DRC 2021. Remove it.
dfMod$area[which(dfMod$`DALYs / 100,000 capita` < 0)] %>% unique()
dfMod <- dfMod[-which(dfMod$`DALYs / 100,000 capita` < 0), ]
# There are also zero values due to a handful of ctys where people are not eating pulses
dfMod$area[which(dfMod$Pulses == 0)] %>% unique()
#  Replace with 1 so that they will become 0 after log transform.
# First Save untransformed data for summary stats
dfModSumStats <- dfMod
# Log transform
dfMod$Pulses[which(dfMod$Pulses == 0)] <- 1
dfMod[, -notCols] <- as.data.frame(apply(dfMod[, -notCols], 2, log))
apply(dfMod[, -notCols], 2, function(x) sum(is.nan(x)))
#dfMod$area[which(is.nan(dfMod$`DALYs / 100,000 capita`))]
#-----------------------------------------------------------------------
# Separate into data frames for each hunger model
catCol <- which(colnames(dfMod) == "Cat")
dfModHid <- dfMod %>% subset(Cat == "Hidden hunger") %>% select (-catCol)
dfModChr <- dfMod %>% subset(Cat == "Chronic hunger") %>% select (-catCol)
dfModOve <- dfMod %>% subset(Cat == "Overnutrition") %>% select (-catCol)
# Center data so that cty FEs can be interpreted as mean logged DALYs/100,000 capita
# (or the log of the geometric mean of DALYs/100,000 capita)
dfModChr[, -c(1, 2)] <- scale(dfModChr[, -c(1, 2)], scale = F) %>% as.data.frame()
dfModHid[, -c(1, 2)] <- scale(dfModHid[, -c(1, 2)], scale = F) %>% as.data.frame()
dfModOve[, -c(1, 2)] <- scale(dfModOve[, -c(1, 2)], scale = F) %>% as.data.frame()
#-----------------------------------------------------------------------
# Burden of chronic hunger model
colnames(dfModChr)[3] <- "y"
varsChr1 <- c("Pct Pop < 15", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses", "Sugar & Sweeteners")
varsChr2 <- c("Pct Pop < 15", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses")
varsChr3 <- c("sdi", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses", "Sugar & Sweeteners")
varsChr4 <- c("GDP / capita", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses", "Sugar & Sweeteners")
varsChr5 <- c("GDP / capita", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses")
varsChr1 <- paste0("`", varsChr1, "`")
varsChr2 <- paste0("`", varsChr2, "`")
varsChr3 <- paste0("`", varsChr3, "`")
varsChr4 <- paste0("`", varsChr4, "`")
varsChr5 <- paste0("`", varsChr5, "`")
modChr1a <- feols(y ~ .[varsChr1] | area, data = dfModChr, vcov = "hetero")
modChr1b <- feols(y ~ .[varsChr1] | area, data = dfModChr, cluster = c("area", "year"))
modChr2a <- feols(y ~ .[varsChr2] | area, data = dfModChr, vcov = "hetero")
modChr2b <- feols(y ~ .[varsChr2] | area, data = dfModChr, cluster = c("area", "year"))
modChr3a <- feols(y ~ .[varsChr3] | area, data = dfModChr, vcov = "hetero")
modChr3b <- feols(y ~ .[varsChr3] | area, data = dfModChr, cluster = c("area", "year"))
modChr4a <- feols(y ~ .[varsChr4] | area, data = dfModChr, vcov = "hetero")
modChr4b <- feols(y ~ .[varsChr4] | area, data = dfModChr, cluster = c("area", "year"))
modChr5a <- feols(y ~ .[varsChr5] | area, data = dfModChr, vcov = "hetero")
modChr5b <- feols(y ~ .[varsChr5] | area, data = dfModChr, cluster = c("area", "year"))
modChr5c <- feols(y ~ .[varsChr5] | year, data = dfModChr, cluster = c("area", "year")) # Lenaerts & Demont
modChr1a
modChr1b
modChr2a
modChr2b
modChr3a
modChr3b
modChr4a
modChr4b
modChr5a
modChr5b
modChr5c
# Plot of residuals v fitted values
plot(modChr1a$fitted.values, modChr1a$residuals)
# Looks like there are some outliers
# Remove outliers (abs(error) > 3 s.d.) and fit again
errVec <- modChr1a$residuals
names(errVec) <- dfModChr$area
sdErr <- sd(errVec)
indOutlier <- which(abs(errVec) > 3 * sdErr); names(indOutlier)
dfModChr <- dfModChr[-indOutlier, ]
# By informal visual inspection of e(t) v e(t-1) plot, it seems there's
# not much serial autocorrelation (after dropping outliers),
# while inspection of residuals v fitted values plot reveals some heteroskedasticity.
# So maybe it's better to report heteroskedasticity-robust standard errors instead of
# cty clustered standard errors.
errVec <- modChr1a$residuals
dfErr <- data.frame(area = dfModChr$area, err = errVec, yHat = modChr1a$fitted.values) %>% group_by(area) %>%
  mutate(errDif = c(NA, diff(err))) %>% as.data.frame()
dfErr <- dfErr[-which(is.na(dfErr$errDif)), ]
plot(dfErr$err, dfErr$errDif)
plot(modChr2a$fitted.values, modChr2a$residuals)
plot(modChr3a$fitted.values, modChr3a$residuals)
plot(modChr4a$fitted.values, modChr4a$residuals)
plot(modChr5a$fitted.values, modChr5a$residuals)
plot(modChr5c$fitted.values, modChr5c$residuals)
# Assess multicollinearity, heteroskedasticity for single years
# VIFs are all < 5 for all regressors and years it seems
# BP test indicates heteroskedasticity for some years, not for others
theseVars <- gsub("`", "", varsChr2)
dfTest <- dfModChr %>% subset(year == 2019) %>%
  select(y, all_of(theseVars))
modTest <- lm(y~., dfTest)
car::vif(modTest)
lmtest::bptest(modTest)
# Save estimations to word files for reporting
modelsA <- list(modChr1a, modChr2a, modChr3a, modChr4a, modChr5a, modChr5c)
modelsB <- list(modChr1b, modChr2b, modChr3b, modChr4b, modChr5b)
thisFile <- "ctyFEmods_Chr_htrskRob.docx"
thisFilepath <- paste0(outFolder, thisFile)
modelsummary(modelsA, output = thisFilepath, statistic = "p.value")
thisFile <- "ctyFEmods_Chr_clstRob.docx"
thisFilepath <- paste0(outFolder, thisFile)
modelsummary(modelsB, output = thisFilepath, statistic = "p.value")
# Pretty resid v fitted plots for reporting
areaVec <- dfModChr$area; yrVec <- dfModChr$year
dfErr1 <- data.frame(area = areaVec, year = yrVec, yHat = modChr1a$fitted.values, residual = modChr1a$residuals, model = 1)
dfErr2 <- data.frame(area = areaVec, year = yrVec, yHat = modChr2a$fitted.values, residual = modChr2a$residuals, model = 2)
dfErr3 <- data.frame(area = areaVec, year = yrVec, yHat = modChr3a$fitted.values, residual = modChr3a$residuals, model = 3)
dfErr4 <- data.frame(area = areaVec, year = yrVec, yHat = modChr4a$fitted.values, residual = modChr4a$residuals, model = 4)
dfErr5 <- data.frame(area = areaVec, year = yrVec, yHat = modChr5a$fitted.values, residual = modChr5a$residuals, model = 5)
dfErr6 <- data.frame(area = areaVec, year = yrVec, yHat = modChr5c$fitted.values, residual = modChr5c$residuals, model = 6)
dfErr <- do.call(rbind, list(dfErr1, dfErr2, dfErr3, dfErr4, dfErr5, dfErr6)) %>% as.data.frame()
gg <- ggplot(dfErr,  aes(x = yHat, y = residual)) + geom_point(size = 0.15)
gg <- gg + facet_wrap(~model, nrow = 1)
gg <- gg + labs(x = "fitted value")
gg <- gg + theme_bw()
gg <- gg + theme(axis.text = element_text(size = axisTextSize),
                            axis.title = element_text(size = axisTitleSize),
                            strip.text = element_text(size = facetTitleSize))
thisGraphic <- "errorYhatPlot_chr.png"
thisFilepath <- paste0(outFolder, thisGraphic)
ggsave(thisFilepath, width = 10, height = 2)
# Histograms of cty FEs, which may be interpreted as
# logged geometric mean of DALYs/100,000 since the data is centered
areaVec <- unique(areaVec)
dfGM1 <- data.frame(area = areaVec, FE = fixef(modChr1a)$area, model = 1)
dfGM2 <- data.frame(area = areaVec, FE = fixef(modChr2a)$area, model = 2)
dfGM3 <- data.frame(area = areaVec, FE = fixef(modChr3a)$area, model = 3)
dfGM4 <- data.frame(area = areaVec, FE = fixef(modChr4a)$area, model = 4)
dfGM5 <- data.frame(area = areaVec, FE = fixef(modChr5a)$area, model = 5)
dfGM <- do.call(rbind, list(dfGM1, dfGM2, dfGM3, dfGM4, dfGM5)) %>% as.data.frame()
gg <- ggplot(dfGM, aes(x = FE))
gg <- gg + geom_histogram(aes(y = ..density..),
               colour = 1, fill = "white", binwidth = 0.5)
gg <- gg + geom_density()
gg <- gg + labs(x = "Logged geometric mean DALYs/100,000 capita due to chronic hunger\n(country fixed effects)")
gg <- gg + facet_wrap(~model, nrow = 1)
gg <- gg + theme_bw()
gg <- gg + theme(axis.text = element_text(size = axisTextSize),
                 axis.title = element_text(size = axisTitleSize),
                 strip.text = element_text(size = facetTitleSize))
thisGraphic <- "FEdensityPlot_chr.png"
thisFilepath <- paste0(outFolder, thisGraphic)
ggsave(thisFilepath, width = 9, height = 2)
#=======================================================================
# Burden of hidden hunger model
colnames(dfModHid)[3] <- "y"
varsHid1 <- c("Pct Pop < 15", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses", "Sugar & Sweeteners")
varsHid2 <- c("Pct Pop < 15", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses")
varsHid3 <- c("sdi", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses", "Sugar & Sweeteners")
varsHid4 <- c("GDP / capita", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses", "Sugar & Sweeteners")
varsHid5 <- c("GDP / capita", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses")
varsHid1 <- paste0("`", varsHid1, "`")
varsHid2 <- paste0("`", varsHid2, "`")
varsHid3 <- paste0("`", varsHid3, "`")
varsHid4 <- paste0("`", varsHid4, "`")
varsHid5 <- paste0("`", varsHid5, "`")
modHid1a <- feols(y ~ .[varsHid1] | area, data = dfModHid, vcov = "hetero")
modHid1b <- feols(y ~ .[varsHid1] | area, data = dfModHid, cluster = c("area", "year"))
modHid2a <- feols(y ~ .[varsHid2] | area, data = dfModHid, vcov = "hetero")
modHid2b <- feols(y ~ .[varsHid2] | area, data = dfModHid, cluster = c("area", "year"))
modHid3a <- feols(y ~ .[varsHid3] | area, data = dfModHid, vcov = "hetero")
modHid3b <- feols(y ~ .[varsHid3] | area, data = dfModHid, cluster = c("area", "year"))
modHid4a <- feols(y ~ .[varsHid4] | area, data = dfModHid, vcov = "hetero")
modHid4b <- feols(y ~ .[varsHid4] | area, data = dfModHid, cluster = c("area", "year"))
modHid5a <- feols(y ~ .[varsHid5] | area, data = dfModHid, vcov = "hetero")
modHid5b <- feols(y ~ .[varsHid5] | area, data = dfModHid, cluster = c("area", "year"))
modHid5c <- feols(y ~ .[varsHid5] | year, data = dfModHid, cluster = c("area", "year")) # Lenaerts & Demont
modHid1a
modHid1b
modHid2a
modHid2b
modHid3a
modHid3b
modHid4a
modHid4b
modHid5a
modHid5b
modHid5c
# Plot of residuals v fitted values
plot(modHid1a$fitted.values, modHid1a$residuals)
# Doesn't look like there are any outliers
# By informal visual inspection of e(t) v e(t-1) plot, it seems there's
# not much serial autocorrelation (after dropping outliers),
# while inspection of residuals v fitted values plot reveals some heteroskedasticity.
# So maybe it's better to report heteroskedasticity-robust standard errors instead of
# cty clustered standard errors.
errVec <- modHid1a$residuals
dfErr <- data.frame(area = dfModHid$area, err = errVec, yHat = modHid1a$fitted.values) %>% group_by(area) %>%
  mutate(errDif = c(NA, diff(err))) %>% as.data.frame()
dfErr <- dfErr[-which(is.na(dfErr$errDif)), ]
plot(dfErr$err, dfErr$errDif)
plot(modHid2a$fitted.values, modHid2a$residuals)
plot(modHid3a$fitted.values, modHid3a$residuals)
plot(modHid4a$fitted.values, modHid4a$residuals)
plot(modHid5a$fitted.values, modHid5a$residuals)
plot(modHid5c$fitted.values, modHid5c$residuals)
# Assess multicollinearity, heteroskedasticity for single years
# VIFs are all < 5 for all regressors and years it seems
# BP test indicates heteroskedasticity for all years
theseVars <- gsub("`", "", varsHid1)
dfTest <- dfModHid %>% subset(year == 2012) %>%
  select(y, all_of(theseVars))
modTest <- lm(y~., dfTest)
car::vif(modTest)
lmtest::bptest(modTest)
# Save estimations to word files for reporting
modelsA <- list(modHid1a, modHid2a, modHid3a, modHid4a, modHid5a, modHid5c)
modelsB <- list(modHid1b, modHid2b, modHid3b, modHid4b, modHid5b)
thisFile <- "ctyFEmods_Hid_htrskRob.docx"
thisFilepath <- paste0(outFolder, thisFile)
modelsummary(modelsA, output = thisFilepath, statistic = "p.value")
thisFile <- "ctyFEmods_Hid_clstRob.docx"
thisFilepath <- paste0(outFolder, thisFile)
modelsummary(modelsB, output = thisFilepath, statistic = "p.value")
# Pretty resid v fitted plots for reporting
areaVec <- dfModHid$area; yrVec <- dfModHid$year
dfErr1 <- data.frame(area = areaVec, year = yrVec, yHat = modHid1a$fitted.values, residual = modHid1a$residuals, model = 1)
dfErr2 <- data.frame(area = areaVec, year = yrVec, yHat = modHid2a$fitted.values, residual = modHid2a$residuals, model = 2)
dfErr3 <- data.frame(area = areaVec, year = yrVec, yHat = modHid3a$fitted.values, residual = modHid3a$residuals, model = 3)
dfErr4 <- data.frame(area = areaVec, year = yrVec, yHat = modHid4a$fitted.values, residual = modHid4a$residuals, model = 4)
dfErr5 <- data.frame(area = areaVec, year = yrVec, yHat = modHid5a$fitted.values, residual = modHid5a$residuals, model = 5)
dfErr6 <- data.frame(area = areaVec, year = yrVec, yHat = modHid5c$fitted.values, residual = modHid5c$residuals, model = 6)
dfErr <- do.call(rbind, list(dfErr1, dfErr2, dfErr3, dfErr4, dfErr5, dfErr6)) %>% as.data.frame()
gg <- ggplot(dfErr,  aes(x = yHat, y = residual)) + geom_point(size = 0.15)
gg <- gg + facet_wrap(~model, nrow = 1)
gg <- gg + labs(x = "fitted value")
gg <- gg + theme_bw()
gg <- gg + theme(axis.text = element_text(size = axisTextSize),
                 axis.title = element_text(size = axisTitleSize),
                 strip.text = element_text(size = facetTitleSize))
thisGraphic <- "errorYhatPlot_Hid.png"
thisFilepath <- paste0(outFolder, thisGraphic)
ggsave(thisFilepath, width = 10, height = 2)
# Histograms of cty FEs, which may be interpreted as
# logged geometric mean of DALYs/100,000 since the data is centered
areaVec <- unique(areaVec)
dfGM1 <- data.frame(area = areaVec, FE = fixef(modHid1a)$area, model = 1)
dfGM2 <- data.frame(area = areaVec, FE = fixef(modHid2a)$area, model = 2)
dfGM3 <- data.frame(area = areaVec, FE = fixef(modHid3a)$area, model = 3)
dfGM4 <- data.frame(area = areaVec, FE = fixef(modHid4a)$area, model = 4)
dfGM5 <- data.frame(area = areaVec, FE = fixef(modHid5a)$area, model = 5)
dfGM <- do.call(rbind, list(dfGM1, dfGM2, dfGM3, dfGM4, dfGM5)) %>% as.data.frame()
gg <- ggplot(dfGM, aes(x = FE))
gg <- gg + geom_histogram(aes(y = ..density..),
                          colour = 1, fill = "white", binwidth = 0.25)
gg <- gg + geom_density()
gg <- gg + labs(x = "Logged geometric mean DALYs/100,000 capita due to hidden hunger\n(country fixed effects)")
gg <- gg + facet_wrap(~model, nrow = 1)
gg <- gg + theme_bw()
gg <- gg + theme(axis.text = element_text(size = axisTextSize),
                 axis.title = element_text(size = axisTitleSize),
                 strip.text = element_text(size = facetTitleSize))
thisGraphic <- "FEdensityPlot_Hid.png"
thisFilepath <- paste0(outFolder, thisGraphic)
ggsave(thisFilepath, width = 9, height = 2)
# Note the hidden hunger cty FEs histogram is bimodal.
# What ctys are in the higher risk category?
dfGM %>% subset(FE > 0) %>% .$area %>% unique() # Looks like low income developing ctys
#=======================================================================
# Burden of overnutrition model
colnames(dfModOve)[3] <- "y"
varsOve1 <- c("Pct Pop < 25", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses", "Sugar & Sweeteners")
varsOve2 <- c("Pct Pop < 25", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses")
varsOve3 <- c("sdi", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses", "Sugar & Sweeteners")
varsOve4 <- c("GDP / capita", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses", "Sugar & Sweeteners")
varsOve5 <- c("GDP / capita", "Animal Products", "Cereals",
              "Starchy Roots", "F&V", "Vegetable Oils",
              "Pulses")
varsOve1 <- paste0("`", varsOve1, "`")
varsOve2 <- paste0("`", varsOve2, "`")
varsOve3 <- paste0("`", varsOve3, "`")
varsOve4 <- paste0("`", varsOve4, "`")
varsOve5 <- paste0("`", varsOve5, "`")
modOve1a <- feols(y ~ .[varsOve1] | area, data = dfModOve, vcov = "hetero")
modOve1b <- feols(y ~ .[varsOve1] | area, data = dfModOve, cluster = c("area", "year"))
modOve2a <- feols(y ~ .[varsOve2] | area, data = dfModOve, vcov = "hetero")
modOve2b <- feols(y ~ .[varsOve2] | area, data = dfModOve, cluster = c("area", "year"))
modOve3a <- feols(y ~ .[varsOve3] | area, data = dfModOve, vcov = "hetero")
modOve3b <- feols(y ~ .[varsOve3] | area, data = dfModOve, cluster = c("area", "year"))
modOve4a <- feols(y ~ .[varsOve4] | area, data = dfModOve, vcov = "hetero")
modOve4b <- feols(y ~ .[varsOve4] | area, data = dfModOve, cluster = c("area", "year"))
modOve5a <- feols(y ~ .[varsOve5] | area, data = dfModOve, vcov = "hetero")
modOve5b <- feols(y ~ .[varsOve5] | area, data = dfModOve, cluster = c("area", "year"))
modOve5c <- feols(y ~ .[varsOve5] | year, data = dfModOve, cluster = c("area", "year")) # Lenaerts & Demont
modOve1a
modOve1b
modOve2a
modOve2b
modOve3a
modOve3b
modOve4a
modOve4b
modOve5a
modOve5b
modOve5c
# Plot of residuals v fitted values
plot(modOve1a$fitted.values, modOve1a$residuals)
# Doesn't look like there are any outliers.
# By informal visual inspection of e(t) v e(t-1) plot, it seems there's
# not much serial autocorrelation (after dropping outliers),
# while inspection of residuals v fitted values plot reveals some heteroskedasticity.
# So maybe it's better to report heteroskedasticity-robust standard errors instead of
# cty clustered standard errors.
errVec <- modOve1a$residuals
dfErr <- data.frame(area = dfModOve$area, err = errVec, yHat = modOve1a$fitted.values) %>% group_by(area) %>%
  mutate(errDif = c(NA, diff(err))) %>% as.data.frame()
dfErr <- dfErr[-which(is.na(dfErr$errDif)), ]
plot(dfErr$err, dfErr$errDif)
plot(modOve2a$fitted.values, modOve2a$residuals)
plot(modOve3a$fitted.values, modOve3a$residuals)
plot(modOve4a$fitted.values, modOve4a$residuals)
plot(modOve5a$fitted.values, modOve5a$residuals)
plot(modOve5c$fitted.values, modOve5c$residuals)
# Assess multicollinearity, heteroskedasticity for single years
# VIFs are all < 5 for all regressors and years it seems
# BP test indicates heteroskedasticity for some years, not for others
theseVars <- gsub("`", "", varsOve1)
dfTest <- dfModOve %>% subset(year == 2012) %>%
  select(y, all_of(theseVars))
modTest <- lm(y~., dfTest)
car::vif(modTest)
lmtest::bptest(modTest)
# Save estimations to word files for reporting
modelsA <- list(modOve1a, modOve2a, modOve3a, modOve4a, modOve5a, modOve5c)
modelsB <- list(modOve1b, modOve2b, modOve3b, modOve4b, modOve5b)
thisFile <- "ctyFEmods_Ove_htrskRob.docx"
thisFilepath <- paste0(outFolder, thisFile)
modelsummary(modelsA, output = thisFilepath, statistic = "p.value")
thisFile <- "ctyFEmods_Ove_clstRob.docx"
thisFilepath <- paste0(outFolder, thisFile)
modelsummary(modelsB, output = thisFilepath, statistic = "p.value")
# Pretty resid v fitted plots for reporting
areaVec <- dfModOve$area; yrVec <- dfModOve$year
dfErr1 <- data.frame(area = areaVec, year = yrVec, yHat = modOve1a$fitted.values, residual = modOve1a$residuals, model = 1)
dfErr2 <- data.frame(area = areaVec, year = yrVec, yHat = modOve2a$fitted.values, residual = modOve2a$residuals, model = 2)
dfErr3 <- data.frame(area = areaVec, year = yrVec, yHat = modOve3a$fitted.values, residual = modOve3a$residuals, model = 3)
dfErr4 <- data.frame(area = areaVec, year = yrVec, yHat = modOve4a$fitted.values, residual = modOve4a$residuals, model = 4)
dfErr5 <- data.frame(area = areaVec, year = yrVec, yHat = modOve5a$fitted.values, residual = modOve5a$residuals, model = 5)
dfErr6 <- data.frame(area = areaVec, year = yrVec, yHat = modOve5c$fitted.values, residual = modOve5c$residuals, model = 6)
dfErr <- do.call(rbind, list(dfErr1, dfErr2, dfErr3, dfErr4, dfErr5, dfErr6)) %>% as.data.frame()
gg <- ggplot(dfErr,  aes(x = yHat, y = residual)) + geom_point(size = 0.15)
gg <- gg + facet_wrap(~model, nrow = 1)
gg <- gg + labs(x = "fitted value")
gg <- gg + theme_bw()
gg <- gg + theme(axis.text = element_text(size = axisTextSize),
                 axis.title = element_text(size = axisTitleSize),
                 strip.text = element_text(size = facetTitleSize))
thisGraphic <- "errorYhatPlot_Ove.png"
thisFilepath <- paste0(outFolder, thisGraphic)
ggsave(thisFilepath, width = 10, height = 2)
# Histograms of cty FEs, which may be interpreted as
# logged geometric mean of DALYs/100,000 since the data is centered
areaVec <- unique(areaVec)
dfGM1 <- data.frame(area = areaVec, FE = fixef(modOve1a)$area, model = 1)
dfGM2 <- data.frame(area = areaVec, FE = fixef(modOve2a)$area, model = 2)
dfGM3 <- data.frame(area = areaVec, FE = fixef(modOve3a)$area, model = 3)
dfGM4 <- data.frame(area = areaVec, FE = fixef(modOve4a)$area, model = 4)
dfGM5 <- data.frame(area = areaVec, FE = fixef(modOve5a)$area, model = 5)
dfGM <- do.call(rbind, list(dfGM1, dfGM2, dfGM3, dfGM4, dfGM5)) %>% as.data.frame()
gg <- ggplot(dfGM, aes(x = FE))
gg <- gg + geom_histogram(aes(y = ..density..),
                          colour = 1, fill = "white", binwidth = 0.25)
gg <- gg + geom_density()
gg <- gg + labs(x = "Logged geometric mean DALYs/100,000 capita due to overnutrition\n(country fixed effects)")
gg <- gg + facet_wrap(~model, nrow = 1)
gg <- gg + theme_bw()
gg <- gg + theme(axis.text = element_text(size = axisTextSize),
                 axis.title = element_text(size = axisTitleSize),
                 strip.text = element_text(size = facetTitleSize))
thisGraphic <- "FEdensityPlot_Ove.png"
thisFilepath <- paste0(outFolder, thisGraphic)
ggsave(thisFilepath, width = 9, height = 2)
# Note the higher overnutrition cty FEs include many developed & ex-USSR ctys
dfGM %>% subset(FE > 0.5) %>% .$area %>% unique()
#========================================================================
#========================================================================
#========================================================================
# Graphics/tables
#========================================================================
#========================================================================
# Summary statistic tables
# Hunger summary stats by type, year
dfModSumStats %>% select(year, Cat, `DALYs / 100,000 capita`) %>%
  group_by(Cat, year) %>% summarize_all(list(#N = "length",
                                        mean = "mean",
                                        `s.d.` = "sd",
                                        max = "max",
                                        min = "min")) %>%
  gather(stat, val, mean:min) %>% as.data.frame() %>%
  mutate(stat = factor(stat, levels = unique(stat)),
         year = factor(year, levels = unique(year)),
         val = round(val, 2)) %>% 
  ggplot(aes(x = stat, y = year)) +
  geom_tile(alpha = 0) +
  geom_text(aes(label = val), size = labelSize) +
  scale_x_discrete(position = "top") +
    facet_wrap(~Cat) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = axisTextSize),
        plot.title = element_text(size = plotTitleSize),
        strip.text = element_text(size = facetTitleSize))
thisFile <- "Hunger summary statistics.png"
thisFilepath <- paste0(outFolder, thisFile)
ggsave(thisFilepath, height = 4, width = 5)
#------------------------------------------------------------------------
# Socioeconomic/demographic regressor summary stats by year
regrssrs <- colnames(dfModSumStats)[-c(1:4)]
dfModSumStats %>% subset(Cat == "Chronic hunger") %>%
  select(year, regrssrs) %>%
  gather_("var", "val", regrssrs) %>%
  group_by(year, var) %>% summarize_all(list(#N = "length",
                                        mean = "mean",
                                        `s.d.` = "sd",
                                        max = "max",
                                        min = "min")) %>%
  gather(stat, val, mean:min) %>% as.data.frame() %>%
  mutate(stat = factor(stat, levels = unique(stat)),
         year = factor(year, levels = unique(year)),
         val = round(val, 2)) %>% 
  ggplot(aes(x = stat, y = year)) +
  geom_tile(alpha = 0) +
  geom_text(aes(label = val), size = labelSize) +
  scale_x_discrete(position = "top") +
  facet_wrap(~var, ncol = 3) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = axisTextSize),
        plot.title = element_text(size = plotTitleSize),
        strip.text = element_text(size = facetTitleSize))
thisFile <- "Regressor summary statistics.png"
thisFilepath <- paste0(outFolder, thisFile)
ggsave(thisFilepath, height = 6, width = 5)
#------------------------------------------------------------------------
# Correlation plot
dfMod %>% subset(year == 2021) %>%
  spread(Cat, `DALYs / 100,000 capita`) %>%
  select(-c(year, area)) %>% chart.Correlation(histogram=TRUE, pch=19)
# Manually save using Export in the plot viewer, use width 1500 height 1000
#------------------------------------------------------------------------
# Create graphics data frame for regionally disaggregated IHME GBD data plots
thisFile <- "IHME-GBD_1990-2021_cNutDef_byAge_reg.csv"
thisFilepath <- paste0(dataFolder, thisFile)
dfGraphicsC <- read.csv(thisFilepath, stringsAsFactors = F) %>%
  subset(cause %in% hidHcauses) %>%
  rename(`DALYs / 100,000 capita` = val,
         area = location) %>%
  select(area, year, cause, age, metric, measure, `DALYs / 100,000 capita`, upper, lower) %>%
  mutate(Cat = "Hidden hunger")
thisFile <- "IHME-GBD_1990-2021_rNutDef_byAge_reg.csv"
thisFilepath <- paste0(dataFolder, thisFile)
dfGraphicsR <- read.csv(thisFilepath, stringsAsFactors = F) %>%
  subset(rei %in% c(overNutRiskFactors, "Child underweight")) %>%
  select(-cause) %>%
  rename(`DALYs / 100,000 capita` = val,
         area = location,
         cause = rei) %>%
  select(area, year, cause, age, metric, measure, `DALYs / 100,000 capita`, upper, lower)
dfGraphicsR$Cat <- NA
dfGraphicsR$Cat[grep("Child underweight", dfGraphicsR$cause)] <- "Chronic hunger"
dfGraphicsR$Cat[which(dfGraphicsR$cause %in% overNutRiskFactors)] <- "Overnutrition"
dfGraphics <- rbind(dfGraphicsC, dfGraphicsR) %>% as.data.frame()
#------------------------------------------------------------------------
# Graphic demonstrating that nutrition DALYs/capita are age sensitive
dfPlot <- dfGraphics %>% subset(year == 2021 &
                                  age != "All ages" &
                                  metric == "Rate" &
                                  measure == "DALYs (Disability-Adjusted Life Years)" &
                                  area != "Global" &
                                  Cat %in% c("Chronic hunger",
                                             "Hidden hunger",
                                             "Overnutrition"))  %>%
  mutate(area = gsub(" - WB", "", area),
         age = factor(age, levels = unique(age)))
indChr <- which(dfPlot$Cat == "Chronic hunger")
dfPlot$`DALYs / 100,000 capita`[indChr] <- dfPlot$lower[indChr]
#dfPlot$age <- factor(dfPlot$age, levels = unique(dfPlot$age))
nColors <- length(unique(dfPlot$cause))
bag_of_colors <- distinctColorPalette(k = 2 * nColors)
theseColors <- sample(bag_of_colors, nColors)
gg <- ggplot(dfPlot, aes(x = age, y = `DALYs / 100,000 capita`, fill = cause))
gg <- gg + geom_bar(stat = "identity", position = "stack")
gg <- gg + facet_grid(Cat~area, scales = "free")
gg <- gg + scale_fill_manual(values = theseColors)
gg <- gg + theme_bw()
gg <- gg + theme(axis.text.x = element_text(size = axisTextSize,
                                            angle = 60, hjust = 1),
                 axis.title.x = element_text(size = axisTitleSize),
                 axis.title.y = element_blank(),
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendTextSize),
                 legend.position = "top",
                 legend.key.size = unit(legendKeySize, "cm"),
                 strip.text = element_text(size = facetTitleSize))
gg <- gg + coord_flip()
thisGraphic <- "Hunger DALYs per cap by age group.png"
thisFilepath <- paste0(outFolder, thisGraphic)
ggsave(thisFilepath, width = 8, height = 8)
#------------------------------------------------------------------------
# Line plot of burden of hunger over time, by type, disaggregated by WB region
#theseCauses <- c(hidHcauses, overNutRiskFactors, "Protein-energy malnutrition")
theseCauses <- c(hidHcauses, overNutRiskFactors, "Child underweight")
dfPlot <- dfGraphics %>% subset(age == "All ages" &
                                metric == "Rate" &
                                measure == "DALYs (Disability-Adjusted Life Years)" &
                                Cat %in% c("Chronic hunger",
                                           "Hidden hunger",
                                           "Overnutrition")) %>%
  mutate(area = gsub(" - WB", "", area))
dfPlot <- dfPlot %>% group_by(area, year, Cat) %>%
  summarise(`DALYs / 100,000 capita` = sum(`DALYs / 100,000 capita`))
dfPlot$area[grep("Global", dfPlot$area)] <- "World"
nColors <- length(unique(dfPlot$Cat))
bag_of_colors <- distinctColorPalette(k = 2 * nColors)
theseColors <- sample(bag_of_colors, nColors)
gg <- ggplot(dfPlot, aes(x = year, y = `DALYs / 100,000 capita`, group = Cat, color = Cat))
gg <- gg + geom_line(lwd = 1)
gg <- gg + scale_color_manual(values = theseColors)
gg <- gg + facet_wrap(~area, nrow = 2, scales = "free_y")
gg <- gg + theme_bw()
gg <- gg + theme(axis.text.x = element_text(size = axisTextSize,
                                            angle = 60, hjust = 1),
                 axis.title.y = element_text(size = axisTitleSize),
                 axis.title.x = element_blank(),
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendTextSize),
                 legend.position = "top",
                 legend.key.size = unit(legendKeySize, "cm"),
                 strip.text = element_text(size = facetTitleSize))
thisGraphic <- "Hunger DALYs 1990-2021 by region.png"
thisFilepath <- paste0(outFolder, thisGraphic)
ggsave(thisFilepath, width = 8, height = 4)
#------------------------------------------------------------------------
# Area stack plot of the cause composition of burden of hunger over time,
# by type, disaggregated by WB region 1990-2021
#theseCauses <- c(hidHcauses, overNutRiskFactors, "Protein-energy malnutrition")
thisFile <- "IHME-GBD_1990-2021_ChronicHungDecomp_reg.csv"
thisFilepath <- paste0(dataFolder, thisFile)
dfChrDecomp <- read.csv(thisFilepath, stringsAsFactors = F) %>%
  subset(age == "All ages" &
         metric == "Rate" &
         measure == "DALYs (Disability-Adjusted Life Years)") %>%
  rename(Cat = rei, area = location) %>% mutate(Cat = "Chronic hunger") %>%
  select(area, year, Cat, cause, val) %>% rename(`DALYs / 100,000 capita` = val)
dfPlot <- dfGraphics %>% subset(age == "All ages" &
                                  metric == "Rate" &
                                  measure == "DALYs (Disability-Adjusted Life Years)" &
                                  Cat %in% c("Hidden hunger",
                                             "Overnutrition")) %>%
  select(area, year, Cat, cause, `DALYs / 100,000 capita`) %>%
  rbind(dfChrDecomp) %>% as.data.frame() %>%
  mutate(area = gsub(" - WB", "", area))
dfPlot$area[grep("Global", dfPlot$area)] <- "World"
catVec <- unique(dfPlot$Cat)
nCat <- length(catVec)
for(i in 1:nCat){
  thisCat <- catVec[i]
  thisDfPlot <- subset(dfPlot, Cat == thisCat)
  nColors <- length(unique(thisDfPlot$cause))
  bag_of_colors <- distinctColorPalette(k = 2 * nColors)
  theseColors <- sample(bag_of_colors, nColors)
  gg <- ggplot(thisDfPlot, aes(x = year, y = `DALYs / 100,000 capita`, fill = cause))
  gg <- gg + geom_area(position = "stack")
  gg <- gg + scale_fill_manual(values = theseColors)
  gg <- gg + facet_wrap(~area, nrow = 2, scales = "free_y")
  gg <- gg + theme_bw()
  gg <- gg + theme(axis.text.x = element_text(size = axisTextSize,
                                              angle = 60, hjust = 1),
                   axis.title.y = element_text(size = axisTitleSize),
                   axis.title.x = element_blank(),
                   legend.title = element_blank(),
                   legend.text = element_text(size = legendTextSize),
                   legend.position = "top",
                   legend.key.size = unit(legendKeySize, "cm"),
                   strip.text = element_text(size = facetTitleSize))
  thisGraphic <- paste(thisCat, "DALYs cause decomp 1990-2021 by region.png")
  thisFilepath <- paste0(outFolder, thisGraphic)
  ggsave(thisFilepath, width = 7, height = 3)
}

#------------------------------------------------------------------------
# Area stack plot of consumption/capita/day of major food categories 2010-2022
# By geographic region
thisFile <- "FAOSTAT_data_en_8-7-2024.csv"
thisFilepath <- paste0(dataFolder, thisFile)
sugLook <- c("Sugar Crops", "Sugar & Sweeteners",
             "Sugar (Raw Equivalent)")
keepThese <- c("Grand Total", "Animal Products",
               "Cereals - Excluding Beer", "Starchy Roots",
               "Sugar & Sweeteners",
               "Pulses", "Vegetable Oils",
               "Vegetables", "Fruits - Excluding Wine")
LACvec <- c("South America", "Central America", "Caribbean")
EAPvec <- c("Australia and New Zealand", "Eastern Asia", "South-eastern Asia")
EURCAvec <- c("Europe", "Central Asia")
MENAvec <- c("Northern Africa", "Western Asia")
allRegs <- c(LACvec, EAPvec, EURCAvec, MENAvec, "Northern America", "Southern Asia", "Africa", "World")
dfFBSreg <- read.csv(thisFilepath, stringsAsFactors = F) %>%
  select(Area, Year, Item, Element, Value) %>%
  subset(Area %in% allRegs)
# unique(dfFBSreg$Item)
# unique(dfFBSreg$Element)
# unique(dfFBSreg$Area)
# dfLookSugar <- dfFBSreg %>% subset(Item %in% sugLook & Year == 2021)
# View(dfLookSugar)
# dfPop <- dfFBSreg %>% subset(Item == "Population")
# dfFBSreg <- dfFBSreg %>% subset(Item != "Population")
dfSSA <- dfFBSreg %>% subset(Area %in% c("Africa", "Northern Africa")) %>%
  spread(Area, Value) %>% mutate(Value = Africa - `Northern Africa`,
                                 Area = "Sub-Saharan Africa") %>%
  select(Area, Year, Item, Element, Value)
dfSSApop <- dfSSA %>% subset(Item == "Population") %>%
  select(Year, Value) %>% rename(Pop = Value)
dfSSA <- dfSSA %>% subset(Item != "Population") %>% merge(dfSSApop) %>%
  mutate(`kcal / capita / day` = Value / Pop * 1000 / 365) %>%
  select(Area, Year, Item, `kcal / capita / day`)
u <- dfFBSreg$Area
dfFBSreg$Area[which(u %in% LACvec)] <- "Latin America & Caribbean"
dfFBSreg$Area[which(u %in% EAPvec)] <- "East Asia & Pacific"
dfFBSreg$Area[which(u %in% EURCAvec)] <- "Europe & Central Asia"
dfFBSreg$Area[which(u %in% MENAvec)] <- "Middle East & North Africa"
dfFBSreg$Area[grep("Northern America", u)] <- "North America"
dfFBSreg$Area[grep("Southern Asia", u)] <- "South Asia"
dfFBSreg <- dfFBSreg %>% subset(Area != "Africa") %>%
  group_by(Area, Year, Item) %>% summarise(Value = sum(Value, na.rm = T))
dfPop <- dfFBSreg %>% subset(Item == "Population") %>%
  select(Area, Year, Value) %>% rename(Pop = Value)
dfFBSreg <- dfFBSreg %>% subset(Item != "Population") %>%
  merge(dfPop) %>%
  mutate(`kcal / capita / day` = Value / Pop * 1000 / 365) %>%
  select(Area, Year, Item, `kcal / capita / day`) %>%
  rbind(dfSSA) %>% as.data.frame()
# dfCheck <- dfFBSraw %>% merge(dfModChr[, c(1, 2)]) %>%
#   subset(element == "Food supply (kcal/capita/day)" &
#                                  item %in% c("Eggs", "Fish, Seafood", "Milk - Excluding Butter", "Animal Products")) %>%
#   spread(item, value)
# apply(dfCheck, 2, function(x) sum(is.na(x))) %>% table()
# apply(dfCheck, 2, function(x) sum(is.nan(x))) %>% table()
# apply(dfCheck, 2, function(x) sum(is.infinite(x))) %>% table()
# apply(dfCheck, 2, function(x) sum(x == 0)) %>% table()
# sum(dfCheck$`Fish, Seafood` == 0)
dfPlot <- dfFBSreg %>%
  subset(Item %in% keepThese) %>%
  mutate(Item = gsub("Fruits - Excluding Wine", "F&V", Item)) %>%
  mutate(Item = gsub("Vegetables", "F&V", Item)) %>%
  mutate(Item = gsub("Cereals - Excluding Beer", "Cereals", Item)) %>%
  group_by(Area, Year, Item) %>%
  summarise(`kcal / capita / day` = sum(`kcal / capita / day`, na.rm = T)) %>%
  spread(Item, `kcal / capita / day`)
dfPlot[, -c(1, 2)] <- dfPlot[, -c(1, 2)] / dfPlot$`Grand Total` * 100
dfPlot$`Grand Total` <- NULL
dfPlot$Residual <- 100 - rowSums(dfPlot[, -c(1, 2)])
gatherCols <- colnames(dfPlot)[-c(1, 2)]
dfPlot <- dfPlot %>% as.data.frame() %>%
  gather_("Item", "% kcal / capita / day", gatherCols)
dfPlot$Year <- as.integer(dfPlot$Year) #factor(dfPlot$Year, levels = unique(dfPlot$Year))
nColors <- length(unique(dfPlot$Item))
bag_of_colors <- distinctColorPalette(k = 5 * nColors)
theseColors <- sample(bag_of_colors, nColors)
gg <- ggplot(dfPlot, aes(x = Year, y = `% kcal / capita / day`, fill = reorder(Item, `% kcal / capita / day`)))
gg <- gg + geom_area(position = "stack")
gg <- gg + scale_fill_manual(values = theseColors)
gg <- gg + scale_x_continuous(breaks = pretty_breaks(n = 4))
gg <- gg + facet_wrap(~Area, nrow = 2, scales = "free_y")
gg <- gg + theme_bw()
gg <- gg + theme(axis.text.x = element_text(size = axisTextSize),
                 axis.title.y = element_text(size = axisTitleSize),
                 axis.title.x = element_blank(),
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendTextSize),
                 legend.position = "top",
                 legend.key.size = unit(legendKeySize, "cm"),
                 strip.text = element_text(size = facetTitleSize))
gg
thisGraphic <- "Diet composition 2010-2022 by region.png"
thisFilepath <- paste0(outFolder, thisGraphic)
ggsave(thisFilepath, width = 8, height = 4)





#
# # Graphic demonstrating low confidence around child underweight disease sequelae DALYs
# # as compared to protein-energy malnourishment DALYs
# theseCauses <- c("Child underweight", "Protein-energy malnutrition")
# dfPlot <- dfGraphics %>% subset(age == "All ages" &
#                                   metric == "Rate" &
#                                   measure == "DALYs (Disability-Adjusted Life Years)" &
#                                   cause %in% theseCauses &
#                                   area == "Global")
# nColors <- length(unique(dfPlot$cause))
# bag_of_colors <- distinctColorPalette(k = 2 * nColors)
# theseColors <- sample(bag_of_colors, nColors)
# gg <- ggplot(dfPlot, aes(x = year, y = `DALYs / 100,000 capita`, group = cause, color = cause))
# gg <- gg + geom_line(lwd = 1)
# gg <- gg + geom_ribbon(aes(ymin = lower, ymax = upper, group = cause, fill = cause), alpha = 0.5)
# gg <- gg + scale_color_manual(values = theseColors)
# gg <- gg + scale_fill_manual(values = theseColors)
# #gg <- gg + facet_wrap(~area, nrow = 2)
# gg <- gg + theme_bw()
# gg <- gg + theme(axis.text.x = element_text(size = axisTextSize,
#                                             angle = 60, hjust = 1),
#                  axis.title.y = element_text(size = axisTitleSize),
#                  axis.title.x = element_blank(),
#                  legend.title = element_blank(),
#                  legend.text = element_text(size = legendTextSize),
#                  legend.position = "top",
#                  legend.key.size = unit(legendKeySize, "cm"),
#                  strip.text = element_text(size = facetTitleSize))
# gg
#------------------------------------------------------------------------
# Graphic of variable correlations
















#------------------------------------------------------------------------
# Hunger histograms
thisYr <- 2019
thisFile <- paste0("Hunger density plots ", thisYr, ".png")
thisFilepath <- paste0(outFolder, thisFile)
dfMod %>% subset(year == thisYr) %>%
  rename(`DALYs / 100,000 capita, logged` = `DALYs / 100,000 capita`) %>%
ggplot(aes(x = `DALYs / 100,000 capita, logged`)) +
  geom_density() + facet_wrap(~Cat, nrow = 1) + labs(title = thisYr) +
  theme_bw() +
  theme(axis.title = element_text(size = axisTitleSize),
        axis.text = element_text(size = axisTextSize),
        plot.title = element_text(size = plotTitleSize)) +
ggsave(thisFilepath)

#========================================================================



















































# # Ctys in cause data frame follow standard UN/FAO naming convention.
# # Ctys in risk data frame are in some weird format.
# setdiff(unique(dfCauseRaw$location), unique(dfRiskRaw$location))
# #dfRctys <- data.frame(loc1 = unique(dfRiskRaw$location))
# unique(dfRiskRaw$location[grep("Korea", dfRiskRaw$location)])
# # Start by removing non-UN standard parts of the cty names in the risk data.
# thesePatterns <- "Sultanate of |Hashemite |Principality of |People's Democratic Republic of |People's Republic of |Islamic Republic of |Arab Republic of |Eastern Republic of |Republic of |Kingdom of |Commonwealth of |Democratic |Socialist |Union |Independent State of |State of |of the |Federative "
# dfRiskRaw$location <- gsub(thesePatterns, "", dfRiskRaw$location)
# setdiff(unique(dfCauseRaw$location), unique(dfRiskRaw$location))
# # Use countries package to create UN cty names. Helps identify remaining names to be corrected.
# dfRiskRaw$location_UN <- country_name(dfRiskRaw$location, to = "UN_en", verbose = T)
# u <- dfRiskRaw$location
# dfRiskRaw$location[grep("Portuguese Republic", u)] <- "Portugal"
# dfRiskRaw$location[grep("Mexican States", u)] <- "Mexico"
# dfRiskRaw$location[grep("Luxembourg", u)] <- "Luxembourg"
# dfRiskRaw$location_UN <- country_name(dfRiskRaw$location, to = "UN_en", verbose = T)
# setdiff(unique(dfCauseRaw$location), unique(dfRiskRaw$location_UN))
# u <- dfRiskRaw$location
# dfRiskRaw$location_UN[grep("Korea", dfRiskRaw$location_UN)] %>% unique()
# # Use countries package to change to create ISO3 name.
# country_name("Bolivarian Venezuela", to = "UN_en", verbose = T)
# dfCauseRaw$iso3 <- country_name(dfCauseRaw$location, to = "ISO3", verbose = T)
# dfRiskRaw$iso3 <- country_name(dfRiskRaw$location, to = "ISO3", verbose = T)
# 
# setdiff(unique(dfCauseRaw$location), unique(dfRiskRaw$location))
# # The cause data has a more standard naming convention that matches well
# # with FAO FBS and UN population data which we'll have to merge with later on.
# # So we'll adapt the risk factor data cty names to the cause data cty names.
# 
# setdiff(unique(dfCauseRaw$location), unique(dfRiskRaw$location))




# dfRaw <- get_faostat_bulk(code = "MK", data_folder = dataFolder)
# keepCols <- c("area", "year", "item", "element", "unit", "value")
# dfGDP <- dfRaw[, keepCols] %>% subset(item == "Gross Domestic Product" &
#                                         element == "value_us__per_capita__2015_prices") %>%
#   select(c("area", "year", "value")) %>%
#   rename(`GDP / capita (USD 2015 prices)` = value)
# dfGDP <- dfGDP %>% subset(!(area %in% notThese))
# dfGDP$area[grep("United Kingdom", dfGDP$area)] <- "United Kingdom"
# dfGDP$area[grep("Netherlands", dfGDP$area)] <- "Netherlands"
# # setdiff(unique(dfGDP$area), unique(dfRiskRaw$location))
# # unique(dfGDP$area)[order(unique(dfGDP$area))]
# # unique(dfRiskRaw$location)[order(unique(dfRiskRaw$location))]
# #dfFBSraw <- merge(dfFBSraw, dfGDP)

#========================================================================










# # Test multicollinearity
# dfModChrTest <- dfModChr %>% subset(year == 2019) %>%
#   select(-c(area, year, sdi))
# modTest <- lm(y~., dfModChrTest)
# summary(modTest)
# car::vif(modTest)
# # Test endogeneity
# thisFile <- "FAOSTAT_urbanRuralPop.csv"
# thisFilepath <- paste0(dataFolder, thisFile)
# dfIV <- read.csv(thisFilepath, stringsAsFactors = F) %>%
#   select(Area, Year, Element, Value) %>%
#   spread(Element, Value) %>%
#   mutate(PctPopUrban = 100 * `Urban population` / (`Rural population` + `Urban population`)) %>%
#   select(Area, Year, PctPopUrban)
# colnames(dfIV)[1:2] <- tolower(colnames(dfIV)[1:2])
# dfModChrTest <- dfModChr %>% merge(dfIV)
# varsChr2SLS <- setdiff(varsChr1, c("`Animal Products`", "`sdi`"))
# #varsChr2SLS <- setdiff(varsChr2, c("`Cereals`", "`sdi`"))
# modChr2SLS <- feols(y ~ .[varsChr2SLS] | area | `Animal Products` ~ PctPopUrban, data = dfModChrTest, cluster = "area")
# modChr2SLS
# plot(modChr2SLS$fitted.values, modChr2SLS$residuals)
# 
# 
# 
# 
# dfModChrTest <- dfModChr %>% subset(year == 2019)
# library(ivreg)
# mod2SLS <- ivreg(y ~  | `Animal Products` |
#                    nearcollege + poly(age, 2), dfModChrTest)
# 
# modTest <- lm(y~., dfModChrTest)
# summary(modTest)
# 
# 
# 
# 
# modChrEndogSDI <- feols(y ~ sdi | area, data = dfModChr, vcov = "hetero")
# summary(modChrEndogSDI); plot(modChrEndogSDI$fitted.values, modChrEndogSDI$residuals)
# modChrEndogPop <- feols(y ~ `Pct Pop < 15` | area, data = dfModChr, vcov = "hetero")
# summary(modChrEndogPop)
# plot(modChrEndogSDI$fitted.values, modChrEndogSDI$residuals)










#unique(u[grep("Cote", u)])
dfSDI$area[grep("Venezuela", u)] <- "Venezuela (Bolivarian Republic of)"
dfSDI$area[grep("USA", u)] <- "United States of America"
dfSDI$area[grep("Vietnam", u)] <- "Viet Nam"
dfSDI$area[grep("Tanzania", u)] <- "United Republic of Tanzania"
dfSDI$area[grep("UK", u)] <- "United Kingdom"
dfSDI$area[grep("Turkey", u)] <- "Turkiye"
dfSDI$area[grep("Syria", u)] <- "Syrian Arab Republic"
dfSDI$area[grep("Russia", u)] <- "Russian Federation"
dfSDI$area[grep("Moldova", u)] <- "Republic of Moldova"
dfSDI$area[grep("South Korea", u)] <- "Republic of Korea"
dfSDI$area[grep("Micronesia", u)] <- "Micronesia (Federated States of)"
dfSDI$area[grep("Laos", u)] <- "Lao People's Democratic Republic"
dfSDI$area[grep("Iran", u)] <- "Iran (Islamic Republic of)"
dfSDI$area[grep("Gambia", u)] <- "Gambia"
dfSDI$area[grep("DR Congo", u)] <- "Democratic Republic of the Congo"
dfSDI$area[grep("North Korea", u)] <- "Democratic People's Republic of Korea"
dfSDI$area[grep("Czech", u)] <- "Czechia"
dfSDI$area[grep("Congo \\(Brazzaville\\)", u)] <- "Congo"
dfSDI$area[grep("Cape Verde", u)] <- "Cabo Verde"
dfSDI$area[grep("Brunei", u)] <- "Brunei Darussalam"
dfSDI$area[grep("São Tomé and PrÍncipe", u)] <- "Sao Tome and Principe"
dfSDI$area[grep("Bolivia", u)] <- "Bolivia (Plurinational State of)"
dfSDI$area[grep("Bahamas", u)] <- "Bahamas"
# u <- dfSDI$area
# unique(u[grep("Lesotho", u)])
# u <- dfDaly$area
# unique(u[grep("Eswatini", u)])
# x1 <- unique(subset(dfDaly, year == 2019)$area)
# x2 <- unique(subset(merge(dfDaly, dfSDI), year == 2019)$area)
# setdiff(x1, x2)
# intersect(dfSDI$area, dfDaly$area)
dfDaly <- dfDaly %>% merge(dfSDI) %>% subset(area != "United States Virgin Islands")

# Create region groupings
dfDaly$Region <- NA
dfDaly$Region[which(dfDaly$area %in% ctyInAsansCWA)] <- "Asia\nexcl. C&W Asia"
dfDaly$Region[which(dfDaly$area %in% ctyInNAMEURAUSNZ)] <- "Eur. / N. Amer. /\nAus. / NZ"
dfDaly$Region[which(dfDaly$area %in% ctyInLAC)] <- "Lat. Amer. /\nCaribbean"
dfDaly$Region[which(dfDaly$area %in% ctyInCWANA)] <- "CWANA"
dfDaly$Region[which(dfDaly$area %in% ctyInSSA)] <- "Africa South\nof the Sahara"
unique(dfDaly$area[which(is.na(dfDaly$Region))])
u <- unique(dfDaly$area)
ctyAll <- c(ctyInAsansCWA, ctyInCWANA, ctyInLAC, ctyInSSA, ctyInNAMEURAUSNZ)
# ctyAll[grep("China", ctyAll)]
# u[grep("Brunei", u)]
ctyInNAMEURAUSNZ[grep("United Kingdom of", ctyInNAMEURAUSNZ)] <- "United Kingdom"
ctyInNAMEURAUSNZ[grep("Netherlands", ctyInNAMEURAUSNZ)] <- "Netherlands"
ctyInAsansCWA[grep("China, mainland", ctyInAsansCWA)] <- "China"
dfDaly$Region[which(dfDaly$area %in% ctyInNAMEURAUSNZ)] <- "Eur. / N. Amer. /\nAus. / NZ"
dfDaly$Region[which(dfDaly$area %in% ctyInAsansCWA)] <- "Asia\nexcl. C&W Asia"
unique(dfDaly$area[which(is.na(dfDaly$Region))])
dfDaly <- dfDaly[, c("area", "Region", colnames(dfDaly)[-c(1, ncol(dfDaly))])]
#=========================================================
#--------------------------------------------------------------
# PCA
# Get dataset for risk factor PCA
# Exclude level 1 headings or otherwise redundant headings
thisYr <- 2019
thisMetric <- "Rate"
thisAge <- "All ages"
# rmRisk <- c("Childhood sexual abuse and bullying",
#             "Intimate partner violence", "Unsafe sex",
#             "All risk factors",
#             "Child and maternal malnutrition", "Metabolic risks",
#             "Dietary risks")
#---
chldMatern <- c("Suboptimal breastfeeding",
                "Iron deficiency",
                "Vitamin A deficiency",
                "Zinc deficiency",
                "Child growth failure",
                "Low birth weight and short gestation")
metab <- c("Low physical activity",
           "Kidney dysfunction",
           "High fasting plasma glucose",
           "High systolic blood pressure",
           "High body-mass index",
           "High LDL cholesterol",
           "Low bone mineral density")
environ <- c("High temperature",
             "Low temperature",
             "Unsafe water, sanitation, and handwashing",
             "Air pollution",
             "Other environmental risks",
             "Occupational risks")
subAbuse <- c("Tobacco", "Alcohol use", "Drug use")
diet <- c("Diet low in fruits",
          "Diet low in vegetables",
          "Diet low in whole grains",
          "Diet low in nuts and seeds",
          "Diet low in milk",
          "Diet high in red meat",
          "Diet high in processed meat",
          "Diet high in sugar-sweetened beverages",
          "Diet low in fiber",
          "Diet low in seafood omega-3 fatty acids",
          "Diet low in polyunsaturated fatty acids",
          "Diet high in trans fatty acids",
          "Diet high in sodium",
          "Diet low in calcium",
          "Diet low in legumes")
allVec <- c(chldMatern, metab, environ, subAbuse, diet)
#catList <- list(chldMatern, metab, diet, environ, subAbuse)
# thisFile <- "IHME-GBD_2019_LEV2r1990-2019countries.csv"
# thisFilePath <- paste0(dataFolder, thisFile)
# dfRiskPCA <- read.csv(thisFilePath, stringsAsFactors = F)
dfrPCA <- dfRiskRaw %>% subset(rei %in% allVec &
                                 age == thisAge &
                                 metric == thisMetric &
                                 year == thisYr) %>%
  select(c("location", "rei",  "upper"))
colnames(dfrPCA)[3] <- "val"
colnames(dfrPCA)[2] <- "Issue"
colnames(dfrPCA)[1] <- "area"
dfrPCA$area <- gsub("Turkey", "Türkiye", dfrPCA$area)
dfrPCA <- subset(dfrPCA, !(area %in% c("Taiwan (Province of China)")))
# setdiff(unique(dfGDP$area), unique(dfRiskRaw$location))
# unique(dfGDP$area)[order(unique(dfGDP$area))]
# unique(dfRiskRaw$location)[order(unique(dfRiskRaw$location))]
# dfGDPmerge <- subset(dfGDP, year == thisYr)[c(1, 3)]
# colnames(dfGDPmerge)[2] <- "val"
# dfGDPmerge$Issue <- "GDP / capita (USD 2015 prices)"
# dfGDPmerge <- dfGDPmerge[, colnames(dfrPCA)]
# rmAreas <- setdiff(unique(dfGDPmerge$area), unique(dfrPCA$area))
# dfGDPmerge <- dfGDPmerge %>% subset(!(area %in% rmAreas))
dfSDImerge <- dfSDI %>% subset(year == thisYr) %>% select(-year)
dfPopYoungMerge <- dfPopYoung %>% subset(year == thisYr) %>%
  select(-year)
dfSocioD <- dfSDImerge %>% merge(dfPopYoungMerge) %>%
  gather(Issue, val, sdi:pop14)
dfSocioD <- dfSocioD[, colnames(dfrPCA)]
keepAreas <- intersect(dfSocioD$area, dfrPCA$area)
dfSocioD <- dfSocioD %>% subset(area %in% keepAreas)
dfPCA <- as.data.frame(rbind(dfrPCA, dfSocioD))
#dfPCA[which(is.infinite(dfPCA$val)),]
X <- dfPCA %>% spread(Issue, val)
naFn <- function(x){u <- sum(is.na(x)); return(u)}; apply(X[, -1], 2, naFn)
#indNA <- which(is.na(X$`GDP / capita (USD 2015 prices)`)); X$area[indNA]
indNA <- which(is.na(X$pop14)); X$area[indNA]
X <- X[-indNA, ]
zerFn <- function(x){u <- sum(x < 0); return(u)}; apply(X[, -1], 2, zerFn)
indZer <- which(X$`Low temperature` < 0)
X <- X[-indZer, ]
X[, -1] <- log(X[, -1])
nanFn <- function(x){u <- sum(is.nan(x)); return(u)}; apply(X[,-1], 2, nanFn)
infFn <- function(x){u <- sum(is.infinite(x)); return(u)}; apply(X[, -1], 2, infFn)
indInf <- which(is.infinite(X$`High temperature`))
X <- X[-indInf, ]
areaVecPCA_ihme <- X$area
X <- X %>%
  select(-"area") %>% as.matrix() %>%
  apply(2, scale)
#dim(X)
eigOut <- X %>% cor() %>% eigen()
eigVals <- eigOut$values
cumFracExpld <- cumsum(eigVals) / sum(eigVals)
cutOff <- which(cumFracExpld > 0.75)[1]
P <- eigOut$vectors[, 1:cutOff]
G <- diag(eigVals[1:cutOff])
L <- P %*% sqrt(G)
dfBar <- L %>% as.data.frame()
colnames(dfBar) <- paste("PC", 1:cutOff)
gatherCols <- colnames(dfBar)
dfBar$var <- colnames(X)
dfBar <- dfBar %>% gather_("PC", "val", gatherCols)
dfPlot <- dfBar
dfPlot$Type <- NA; u <- dfPlot$var
dfPlot$Type[which(u %in% diet)] <- "Dietary risks"
dfPlot$Type[which(u %in% chldMatern)] <- "Child and maternal malnutrition"
dfPlot$Type[which(u %in% metab)] <- "Metabolic risks"
dfPlot$Type[which(u %in% environ)] <- "Environmental risks"
dfPlot$Type[which(u %in% subAbuse)] <- "Substance abuse"
dfPlot$Type[grep("Socio-demographic", u)] <- "SDI"
dfPlot$Type <- factor(dfPlot$Type, levels = unique(dfPlot$Type))
dfPlot$var <- factor(dfPlot$var, levels = unique(dfPlot$var))

gg <- ggplot(dfPlot, aes(x = val, y = var, fill = Type))
gg <- gg + geom_bar(stat = "identity", position = "dodge")
gg <- gg + geom_hline(yintercept = 0, color = "red")
#gg <- gg + coord_flip()
gg <- gg + facet_wrap(~PC, nrow = 1)
gg
Lrot <- varimax(L)[[1]]
Lrot <- matrix(as.numeric(Lrot),
               attributes(Lrot)$dim,
               dimnames = attributes(Lrot)$dimnames)
R <- varimax(L)[[2]]
R <- matrix(as.numeric(R),
            attributes(R)$dim,
            dimnames = attributes(R)$dimnames)
dfBar <- Lrot %>% as.data.frame()
fracExpld <- eigVals / sum(eigVals)
colnames(dfBar) <- paste0("rot. PC ", 1:cutOff, "\n(", round(fracExpld[1:cutOff], 2), ")")
gatherCols <- colnames(dfBar)
dfBar$var <- colnames(X)
#dfBar$`rot. PC 2\n(0.16)` <- -dfBar$`rot. PC 2\n(0.16)`
dfBar <- dfBar %>% gather_("PC", "val", gatherCols)
dfPlot <- dfBar
dfPlot$Type <- NA; u <- dfPlot$var
dfPlot$Type[which(u %in% diet)] <- "Dietary risks"
dfPlot$Type[which(u %in% chldMatern)] <- "Child and maternal malnutrition"
dfPlot$Type[which(u %in% metab)] <- "Metabolic risks"
dfPlot$Type[which(u %in% environ)] <- "Environmental risks"
dfPlot$Type[which(u %in% subAbuse)] <- "Substance abuse"
dfPlot$Type[which(u %in% c("sdi", "pop14"))] <- "Socio-demog"
dfPlot$Type <- factor(dfPlot$Type, levels = unique(dfPlot$Type))
dfPlot$var <- factor(dfPlot$var, levels = unique(dfPlot$var))
gg <- ggplot(dfPlot, aes(x = val, y = var, fill = Type))
gg <- gg + geom_bar(stat = "identity", position = "dodge", color = "black")
gg <- gg + geom_hline(yintercept = 0, color = "red")
gg <- gg + facet_wrap(~PC, nrow = 1)
gg <- gg + labs(x = "Correlation with PC")
gg <- gg + theme_bw()
gg <- gg + theme(legend.position = "top",
                 legend.title = element_blank(),
                 legend.text = element_text(size = axisTextSize),
                 axis.text = element_text(size = axisTextSize),
                 axis.title.y = element_blank(),
                 axis.title.x = element_text(size = axisTitleSize))
gg
saveFile <- "riskFactrLoadings.png"
saveTo <- paste0(graphicsFolder, saveFile)
ggsave(saveTo, width = 7, height = 6)
#---
dfPC <- as.data.frame(X %*% P %*% R)
dfPC$area <- areaVecPCA_ihme 
dfPC$Region <- NA
dfPC$Region[which(dfPC$area %in% ctyInAsansCWA)] <- "Asia\nexcl. C&W Asia"
dfPC$Region[which(dfPC$area %in% ctyInNAMEURAUSNZ)] <- "Eur. / N. Amer. /\nAus. / NZ"
dfPC$Region[which(dfPC$area %in% ctyInLAC)] <- "Lat. Amer. /\nCaribbean"
dfPC$Region[which(dfPC$area %in% ctyInCWANA)] <- "CWANA"
dfPC$Region[which(dfPC$area %in% ctyInSSA)] <- "Africa South\nof the Sahara"
unique(dfPC$area[which(is.na(dfPC$Region))])
colnames(dfPC) <- gsub("V", "PC", colnames(dfPC))
gg <- ggplot(dfPC, aes(x = PC1, y = PC2, group = Region, color = Region))
gg <- gg + geom_point()
gg <- gg + geom_text(aes(label = area), size = 2, vjust = 1)
gg <- gg + theme_bw()
gg
saveFile <- "riskFactrClust.png"
saveTo <- paste0(graphicsFolder, saveFile)
ggsave(saveTo, width = 5, height = 3)
#---
k2 <- kmeans(X, centers = 2, nstart = 25)
fviz_cluster(k2, data = X)
#===============================================================
#===============================================================


dfFBSmn <- dfFBS %>% subset(element %in%
                              c("Fat supply quantity (g/capita/day)",
                                "Protein supply quantity (g/capita/day)",
                                "Food supply (kcal/capita/day)")) %>%
  select(c(area, year, Population, element, `Grand Total`)) %>%
  rename(value = `Grand Total`)
# Convert g to kcal
dfFBSmn$value[grep("Protein", dfFBSmn$element)] <- 4 *
  dfFBSmn$value[grep("Protein", dfFBSmn$element)]
dfFBSmn$value[grep("Fat", dfFBSmn$element)] <- 9 *
  dfFBSmn$value[grep("Fat", dfFBSmn$element)]
dfFBSmn$element[grep("Protein", dfFBSmn$element)] <- "Protein supply (kcal/capita/day)"
dfFBSmn$element[grep("Fat", dfFBSmn$element)] <- "Fat supply (kcal/capita/day)"
# Calculate carb kcal
dfFBSmn <- dfFBSmn %>% spread(element, value) %>%
  mutate(`Carb supply (kcal/capita/day)` = `Food supply (kcal/capita/day)` -
           `Protein supply (kcal/capita/day)` - `Fat supply (kcal/capita/day)`)
# # Create % of diet variable
# dfPop <- dfFBSmacNut %>% select("area", "year", "Population (1000 persons)")
# dfFBSmacNutPct <- dfFBSmacNut %>% select(-c("Food supply (kcal/capita/day)",
#                                             "Population (1000 persons)"))
# dfFBStot <- dfFBS %>% subset(item == "Grand Total" &
#                                element == "food_supply__kcal_capita_day_") %>%
#   select(c("area", "year", "value")) %>% rename(total = value)
# gathercols <- colnames(dfFBSmacNutPct)[3:ncol(dfFBSmacNutPct)]
# dfFBSmacNutPct <- dfFBSmacNutPct %>% gather_("item", "value", gathercols)
# dfFBSmacNutPct$item[grep("Protein", dfFBSmacNutPct$item)] <- "Protein supply (%)"
# dfFBSmacNutPct$item[grep("Fat", dfFBSmacNutPct$item)] <- "Fat supply (%)"
# dfFBSmacNutPct$item[grep("Carb", dfFBSmacNutPct$item)] <- "Carb supply (%)"
# dfFBSmacNutPct <- dfFBSmacNutPct %>% merge(dfFBStot) %>%
#   mutate(`Diet share (%)` = round(100 * value / total, 2)) %>%
#   select(-c("value", "total")) %>%
#   spread(item, `Diet share (%)`) %>%
#   merge(dfPop)
#=======================================================================
# # SDG data
# dfRaw <- get_faostat_bulk(code = "SDGB", data_folder = dataFolder)
# keepCols <- c("area", "year", "item", "element", "unit", "value")
# rmRows <- which(dfRaw$value == "NaN")
# dfSDGB <- dfRaw[-rmRows, keepCols]
# dfSDGB <- dfSDGB %>%
#   #subset(element == "value__2017_constant_prices_") %>%
#   subset(element == "value") %>%
#   #  select(-c("element", "unit")) %>%
#   group_by(area, item) %>%
#   mutate(nYrs = length(value)) %>% as.data.frame()
# max(dfSDGB$nYrs)
# keepRows <- which(dfSDGB$nYrs >= (max(dfSDGB$nYrs) - 15))
# dfSDGB <- dfSDGB[keepRows, ]
# unique(dfSDGB$item)
# # dfSDGB <- dfSDGB %>% subset(item == )
# dfSDGB$nYrs <- NULL
#=========================================================
# FBS graphics and PCA
# u <- dfFBScommod$area
# unique(u[grep("Central", u)])
# Graphic: Who is eating what? Are existing studies leaving out some
# important food categories?
SSAvec <- c("Southern Africa", "Western Africa", "Middle Africa",
            "Eastern Africa")
LACvec <- c("Caribbean", "South America", "Central America")
CWANAvec <- c("Central Asia", "Northern Africa", "Western Asia")
SEasAusNZvec <- c("South-eastern Asia", "Australia and New Zealand")
areaVec <- c("Europe", "Northern America", "Eastern Asia",
             "Southern Asia", SSAvec, LACvec, CWANAvec,
             SEasAusNZvec, "World")
#dfPop <- dfFBScommod[, c("area", "year", "Population (1000 persons)")]
#gathercols <- colnames(dfFBScommod)[-c(1, 2, which(colnames(dfFBScommod) == "Population (1000 persons)"))]
indPop <- which(colnames(dfFBScom) == "Population")
gathercols <- colnames(dfFBScom)[-c(1:2, indPop)]
#---
dfPlot <- dfFBScom %>% subset(area %in% areaVec) %>%
  gather_("item", "val", gathercols) %>%
  mutate(val = val * Population * 1000)
dfWorld <- dfPlot %>% subset(area == "World")
dfPlot <- dfPlot %>% subset(area != "World")
dfPlot$area[which(dfPlot$area %in% SSAvec)] <- "SSA"
dfPlot$area[which(dfPlot$area %in% LACvec)] <- "LAC"
dfPlot$area[which(dfPlot$area %in% CWANAvec)] <- "CWANA"
dfPlot$area[which(dfPlot$area %in% SEasAusNZvec)] <- "SE Asia Aus & NZ"
dfPlot <- dfPlot %>% group_by(area, year, item) %>%
  summarise_all(sum, na.rm = T) %>%
  mutate(val = val / (Population * 1000)) %>%
  select(-Population) %>%
  as.data.frame()
dfPlot$year <- as.integer(dfPlot$year)
# dfWorld$year <- as.integer(dfWorld$year)
# dfWorld$val <- dfWorld$val / (dfWorld$Population * 1000)
# dfWorld$Population <- NULL
# gg <- ggplot(dfWorld, aes(x = year, y = val, fill = item))
# gg <- gg + geom_area()
# gg <- gg + scale_fill_manual(values = theseColors)
# gg <- gg + labs(y = "kcal/capita/day")
# gg <- gg + theme_bw()
# gg
theseColors <- randomcoloR::distinctColorPalette(length(unique(dfPlot$item)))
gg <- ggplot(dfPlot, aes(x = year, y = val, fill = reorder(item, val)))
gg <- gg + geom_area(position = "stack")
gg <- gg + scale_fill_manual(values = theseColors)
gg <- gg + facet_wrap(~area, nrow = 2)
gg <- gg + labs(y = "kcal/capita/day")
gg <- gg + theme_bw()
gg <- gg + theme(axis.title.x = element_blank(),
                 axis.title.y = element_text(size = axisTitleSize),
                 axis.text = element_text(size = axisTextSize),
                 strip.text = element_text(size = facetTitleSize),
                 legend.position = "top",
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendTextSize))
gg
thisPic <- "Diet shares mag2.png"
thisPicPath <- paste0(graphicsFolder, thisPic)
ggsave(thisPicPath, width = 7, height = 5)
#---
dfPlot2 <- dfPlot %>% group_by(area, year) %>%
  mutate(share = val / sum(val))
gg <- ggplot(dfPlot2, aes(x = year, y = share, fill = reorder(item, share)))
gg <- gg + geom_area(position = "stack")
gg <- gg + scale_fill_manual(values = theseColors)
gg <- gg + facet_wrap(~area, nrow = 2)
gg <- gg + labs(y = "Share of daily diet/capita")
gg <- gg + theme_bw()
gg <- gg + theme(axis.title.x = element_blank(),
                 axis.title.y = element_text(size = axisTitleSize),
                 axis.text = element_text(size = axisTextSize),
                 strip.text = element_text(size = facetTitleSize),
                 legend.position = "top",
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendTextSize))
gg
thisPic <- "Diet shares frac.png"
thisPicPath <- paste0(graphicsFolder, thisPic)
ggsave(thisPicPath, width = 7, height = 5)
#---
dfPlot3 <- dfPlot2 %>%
  subset(area == "Europe" &
           year == 2019)
dfPlot3 <- dfPlot3[order(dfPlot3$val), ]
dfPlot3$rank <- seq(1, nrow(dfPlot3))
dfPlot3$rank <- log(dfPlot3$rank)
# dfPlot3$share <- log(dfPlot3$share)
dfPlot3$val <- log(dfPlot3$val)
mod <- lm(val~rank, dfPlot3)
summ(mod)
gg <- ggplot(dfPlot3, aes(x = rank, y = val))
gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
gg <- gg + geom_point()
gg <- gg + geom_text_repel(aes(label = item), size = 2.5)
gg
#---
# FBS PCA (just like the DALY PCA but FBS)
dfFBSpca <- dfFBS %>% merge(dfGDP)
yrVec <- unique(dfFBSpca$year)
# listDf <- list()
# for(i in 1:length(yrVec)){
#   thisYr <- yrVec[i]
thisYr <- 2019
X <- dfFBSpca %>%
  subset(year == thisYr & area %in% areaVecPCA_ihme &
           element == "Food supply (kcal/capita/day)") %>%
  select(-c(year, element, Population, `Grand Total`))
# setdiff(areaVecPCA, dfFBSpca$area)
# u <- dfFBSpca$area
# u[grep("Eritrea", u)]
# v <- areaVecPCA
# v[grep("Singapore", v)]
naFn <- function(x){u <- sum(is.na(x)); return(u)}
o <- apply(X[, -1], 2, naFn); colNA <- which(o > 0);table(o)
if(length(colNA) != 0){
  indRm <- NULL
  for(i in 1:length(colNA)){indRm <- c(indRm, which(is.na(X[, colNA[i] + 1])))}
  print(X$area[indRm])
  X <- X[-unique(indRm), ]
}
X$`Alcoholic Beverages` <- NULL
X[, -1] <- log(X[, -1])
indRm <- which(is.infinite(X$Pulses))
if(length(indRm) != 0){
  X <- X[-indRm, ]
}
areaVecPCA <- X$area
X <- X %>%
  select(-"area") %>% as.matrix() %>% scale(scale = F)
#  apply(2, scale)
eigOut <- X %>% cor() %>% eigen()
eigVals <- eigOut$values
cumFracExpld <- cumsum(eigVals) / sum(eigVals)
cutOff <- which(cumFracExpld > 0.8)[1]
P <- eigOut$vectors[, 1:cutOff]
G <- diag(eigVals[1:cutOff])
L <- P %*% sqrt(G)
# indLook <- which(abs(L[, 2]) == max(abs(L[, 2])))
# if(L[indLook, 2] < 0){L <- -L}
dfBar <- L %>% as.data.frame()
colnames(dfBar) <- paste0("PC ", 1:cutOff, "\n(", round(fracExpld[1:cutOff], 2), ")")
gatherCols <- colnames(dfBar)
dfBar$var <- colnames(X)
dfBar <- dfBar %>% gather_("PC", "val", gatherCols)
dfPlot <- dfBar
gg <- ggplot(dfPlot, aes(x = val, y = var))
gg <- gg + geom_bar(stat = "identity", position = "dodge")
gg <- gg + geom_vline(xintercept = 0, color = "red")
#gg <- gg + coord_flip()
gg <- gg + facet_wrap(~PC, nrow = 1)
gg
Lrot <- varimax(L)[[1]]
Lrot <- matrix(as.numeric(Lrot),
               attributes(Lrot)$dim,
               dimnames = attributes(Lrot)$dimnames)
R <- varimax(L)[[2]]
R <- matrix(as.numeric(R),
            attributes(R)$dim,
            dimnames = attributes(R)$dimnames)
dfBar <- Lrot %>% as.data.frame()
fracExpld <- eigVals / sum(eigVals)
colnames(dfBar) <- paste0("rot. PC ", 1:cutOff, "\n(", round(fracExpld[1:cutOff], 2), ")")
gatherCols <- colnames(dfBar)
dfBar$var <- colnames(X)
#dfBar$`rot. PC 2\n(0.16)` <- -dfBar$`rot. PC 2\n(0.16)`
dfBar <- dfBar %>% gather_("PC", "val", gatherCols)
dfPlot <- dfBar
# dfPlot$Type <- NA; u <- dfPlot$var
# dfPlot$Type[which(u %in% diet)] <- "Dietary risks"
# dfPlot$Type[which(u %in% chldMatern)] <- "Child and maternal malnutrition"
# dfPlot$Type[which(u %in% metab)] <- "Metabolic risks"
# dfPlot$Type[which(u %in% environ)] <- "Environmental risks"
# dfPlot$Type[which(u %in% subAbuse)] <- "Substance abuse"
# dfPlot$Type[grep("GDP", u)] <- "GDP"
# dfPlot$Type <- factor(dfPlot$Type, levels = unique(dfPlot$Type))
# dfPlot$var <- factor(dfPlot$var, levels = unique(dfPlot$var))
gg <- ggplot(dfPlot, aes(x = val, y = var))
gg <- gg + geom_bar(stat = "identity", position = "dodge", color = "black")
gg <- gg + geom_vline(xintercept = 0, color = "red")
gg <- gg + facet_wrap(~PC, nrow = 1)
gg <- gg + labs(x = "Correlation with PC")
gg <- gg + theme_bw()
gg <- gg + theme(legend.position = "top",
                 legend.title = element_blank(),
                 legend.text = element_text(size = axisTextSize),
                 axis.text = element_text(size = axisTextSize),
                 axis.title.y = element_blank(),
                 axis.title.x = element_text(size = axisTitleSize))
gg

saveFile <- "dietLoadings.png"
saveTo <- paste0(graphicsFolder, saveFile)
ggsave(saveTo, width = 7, height = 4)

#---
dfPC <- as.data.frame(X %*% P %*% R) %>% select(V1, V2)
dfPC$area <- areaVecPCA
dfPC$Region <- NA
dfPC$Region[which(dfPC$area %in% ctyInAsansCWA)] <- "Asia\nexcl. C&W Asia"
dfPC$Region[which(dfPC$area %in% ctyInNAMEURAUSNZ)] <- "Eur. / N. Amer. /\nAus. / NZ"
dfPC$Region[which(dfPC$area %in% ctyInLAC)] <- "Lat. Amer. /\nCaribbean"
dfPC$Region[which(dfPC$area %in% ctyInCWANA)] <- "CWANA"
dfPC$Region[which(dfPC$area %in% ctyInSSA)] <- "Africa South\nof the Sahara"
unique(dfPC$area[which(is.na(dfPC$Region))])
colnames(dfPC) <- gsub("V", "PC", colnames(dfPC))
# library(cluster)
# row.names(dfPC) <- dfPC$area
# pamOut <- pam(dfPC[, c(1, 2)], k = 2)
# p <- fviz_cluster(pamOut, labelsize = 5)
# hull_data <-  p$data %>%
#   group_by(cluster) %>%
#   slice(chull(x, y)) %>%
#   rename(area = name,
#          PC1 = x, PC2 = y)# %>%
#  as.data.frame()
#hull_data$area <- as.character(hull_data$area)
#hull_data$cluster <- as.numeric(hull_data$cluster)
gg <- ggplot(dfPC, aes(x = PC1, y = PC2, group = Region, color = Region))
gg <- gg + geom_point()
gg <- gg + geom_text(aes(label = area), size = 2, vjust = 1)
gg <- gg + theme_bw()
gg
saveFile <- "dietClust.png"
saveTo <- paste0(graphicsFolder, saveFile)
ggsave(saveTo, width = 5, height = 3)

# gg <- ggplot(dfPlot, aes(x = PC1, y = PC2))
# gg <- gg + geom_point()
# gg <- gg + geom_text(aes)
# #gg <- gg + geom_polygon(data = hull_data, alpha = 0.5, aes(fill = cluster, linetype=cluster))
# gg
# clustVec <- p$data
# clustVec <- as.numeric(clustVec$cluster)
# sum(clustVec == 2)
# 
# p <- fviz_cluster(pamOut, labelsize = 5)
# p <- p + geom_density_2d(colour=1, bins=6)
# p <- p + theme_bw()
# p
# 
# dfClustKey <- data.frame(area = areaVecPCA,
#                          clust = clustVec)
# library(rworldmap)
# dfMap <- data.frame(area = dfPC$area, cluster = clustVec)
# clustMap <- joinCountryData2Map(dfMap, 
#                                   joinCode = "NAME",
#                                   nameJoinColumn = "area")
# map <- mapCountryData(clustMap, 
#                       nameColumnToPlot="cluster",
#                       catMethod = "categorical",
#                       missingCountryCol = gray(.8),
#                       addLegend = F,
#                       colourPalette = c("deepskyblue4","aquamarine4"),
#                       borderCol = "black",
#                       mapTitle = "pam clusters")


# dfPC$year <- thisYr
# listDf[[as.character(thisYr)]] <- dfPC
# print(nrow(dfPC))
# print(thisYr)
# }
# dfPC <- as.data.frame(do.call(rbind, listDf))
# #---
# library(gifski)
# gg <- ggplot(dfPC, aes(x = V1, y = V2, color = Region))
# gg <- gg + geom_point()
# gg <- gg + geom_density_2d(colour=1, bins=6)
# gg <- gg + geom_text(aes(label = area), size = 2)
# gg <- gg + transition_time(year) +
#   labs(title = "Year: {frame_time}")
# animate(gg, renderer = gifski_renderer())
# #gg
# thisAnimPath <- paste0(graphicsFolder, "PCfbs.gif")
# anim_save(thisAnimPath, gg)
#========================================================================
# Model year
#thisYr <- 2019
# Model 1
# Hunger ~ Commodity
#-----------------------------------------------------------------
#export_summs(modChr, modChr2, modHid, modHid2, modOve, model.names = c("Chronic\nhunger 1", "Chronic\nhunger 2", "Hidden\nhunger 1", "Hidden\nhunger 2", "Overnutrition"), to.file = "docx", file.name = paste0(graphicsFolder, "modsCtyFE.docx"))
#-----------------------------------------------------------------

#





























dfModX <- dfModChr
dfX <- dfModX %>% select(`DALYs/100,000 capita`, Cereals)
dfX <- dfModX %>% select(`DALYs/100,000 capita`, `Animal Products`)
dfX <- dfModX %>% select(`DALYs/100,000 capita`, `Starchy Roots`)
dfX <- dfModX %>% select(`DALYs/100,000 capita`, Pulses)
dfX <- dfModX %>% select(`DALYs/100,000 capita`, Oilcrops)
dfX <- dfModX %>% select(`DALYs/100,000 capita`, `F&V`)
dfX <- dfModX %>% select(`DALYs/100,000 capita`, Cereals,
                         `F&V`, `Animal Products`,
                         Pulses, `Oilcrops`, `Starchy Roots`)
mod <- lm(`DALYs/100,000 capita` ~., dfX)
summ(mod)
plot(mod$fitted.values, mod$residuals)
#---
# dfPca <- dfMod %>% select(-c("NutDef (DALYs)"))
# isNA <- function(x){
#   nNA <- sum(is.na(x) > 0)
#   return(nNA)
# }
# naLook <- apply(dfPca, 2, isNA)
# rmRows <- which(is.na(dfPca$Pulses))
# dfPca <- dfPca[-rmRows, ]
# p <- princomp(dfPca)
# 
# res <- FactoMineR::PCA(dfPca, ncp = 5, graph = T)
# eigvals <- as.data.frame(res$eig)$eigenvalue
# mat_loads <- res$var$coord
# mat_loads_rot <- varimax(mat_loads)[[1]]
# mat_eigvecs <- mat_loads %*% diag(1 / sqrt(eigvals))
# 
# p$loadings
#---
# PC models
thisDfMod <- dfModChr
X <- thisDfMod %>%
  select(-"DALYs/100,000 capita") %>%
  as.matrix() %>%
  apply(2, scale)
eigOut <- X %>% cor() %>% eigen()
eigVals <- eigOut$values
fracExpld <- cumsum(eigVals) / sum(eigVals)
cutOff <- which(fracExpld > 0.95)[1]
#cutOff <- ncol(X)
P <- eigOut$vectors[, 1:cutOff]
G <- diag(eigVals[1:cutOff])
L <- P %*% sqrt(G)
Lrot <- varimax(L)[[1]]
Lrot <- matrix(as.numeric(Lrot),
               attributes(Lrot)$dim,
               dimnames = attributes(Lrot)$dimnames)
R <- varimax(L)[[2]]
R <- matrix(as.numeric(R),
            attributes(R)$dim,
            dimnames = attributes(R)$dimnames)
dfPC <- as.data.frame(X %*% P %*% R)
#dfModPC <- as.data.frame(cbind(dfModHid[, "DALYs/capita"], dfPC))
dfModPC <- as.data.frame(cbind(thisDfMod[, "DALYs/100,000 capita"], dfPC))
colnames(dfModPC) <- c("DALYs/100,000 capita", paste("PC", 1:cutOff))
dfModPC$`DALYs/100,000 capita` <- scale(dfModPC$`DALYs/100,000 capita`)
#dfModPC <- dfModPC[, -5]
mod <- lm(`DALYs/100,000 capita` ~.-1, dfModPC)
summ(mod)
car::vif(mod)
plot(mod$fitted.values, mod$residuals)
pVals <- summary(mod)$coefficients[, 4]
colRm <- which(pVals > 0.04) + 1
dfModPC <- dfModPC[, -colRm]
#export_summs(mod, model.names = c("Hidden\nHunger"), to.file = "docx", file.name = paste0(graphicsFolder, "mod2Results.docx"))
#---
# Plot loadings
thesePCs <- as.numeric(gsub("\\D", "", colnames(dfModPC)[-1]))
dfBar <- Lrot[, thesePCs] %>% as.data.frame()
colnames(dfBar) <- paste("rot. PC", thesePCs)
gatherCols <- colnames(dfBar)
dfBar$var <- colnames(X)
dfBar <- dfBar %>% gather_("PC", "val", gatherCols)
gg <- ggplot(dfBar, aes(x = var, y = val))
gg <- gg + geom_bar(stat = "identity")
gg <- gg + geom_hline(yintercept = 0, color = "red")
gg <- gg + coord_flip()
gg <- gg + facet_wrap(~PC, nrow = 1)
gg
#---
# dfPCrot <- as.data.frame(as.matrix(dfPC) %*% R)
# gg <- ggplot(dfPCrot, aes(x = V1, y = V4))
# gg <- gg + geom_point()
# gg
#========================================================================
# Model 2
# Hunger ~ Macronutrient
dfMod <- merge(dfDaly, dfFBSmacNut) %>%
  #merge(dfFBScommod) %>%
  #merge(dfGDP) %>%
  subset(year %in% thisYr) %>%
  rename(`DALYs/100,000 capita` = val) %>%
  # mutate(`DALYs/capita` = 1000 * val / `Population (1000 persons)`) %>%
  #mutate(`country-year` = paste0(area, year)) %>%
  select(-c("year", "Population (1000 persons)",
            "Protein supply (kcal/capita/day)",
            #"Carb supply (kcal/capita/day)",
            #"Fat supply (kcal/capita/day)",
            #"GDP / capita (USD 2015 prices)",
            "Food supply (kcal/capita/day)"))
# dfMod <- merge(dfHidHung, dfFBSmacNut) %>% subset(year == thisYr) %>%
#   select(-c("area", "year", #"GDP / capita (USD 2015 prices)",
#             #"Fat supply (kcal/capita/day)",
#             "Protein supply (kcal/capita/day)",
#             "Food supply (kcal/capita/day)"
#             ))
#select(-c("area", "year"))
#dfMod$`NutDef (DALYs) per 1000 capita` <- log(dfMod$`NutDef (DALYs) per 1000 capita`)
#---
indRm <- which(dfMod$area %in% c("Mali", "South Sudan"))
dfMod <- dfMod[-indRm, ]
dfMod$area <- NULL
#---
dfMod[, -2] <- as.data.frame(apply(dfMod[, -2], 2, log))
colnames(dfMod)
#dfMod$x <- dfMod$`Fat supply (kcal/capita/day)` * dfMod$`Carb supply (kcal/capita/day)`
#dfMod$`DALYs/capita` <- log(dfMod$`DALYs/capita`)
#dfMod <- as.data.frame(apply(dfMod, 2, scale))
infNanLook <- apply(dfMod, 2, isInfNan)
infNanLook
rmRows <- which(is.infinite(dfMod$Pulses))
if(length(rmRows) != 0){dfMod <- dfMod[-rmRows, ]}
dfModHid <- dfMod %>% subset(Cat == "Hid") %>% select (-"Cat")
dfModChr <- dfMod %>% subset(Cat == "Chr") %>% select (-"Cat")
dfModOve <- dfMod %>% subset(Cat == "OverDev") %>% select (-"Cat")
#---
dfModHid[, -1] <- as.data.frame(apply(dfModHid[, -1], 2, refFn))
mod <- lm(`DALYs/100,000 capita` ~., dfModHid)
summ(mod)
#summary(mod)
car::vif(mod)
plot(mod$fitted.values, mod$residuals)
modHid <- mod
#---
dfModChr[, -1] <- as.data.frame(apply(dfModChr[, -1], 2, refFn))
mod <- lm(`DALYs/100,000 capita` ~., dfModChr)
summ(mod)
#summary(mod)
car::vif(mod)
plot(mod$fitted.values, mod$residuals)
modChr <- mod
#---
dfModOve[, -1] <- as.data.frame(apply(dfModOve[, -1], 2, refFn))
mod <- lm(`DALYs/100,000 capita` ~., dfModOve)
summ(mod)
#summary(mod)
car::vif(mod)
plot(mod$fitted.values, mod$residuals)
modOve <- mod
#---
export_summs(modChr, modHid, modOve, model.names = c("Chronic\nHunger", "Hidden\nHunger", "Over-dev"), to.file = "docx", file.name = paste0(graphicsFolder, "mod3Results.docx"))
#-----------------------------------------------------------------------
# Fat-carb frontier
dfMod <- dfFBSmacNutPct %>% subset(year == thisYr) %>%
  select(c("Carb supply (%)",
           "Fat supply (%)"))
mod <- lm(`Carb supply (%)`~., dfMod)
summary(mod)
plot(mod$fitted.values, mod$residuals)
dfPlot <- merge(dfFBSmacNutPct, dfGDP) %>% subset(year %in% thisYr) %>%
  select(c("Carb supply (%)",
           "Fat supply (%)",
           "GDP / capita (USD 2015 prices)"))

m <- round(mod$coefficients[2], 3)
b <- round(mod$coefficients[1], 3)
dfOut <- as.data.frame(broom::glance(mod))
adjR2 <- round(dfOut$adj.r.squared, 2)
sampleSize <- df.residual(mod)
thisSubtitle <- paste0(thisYr, ", N = ", sampleSize, ", Adj. R-squared = ", adjR2, "\nSlope = ", m, ", Y intercept = ", b)
#Plot
labelSize <- 2.5
smallLabelSize <- 2
titleSize <- 7
subtitleSize <- 7
legendTextSize <- 6
axisTextSize <- 6
axisTitleSize <- 7
facetTitleSize <- 7

gg <- ggplot(dfPlot, aes(x = `Fat supply (%)`,
                         y = `Carb supply (%)`,
                         # group = Region, fill = Region,
                         # shape = Region,
                         size = `GDP / capita (USD 2015 prices)`))#,
#label = label_these))
#gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
gg <- gg + geom_point(alpha = 0.6, color = "black", stroke = 0.5)
# gg <- gg + scale_fill_manual(values = color_vec)
# gg <- gg + scale_shape_manual(values = shape_vec)
gg <- gg + labs(title = "The fat-carb frontier", subtitle = thisSubtitle)
#gg <- gg + geom_text_repel(color = "black", size = label_size)
gg <- gg + theme(legend.position = "none",
                 legend.spacing.x = unit(0.25, 'cm'),
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendTextSize),
                 plot.title = element_text(size = titleSize),
                 plot.subtitle = element_text(size = subtitleSize),
                 axis.title = element_text(size = axisTitleSize),
                 axis.text = element_text(size = axisTextSize))
gg <- gg + guides(fill = guide_legend(nrow = 2, byrow = T, override.aes = list(linetype = 0)),
                  color = guide_legend(override.aes = list(linetype = 0)),
                  size = F)
gg_fatcarb <- gg
#ggsave("FatCarbFrontier.png", width = 4, height = 4, units = "in")

















df <- dfRaw %>%
  select(c("location", "rei", "val")) %>%
  group_by(location, rei) %>%
  summarise(val = sum(val)) %>%
  as.data.frame()



hidHungVec <- c("Other nutritional deficiencies",
                "Iodine deficiency",
                "Vitamin A deficiency",          
                "Dietary iron deficiency")
chrHungVec <- c("Protein-energy malnutrition")
dfHung <- dfRaw %>% subset(metric == "Metric" &
                             measure == "DALYs (Disability-Adjusted Life Years)" &
                             cause %in% c(#"Nutritional deficiencies",
                               hidHungVec, chrHungVec)) %>%
  select(c("location", "year", "cause", "val")) %>%
  rename(area = location)
dfHidHung <- dfHung %>% subset(cause %in% hidHungVec) %>%
  group_by(area, year) %>%
  summarise(val = sum(val, na.rm = T)) %>%
  mutate(item = "Hid. Hunger") %>%
  as.data.frame()
dfHung <- dfHung %>% subset(cause %in% chrHungVec) %>%
  #  group_by(area, year) %>%
  #  summarise(val = sum(val, na.rm = T))
  mutate(item = "Chr. Hunger") %>%
  as.data.frame() %>%
  select(-"cause") %>% rbind(dfHidHung) %>%
  as.data.frame()
#---------------------------------------------------------------------
#==============================================================
#==============================================================
#==============================================================
#GRAPHICS
#==============================================================
# dataFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/DALYs/Hunger DALY data/"
# theseFiles <- paste0(dataFolder, c("IHME-GBD_2019_ALLc1990-2019regions-"), c(1:6), ".csv")
# listDf <- lapply(theseFiles, read.csv)
# dfCauseRegRaw <- as.data.frame(do.call(rbind, listDf))
# theseFiles <- paste0(dataFolder, c("IHME-GBD_2019_ALLr1990-2019regions-"), c(1:2), ".csv")
# listDf <- lapply(theseFiles, read.csv)
# dfRiskRegRaw <- as.data.frame(do.call(rbind, listDf))
# #---------------------------------------------------------------
# # From risk factor data get zinc, iron, vit. A deficiency DALYs
# # u <- dfRiskRawReg$rei
# # unique(u[grep("deficiency", u)])
# keepThese <- c("Vitamin A deficiency",
#                "Zinc deficiency",
#                "Iron deficiency",
#                "Child underweight")
# dfRiskReg <- dfRiskRegRaw %>% subset(rei %in% keepThese) %>%# &
#                                  #age == "All ages" &
#                                  #metric == "Rate") %>%
#   select(c("location", "rei", "year", "age", "metric", "val"))
# 
# dfUnderWgtU5Reg <- dfRiskReg %>%
#   subset(rei == "Child underweight" &
#            age == "<5 years" &
#            metric == "Number") %>%
#   rename(valU5 = val) %>%
#   select(location, year, valU5)
# 
# dfUnderWgtAllReg <- dfRiskReg %>%
#   subset(rei == "Child underweight" &
#            age == "All ages" &
#            metric == "Rate") %>%
#   select(location, year, val)
# #---
# # Also get other (possibly overdevelopment) dietary DALYs
# keepThese <- c("Diet high in sugar-sweetened beverages",
#                "Diet high in processed meat",
#                "Diet low in fiber",
#                "Diet low in legumes",
#                "Diet low in fruits",
#                "Diet low in vegetables",
#                "Diet low in whole grains",
#                "Diet low in nuts and seeds",
#                #"Diet low in seafood omega-3 fatty acids",
#                "Diet low in polyunsaturated fatty acids",
#                "Diet high in trans fatty acids",
#                "Diet high in sodium")
# dfOverDevReg <- dfRiskRaw %>% subset(rei %in% keepThese &
#                                     age == "All ages" &
#                                     metric == "Rate") %>%
#   select(c("location", "year", "val")) %>%
#   group_by(location, year) %>%
#   summarize(val = sum(val))
# dfOverDevReg$Cat <- "Overnut"
# #---
# # From cause data get iodine deficiency, protein-energy malnutrition,
# # and other deficiencies
# keepThese <- c("Iodine deficiency",
#                "Protein-energy malnutrition",
#                "Other nutritional deficiencies")
# dfCause <- dfCauseRaw %>% subset(cause %in% keepThese &
#                                    age != "Age-standardized" &
#                                    metric == "Number") %>%
#   select(c("location", "cause", "year", "age", "val"))
# dfPEMo5Reg <- dfCauseReg %>%
#   subset(cause == "Protein-energy malnutrition") %>%
#   spread(age, val) %>%
#   mutate(valO5 = `All ages` - `<5 years`) %>%
#   select(-c(`All ages`, `<5 years`))
# dfCauseRegPop <- dfCauseRegRaw %>% subset(cause == "Cardiovascular diseases" &
#                                       age == "All ages") %>%
#   select(c("location", "cause", "year", "metric", "val")) %>%
#   spread(metric, val) %>%
#   mutate(Pop100thous = Number / Rate) %>%
#   select(-c(Number, Rate, cause))
# dfPEMo5Reg <- dfPEMo5Reg %>% merge(dfCauseRegPop) %>%
#   select(location, year, valO5, Pop100thous)
# dfChrReg <- merge(dfPEMo5Reg, dfUnderWgtU5Reg) %>%
#   mutate(val = (valO5 + valU5) / Pop100thous) %>%
#   select(location, year, val)
# dfChrReg$Cat <- "Chr"
# dfChrReg2 <- dfUnderWgtAllReg
# dfChrReg2$Cat <- "Chr2"
# #---
# dfHid1 <- dfRiskReg %>%
#   subset(rei != "Child underweight" &
#            metric == "Rate" &
#            age == "All ages") %>%
#   rename(issue = rei) %>%
#   select(location, year, issue, val)
# dfHid2 <- dfCauseRegRaw %>%
#   subset(cause %in% c("Iodine deficiency",
#                       "Other nutritional deficiencies") &
#                                      age == "All ages" &
#                                      metric == "Rate") %>%
#   rename(issue = cause) %>%
#   select(location, year, issue, val)
# dfHidReg <- as.data.frame(rbind(dfHid1, dfHid2)) %>%
#   group_by(location, year) %>%
#   summarise(val = sum(val)) %>%
#   select(location, year, val)
# dfHidReg$Cat <- "Hid"
# #----------------------------------------------------------
# listDf <- list(dfChrReg, dfHidReg, dfOverDevReg)
# dfDalyG <- as.data.frame(do.call(rbind, listDf))
# colnames(dfDalyG)[1] <- "area"
# keepThese <- c("Sub-Saharan Africa - WB",
#                "South Asia - WB",
#                "Latin America & Caribbean - WB",
#                "Middle East & North Africa - WB",
#                "East Asia & Pacific - WB",
#                "Europe & Central Asia - WB",
#                "North America",
#                "Global")
# dfDalyG <- dfDalyG %>% subset(area %in% keepThese)
# dfPlot <- dfDalyG
# dfPlot$area <- gsub(" - WB", "", dfPlot$area)
# colnames(dfPlot)[which(colnames(dfPlot) == "val")] <- "DALYs/100,000 people"
# #---
# gg <- ggplot(dfPlot, aes(x = year, y = `DALYs/100,000 people`,
#                          group = Cat, color = Cat))
# gg <- gg + geom_line(lwd = 1)
# gg <- gg + facet_wrap(~area, ncol = 4, scales = "free_y")
# gg <- gg + theme_bw()
# gg <- gg + theme(legend.position = "bottom",
#                  legend.title = element_blank(),
#                  axis.title.x = element_blank(),
#                  axis.title.y = element_text(size = axisTitleSize),
#                  axis.text = element_text(size = axisTextSize),
#                  legend.text = element_text(size = legendTextSize),
#                  strip.text = element_text(size = facetTitleSize))
# gg
# 
# saveFile <- "overView.png"
# saveTo <- paste0(graphicsFolder, saveFile)
# ggsave(saveTo, width = 7, height = 4)
# #---
# # Proof that Godecke et al.'s convoluted calculation of
# # chronic hunger DALYs is equal to all age DALYs attributable to
# # child underweight
# dfX <- as.data.frame(rbind(dfChrReg, dfChrReg2)) %>%
#   rename(area = location) %>%
#   subset(area %in% keepThese)
# gg <- ggplot(dfX, aes(x = year, y = val, group = Cat, color = Cat))
# gg <- gg + geom_line()
# gg <- gg + facet_wrap(~area)
# gg
# dfX <- dfX %>% spread(Cat, val) %>%
#   mutate(dif = Chr - Chr2)
# #---
# # Let's look at age-standardized DALYs
# dfChrReg3 <- dfRiskReg %>%
#   subset(rei == "Child underweight" &
#            age == "Age-standardized" &
#            metric == "Rate") %>%
#   select(location, year, val)
# dfChrReg3$Cat <- "Age-standardized"
# dfChrReg2$Cat <- "All ages"
# dfX <- as.data.frame(rbind(dfChrReg3, dfChrReg2)) %>%
#   rename(area = location) %>%
#   subset(area %in% keepThese)
# gg <- ggplot(dfX, aes(x = year, y = val, group = Cat, color = Cat))
# gg <- gg + geom_line()
# gg <- gg + facet_wrap(~area, nrow = 2, scales = "free_y")
# gg <- gg + labs(title = "DALYs/100,000 due to child underweight")
# gg <- gg + theme_bw()
# gg <- gg + theme(legend.position = "top",
#                  legend.title = element_blank(),
#                  axis.title = element_blank())
# gg
# saveFile <- "Child underwgt age standardized.png"
# saveTo <- paste0(graphicsFolder, saveFile)
# ggsave(saveTo, width = 7, height = 4)
#==================================================================
# Bar graph main causes of DALYs by region
# dataFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/DALYs/Hunger DALY data/"
# thisFile <- "IHME-GBD_2019_DATA-c1990-2019regions.csv"
# thisFilePath <- paste0(dataFolder, thisFile)
# dfC <- read.csv(thisFilePath, stringsAsFactors = F)
keepGeo <- c("Sub-Saharan Africa - WB",
             "South Asia - WB",
             "Latin America & Caribbean - WB",
             "Middle East & North Africa - WB",
             "East Asia & Pacific - WB",
             "Europe & Central Asia - WB",
             "North America",
             "Global")
# All level 2 causes basically
keepCause <- c("Cardiovascular diseases",
               "Injuries",
               "Neoplasms",
               "Maternal and neonatal disorders",
               "Muscoskeletal disorders",
               "Other non-communicable diseases",
               "Respiratory infections and tuberculosis",
               "Mental disorders",
               "Diabetes and kidney diseases",
               "Neurological disorders",
               "Chronic respiratory dieseases",
               "Digestive diseases",
               "Enteric infections",
               "Sense organ diseases",
               "HIV/AIDS and sexually transmitted infections",
               "Substance use disorders",
               "Other infectious diseases",
               "Nutritional deficiencies",
               "Skin and subcutaneous diseases")
# rmCause <- c("Communicable, maternal, neonatal, and nutritional diseases",
#              "Transport injuries", "Self-harm and interpersonal violence",
#              "Unintentional injuries",
#              "Non-communicable diseases",
#              "Iodine deficiency",
#              "Protein-energy malnutrition",
#              "Other nutritional deficiencies",
#              "Vitamin A deficiency",
#              "Dietary iron deficiency")
dfC <- dfCauseRaw %>%
  rename(area = location) %>%
  subset(area %in% keepGeo &
           age %in% c("Age-standardized", "All ages") &
           cause %in% keepCause &
           metric == "Rate") %>%
  select(area, cause, year, age, val)
# dfAllC <- dfC %>% subset(cause == "All causes")
# colnames(dfAllC)[ncol(dfAllC)] <- "totVal"
# dfAllC$cause <- NULL
# dfC <- dfC %>% subset(cause != "All causes")
# dfCwide <- dfC %>% spread(cause, val)
# dfC <- merge(dfCwide, dfAllC)
# dfC$Other <- dfC$totVal - rowSums(dfC[, -c(1, 2, ncol(dfC))])
# dfC$totVal <- NULL
# gathercols <- colnames(dfC)[-c(1, 2)]
# dfC <- dfC %>% gather_("cause", "val", gathercols)
dfC$area <- gsub(" - WB", "", dfC$area)
colnames(dfC)[ncol(dfC)] <- "DALYs/100,000 people"
dfPlot <- dfC %>% subset(year == 2019)
gg <- ggplot(dfPlot, aes(x = `DALYs/100,000 people`,
                         y = reorder(cause, `DALYs/100,000 people`),
                         fill = age))
gg <- gg + geom_bar(stat = "identity", position = "dodge")
gg <- gg + facet_wrap(~area, nrow = 2)
gg <- gg + theme_bw()
gg <- gg + theme(legend.position = "top")
gg
# Risk factor - cause map
thisFile <- "IHME-GBD_2019_DATA-rcMap1990-2019regions.csv"
thisFilePath <- paste0(dataFolder, thisFile)
dfMap <- read.csv(thisFilePath, stringsAsFactors = F)
colnames(dfMap)[2] <- "area"
keepRei <- c("Child and maternal malnutrition",
             "Dietary risks")
dfMap <- dfMap %>% subset(area %in% keepGeo &
                            rei %in% keepRei &
                            age == "All ages" &
                            metric == "Rate") %>%
  select(area, rei, cause, year, val)
dfMap$area <- gsub(" - WB", "", dfMap$area)
dfMapAllC <- dfMap %>% subset(cause == "All causes")
colnames(dfMapAllC)[ncol(dfMapAllC)] <- "totVal"
dfChk <- dfMap %>% spread(cause, val)
dfChk$`All causes`[which(is.na(dfChk$`All causes`))] <- 0
dfChk$Other <- dfChk$`All causes` - rowSums(dfChk[, -c(1:4)], na.rm = T)
#hist(dfChk$Other)
dfMap <- dfMap %>% spread(cause, val)
naTo0 <- function(x){
  indNA <- which(is.na(x))
  if(length(indNA) != 0){
    x[indNA] <- 0
  }
  return(x)
}
dfMap$`All causes` <- NULL
dfMap[, -c(1:3)] <- as.data.frame(apply(dfMap[, -c(1:3)], 2, naTo0))
gathercols <- colnames(dfMap)[-c(1:3)]
dfMap <- dfMap %>% gather_("cause", "val", gathercols)
colnames(dfMap)[ncol(dfMap)] <- "contrib"
#dfMap <- dfMap %>% subset(cause != "All causes")
dfM <- merge(dfMap, dfC, all.y = T)
# indNA <- which(is.na(dfM$contrib))
# dfM$contrib[indNA] <- 0
dfM <- dfM %>% spread(rei, contrib)
dfM$`<NA>` <- NULL
dfM[, -c(1:4)] <- as.data.frame(apply(dfM[, -c(1:4)], 2, naTo0))
dfM$Other <- dfM$`DALYs/100,000 people` - rowSums(dfM[, -c(1:4)])
dfM$`DALYs/100,000 people` <- NULL
gathercols <- colnames(dfM)[-c(1:3)]
dfM <- dfM %>% 
  gather_("Risk factor contribution", "DALYs/100,000 people", gathercols)
dfPlot <- dfM %>% subset(year == 2019)
gg <- ggplot(dfPlot, aes(x = `DALYs/100,000 people`,
                         y = reorder(cause, `DALYs/100,000 people`),
                         fill = `Risk factor contribution`))
gg <- gg + geom_bar(stat = "identity", color = "black")
gg <- gg + facet_wrap(~area, nrow = 2)#, scales = "free_x")
gg <- gg + theme_bw()
gg <- gg + theme(legend.position = "top",
                 legend.title = element_text(size = axisTitleSize),
                 axis.title.y = element_blank(),
                 axis.title.x = element_text(size = axisTitleSize),
                 axis.text = element_text(size = axisTextSize),
                 legend.text = element_text(size = legendTextSize),
                 strip.text = element_text(size = facetTitleSize))
gg

saveFile <- "riskFactrContrib2.png"
saveTo <- paste0(graphicsFolder, saveFile)
ggsave(saveTo, width = 8, height = 6)
# <- c("Chronic kidney disease", "Diabetes mellitus",
#   "Total cancers", "Maternal and neonatal disorders",
#   "Sudden infant death syndrome",
#   "Ischemic heart disease", "Stroke",
#   "Total burden related to Non-alcoholic fatty liver disease (NAFLD)",
#   "Blindness and vision loss",
#   "Total burden related to hepatitis B",
#   "Total burden related to hepatitis C")