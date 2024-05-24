library(tidyverse)
library(FAOSTAT)
library(jtools)
library(ggrepel)
library(miceadds)
library(gganimate)
library(factoextra)
library(plm)
library(fixest)
library(modelsummary)
#---
axisTitleSize <- 8
axisTextSize <- 7
legendTextSize <- 7
facetTitleSize <- 7
#--------------------------------------------------------------
# Create country vecs for regional subsetting
thisFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FAO country region keys/"
theseFiles <- list.files(thisFolder)
listVec <- list()
for(i in 1:length(theseFiles)){
  print(theseFiles[i])
  thisPath <- paste0(thisFolder, theseFiles[i])
  thisDf <- read.csv(thisPath, stringsAsFactors = F)
  thisVec <- thisDf$Area
  listVec[[theseFiles[i]]] <- thisVec
}
ctyInAsansCWA <- listVec[[theseFiles[1]]]
ctyInCWANA <- listVec[[theseFiles[3]]]
#ctyInEUR <- listVec[[theseFiles[3]]]
ctyInLAC <- listVec[[theseFiles[5]]]
ctyInNAMEURAUSNZ <- listVec[[theseFiles[6]]]
ctyInSSA <- listVec[[theseFiles[7]]]
#--------------------------------------------------------------
#https://vizhub.healthdata.org/gbd-results/
picFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/DALYs/"
thisFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/DALYs/Hunger DALY data/"
# thisFile <- "IHME-GBD_2019_DATA-countries.csv"
# thisFile <- "IHME-GBD_2019_DATA-c1990-2019countries.csv"
# thisFilePath <- paste0(thisFolder, thisFile)
# dfCauseRaw <- read.csv(thisFilePath, stringsAsFactors = F)
theseFiles <- paste0(thisFolder, c("IHME-GBD_2019_ALLc1990-2019countries-"), c(1:2), ".csv")
listDf <- lapply(theseFiles, read.csv)
dfCauseRaw <- as.data.frame(do.call(rbind, listDf))
# thisFile <- "IHME-GBD_2019_DATA-r1990-2019countries.csv"
# thisFilePath <- paste0(thisFolder, thisFile)
# dfRiskRaw <- read.csv(thisFilePath, stringsAsFactors = F)
theseFiles <- paste0(thisFolder, c("IHME-GBD_2019_ALLr1990-2019countries-"), c(1:3), ".csv")
listDf <- lapply(theseFiles, read.csv)
dfRiskRaw <- as.data.frame(do.call(rbind, listDf))
#---------------------------------------------------------------
# Chronic hunger DALYs
# From risk factor data get DALYs due to zinc, iron,
# vit. A deficiency, and child underweight
dfChr <- dfRiskRaw %>%
  subset(rei == "Child underweight" &
# dfChr <- dfCauseRaw %>%
#   subset(cause == "Protein-energy malnutrition" &
           age == "All ages" &
           metric == "Rate") %>%
  rename(area = location) %>%
  select(area, year, val)
dfChr$Cat <- "Chr"
#---
# Overnutrition DALYs
keepThese <- c("Diet high in sugar-sweetened beverages",
               "Diet high in processed meat",
          #"Diet low in fiber",
          # "Diet low in legumes",
          # "Diet low in fruits",
          # "Diet low in vegetables",
          #"Diet low in whole grains",
          #"Diet low in nuts and seeds",
          # "Diet low in seafood omega-3 fatty acids",
          #"Diet low in polyunsaturated fatty acids",
          "Diet high in trans fatty acids",
          "Diet high in sodium")
overDevVec <- keepThese
dfOverDev <- dfRiskRaw %>% subset(rei %in% keepThese &
                                    age == "All ages" &
                                    metric == "Rate") %>%
  rename(area = location) %>%
  select(area, year, val) %>%
  group_by(area, year) %>%
  summarize(val = sum(val))
dfOverDev$Cat <- "OverDev"
#---
# Hidden hunger DALYs
# From cause data get iodine deficiency, protein-energy malnutrition,
# and other deficiencies
hidHcause <- c("Iodine deficiency",
               "Other nutritional deficiencies")
hidHrisks <- c("Vitamin A deficiency",
               "Zinc deficiency",
               "Iron deficiency")
dfHid1 <- dfRiskRaw %>%
  subset(rei %in% hidHrisks &
           metric == "Rate" &
           age == "All ages") %>%
  rename(issue = rei, area = location) %>%
  select(area, year, issue, val)
dfHid2 <- dfCauseRaw %>%
  subset(cause %in% hidHcause &
           metric == "Rate" &
           age == "All ages") %>%
  rename(issue = cause, area = location) %>%
  select(area, year, issue, val)
dfHid <- as.data.frame(rbind(dfHid1, dfHid2)) %>%
  group_by(area, year) %>%
  summarise(val = sum(val))
dfHid$Cat <- "Hid"
#---
# Unite them
listDf <- list(dfChr, dfHid, dfOverDev)
dfDaly <- as.data.frame(do.call(rbind, listDf))
dfDaly$area <- gsub("Turkey", "Turkiye", dfDaly$area)
dfDaly <- subset(dfDaly, !(area %in% c("Taiwan (Province of China)")))
#--------------------------------------------------------------
# Get IHME's Socio-demographic index too and merge
thisFile <- "IHME_GBD_2019_SDI_1990_2019_Y2020M10D15.xlsx"
thisFilePath <- paste0(thisFolder, thisFile)
#dfRaw <- read.csv(thisFilePath, stringsAsFactors = F)
dfRaw <- readxl::read_excel(thisFilePath)
colnames(dfRaw) <- dfRaw[1, ]
dfRaw <- dfRaw[-1, ]
# Get rid of Georgia the USA state
indGeorgia <- which(dfRaw$Location == "Georgia")
dfRaw <- dfRaw[-indGeorgia[2], ]
# Get rid of repeated MENA row
indMENA <- which(dfRaw$Location == "North Africa and Middle East")
dfRaw <- dfRaw[-indMENA[2], ]
# Get rid of repeated South Asia row
indSAS <- which(dfRaw$Location == "South Asia")
dfRaw <- dfRaw[-indSAS[2], ]
gathercols <- colnames(dfRaw)[-1]
dfSDI <- dfRaw %>% as.data.frame() %>%
  gather_("year", "sdi", gathercols)
dfSDI$sdi <- as.numeric(sub("..", "0.", dfSDI$sdi))
colnames(dfSDI)[1] <- "area"
u <- dfSDI$area
#unique(u[grep("Cote", u)])
dfSDI$area[grep("Venezuela", u)] <- "Venezuela (Bolivarian Republic of)"
dfSDI$area[grep("USA", u)] <- "United States of America"
dfSDI$area[grep("Vietnam", u)] <- "Viet Nam"
#dfSDI$area[grep("Virgin Islands", u)] <- "United States Virgin Islands"
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
#dfSDI$area[grep("Swaziland", u)] <- "Eswatini"
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
#--------------------------------------------------------------
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
thisFile <- "popUnder14pct.csv"
thisFilePath <- paste0(thisFolder, thisFile)
dfPopYoung <- read.csv(thisFilePath, stringsAsFactors = F) %>%
  rename(area = Country.Name)
# indEnd <- which(dfPopYoung$Series.Name == "")[1] - 1
# dfPopYoung <- dfPopYoung[1:indEnd, ]
gatherCols <- colnames(dfPopYoung)[5:ncol(dfPopYoung)]
dfPopYoung <- dfPopYoung %>% gather_("year", "pop14", gatherCols) %>%
  select(area, year, pop14)
dfPopYoung$year <- readr::parse_number(dfPopYoung$year)
setdiff(unique(dfPopYoung$area), unique(dfDaly$area))
u <- dfPopYoung$area
unique(u[grep("Cote d'Ivoire", u)])
unique(dfDaly$area[grep("Côte d'Ivoire", dfDaly$area)])
dfPopYoung$area[grep("Cote d'Ivoire", u)] <- "Côte d'Ivoire"
dfPopYoung$area[grep("Kyrgyz", u)] <- "Kyrgyzstan"
dfPopYoung$area[grep("Egypt", u)] <- "Egypt"
dfPopYoung$area[grep("Venezuela", u)] <- "Venezuela (Bolivarian Republic of)"
dfPopYoung$area[which(u == "United States")] <- "United States of America"
dfPopYoung$area[grep("Vietnam", u)] <- "Viet Nam"
dfPopYoung$area[grep("Virgin Islands \\(U\\.S\\.\\)", u)] <- "United States Virgin Islands"
#dfPopYoung$area[grep("British Virgin Islands", u)] <- 
dfPopYoung$area[grep("Tanzania", u)] <- "United Republic of Tanzania"
# dfPopYoung$area[grep("United Kingdom", u)] <- "United Kingdom"
#dfPopYoung$area[grep("Turkiye", u)] <- "Türkiye"
dfPopYoung$area[grep("Yemen", u)] <- "Yemen"
# dfPopYoung$area[grep("Syria", u)] <- "Syrian Arab Republic"
# dfPopYoung$area[grep("Russia", u)] <- "Russian Federation"
dfPopYoung$area[grep("Moldova", u)] <- "Republic of Moldova"
dfPopYoung$area[grep("Korea, Rep\\.", u)] <- "Republic of Korea"
dfPopYoung$area[grep("Korea, Dem\\. People's", u)] <- "Democratic People's Republic of Korea"
dfPopYoung$area[grep("Micronesia", u)] <- "Micronesia (Federated States of)"
dfPopYoung$area[grep("Lao PDR", u)] <- "Lao People's Democratic Republic"
dfPopYoung$area[grep("Iran, Islamic", u)] <- "Iran (Islamic Republic of)"
dfPopYoung$area[grep("Gambia", u)] <- "Gambia"
#dfPopYoung$area[grep("Swaziland", u)] <- "Eswatini"
dfPopYoung$area[grep("Congo, Dem\\. Rep\\.", u)] <- "Democratic Republic of the Congo"
dfPopYoung$area[grep("Congo, Rep\\.", u)] <- "Congo"
#dfPopYoung$area[grep("Czech", u)] <- "Czechia"
#dfPopYoung$area[grep("Cabo Verde", u)] <- "Cabo Verde"
#dfPopYoung$area[grep("Brunei", u)] <- "Brunei Darussalam"
#dfPopYoung$area[grep("São Tomé and PrÍncipe", u)] <- "Sao Tome and Principe"
dfPopYoung$area[grep("Bolivia", u)] <- "Bolivia (Plurinational State of)"
dfPopYoung$area[grep("Bahamas", u)] <- "Bahamas"
dfPopYoung$area[grep("Slovak", u)] <- "Slovakia"
setdiff(unique(dfPopYoung$area), unique(dfDaly$area))
dfPopYoung$pop14 <- as.numeric(dfPopYoung$pop14)
# indNA <- which(is.na(dfPopYoung$pop14))
# dfPopYoung[indNA, ]
dfDaly <- dfDaly %>% merge(dfPopYoung)
unique(dfDaly$area)
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
# thisFilePath <- paste0(thisFolder, thisFile)
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
# dfGDPmerge$Issue <- "GDP/capita (USD 2015 prices)"
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
#indNA <- which(is.na(X$`GDP/capita (USD 2015 prices)`)); X$area[indNA]
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
saveTo <- paste0(picFolder, saveFile)
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
saveTo <- paste0(picFolder, saveFile)
ggsave(saveTo, width = 5, height = 3)
#---
k2 <- kmeans(X, centers = 2, nstart = 25)
fviz_cluster(k2, data = X)
#===============================================================
#===============================================================
# FAO
# dfKey <- FAOsearch()
# thisFolder <- "FAOSTAT data"
# dir.create(thisFolder)
# # Food balance sheets (have to unite old and new files)
# # Note these include population
# dfRaw <- get_faostat_bulk(code = "FBS", data_folder = thisFolder)
thisFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/DALYs/Hunger DALY data/FAO FBS/"
thisFile <- "FoodBalanceSheets_E_All_Data.csv"
thisFilePath <- paste0(thisFolder, thisFile)
dfRaw <- read.csv(thisFilePath, stringsAsFactors = F)
theseElems <- c("Food supply (kcal/capita/day)", "Total Population - Both sexes",
                "Protein supply quantity (g/capita/day)",
                "Fat supply quantity (g/capita/day)")
dfRaw <- dfRaw %>% select(-c(contains("Code"), ends_with("F"))) %>% subset(Element %in% theseElems)
yrCols <- colnames(dfRaw)[grep("20", colnames(dfRaw))]
dfRaw <- dfRaw %>% gather("year", "value", yrCols)
dfRaw$year <- gsub("Y", "", dfRaw$year)
colnames(dfRaw) <- tolower(colnames(dfRaw))
dfFBSraw <- dfRaw; rm(dfRaw)
# keepCols <- c("area", "year", "item", "element", "unit", "value")
# dfFBSraw2 <- dfRaw[, keepCols]; rm(dfRaw)
# # dfRaw <- get_faostat_bulk(code = "FBSH", data_folder = thisFolder)
# # keepCols <- c("area", "year", "item", "element", "unit", "value")
# # dfFBSraw1 <- dfRaw[, keepCols]
# dfFBSraw <- dfFBSraw2; rm(dfFBSraw2)
# dfLook <- dfFBSraw %>% subset(year == 2019)
# u <- dfLook$area
# unique(u[grep("Lesotho", u)])
# dfPop <- dfFBSraw %>% subset(item == "Population" &
#                   element == "total_population___both_sexes") %>%
#   rename(Population = value) %>%
#   select("area", "year", "Population")
# Rectification of names
# setdiff(dfFBSraw$area, dfHidHung$area)
# setdiff(dfHidHung$area, dfFBSraw$area)
# unique(dfHidHung$area)
# unique(dfFBSraw$area)
#unique(dfFBSraw$area[grep("rkiye", dfFBSraw$area)])
dfFBSraw$area[grep("United Kingdom", dfFBSraw$area)] <- "United Kingdom"
dfFBSraw$area[grep("Netherlands", dfFBSraw$area)] <- "Netherlands"
dfFBSraw$area[grep("rkiye", dfFBSraw$area)] <- "Turkiye"
dfFBSraw$area[grep("Ivoire", dfFBSraw$area)] <- "Côte d'Ivoire"
notThese <- c("China, Hong Kong SAR", "China, Macao SAR",
              "China, Taiwan Province of", "China, mainland")
dfFBSraw <- subset(dfFBSraw, !(area %in% notThese))
# Fix milk (and any other duplicates)
dfFBSraw <- dfFBSraw %>% group_by(area, year, element, item) %>%
  mutate(x = duplicated(item)) %>%
  subset(x == F) %>%
  select(-x)
#---
# Preliminary Look
# theseItems <- c("Cereals - Excluding Beer",
#                 "Starchy Roots", "Vegetables",
#                 "Vegetable Oils",
#                 "Oilcrops",
#                 "Fruits - Excluding Wine", "Pulses",
#                 "Animal Products",
#                 "Sugar Crops",
#                 "Alcoholic Beverages")
# dfLook <- dfFBSraw %>% subset(area %in% "World" &
#                              year == 2018 &
#                              #item %in% theseItems &
#                              element == "Food supply (kcal/capita/day)")
#                              #"food_supply__kcal_capita_day_")
# 
# dfLook <- dfLook %>% subset(value > quantile(dfLook$value, 0.6))
# gg <- ggplot(dfLook, aes(x = value,
#                          y = reorder(item, value)))
# gg <- gg + geom_bar(stat = "identity")
# gg <- gg + facet_wrap(~area)
# gg <- gg + theme_bw()
# gg <- gg + theme(axis.text = element_text(size = axisTextSize),
#                  axis.title = element_blank(),
#                  strip.text = element_text(size = facetTitleSize))
# gg
# thisPic <- "eatRankWld.png"
# thisPicPath <- paste0(picFolder, thisPic)
# ggsave(thisPicPath, width = 5, height = 3)
#=====================================================================
# GDP/capita
thisFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/DALYs/Hunger DALY data/FAO FBS/"
thisFile <- "Macro-Statistics_Key_Indicators_E_All_Data.csv"
thisFilePath <- paste0(thisFolder, thisFile)
dfRaw <- read.csv(thisFilePath, stringsAsFactors = F)
theseElems <- "Value US$ per capita, 2015 prices"
dfRaw <- dfRaw %>% select(-c(contains("Code"), ends_with("F"), ends_with("N"))) %>% subset(Element %in% theseElems)
yrCols <- colnames(dfRaw)[grep("20|19", colnames(dfRaw))]
dfRaw <- dfRaw %>% gather("year", "value", yrCols)
dfRaw$year <- gsub("Y", "", dfRaw$year)
colnames(dfRaw) <- tolower(colnames(dfRaw))
dfGDP <- dfRaw %>% select(area, year, value) %>% rename(`GDP/capita` = value)
rm(dfRaw)
dfGDP$area[grep("United Kingdom", dfGDP$area)] <- "United Kingdom"
dfGDP$area[grep("Netherlands", dfGDP$area)] <- "Netherlands"
dfGDP$area[grep("rkiye", dfGDP$area)] <- "Turkiye"
dfGDP$area[grep("Ivoire", dfGDP$area)] <- "Côte d'Ivoire"
notThese <- c("China, Hong Kong SAR", "China, Macao SAR",
              "China, Taiwan Province of", "China, mainland")
dfGDP <- subset(dfGDP, !(area %in% notThese))
# dfRaw <- get_faostat_bulk(code = "MK", data_folder = thisFolder)
# keepCols <- c("area", "year", "item", "element", "unit", "value")
# dfGDP <- dfRaw[, keepCols] %>% subset(item == "Gross Domestic Product" &
#                                         element == "value_us__per_capita__2015_prices") %>%
#   select(c("area", "year", "value")) %>%
#   rename(`GDP/capita (USD 2015 prices)` = value)
# dfGDP <- dfGDP %>% subset(!(area %in% notThese))
# dfGDP$area[grep("United Kingdom", dfGDP$area)] <- "United Kingdom"
# dfGDP$area[grep("Netherlands", dfGDP$area)] <- "Netherlands"
# # setdiff(unique(dfGDP$area), unique(dfRiskRaw$location))
# # unique(dfGDP$area)[order(unique(dfGDP$area))]
# # unique(dfRiskRaw$location)[order(unique(dfRiskRaw$location))]
# #dfFBSraw <- merge(dfFBSraw, dfGDP)
#=====================================================================
# Get df for commodity model
# u <- dfFBSraw$item
# unique(u[grep("Sugar", u)])
# Note "Fruit, other" and "Vegetables, other" both include
# 567 "Watermelons" and 568 "Melons, other (inc.cantaloupes)"
theseItems <- c("Cereals - Excluding Beer",
                "Starchy Roots", "Vegetables",
                "Vegetable Oils",
                #"Oilcrops", #consumed oil crop oils are included under vegetable oils
                "Fruits - Excluding Wine", "Pulses",
#                "Animal Products",
                "Animal fats",
#                "Meat",
#                "Milk - Excluding Butter",
                #"Sugar Crops",
                #"Sugar & Sweeteners",
                "Sugar (Raw Equivalent)",
                #"Alcoholic Beverages",
                "Grand Total")

dfPop <- dfFBSraw %>% subset(item == "Population") %>%
  #rename(`Population (1000 persons)` = value) %>%
  rename(Population = value) %>%
  select(area, year, Population)
dfPop$element <- NULL; dfPop$item <- NULL
dfFBS <- dfFBSraw %>% select(area, year, item, element, value) %>%
  subset(item %in% theseItems) %>%
  mutate(item = gsub("Fruits - Excluding Wine", "F&V", item)) %>%
  mutate(item = gsub("Vegetables", "F&V", item)) %>%
  #mutate(item = gsub("Milk - Excluding Butter", "Milk", item)) %>%
  mutate(item = gsub("Cereals - Excluding Beer", "CRT", item)) %>%
  mutate(item = gsub("Starchy Roots", "CRT", item)) %>%
  #mutate(item = gsub("Sugar \\(Raw Equivalent\\)", "CRTnS", item)) %>%
  mutate(item = gsub("Animal fats", "Fats", item)) %>%
  mutate(item = gsub("Vegetable Oils", "Fats", item)) %>%
  group_by(area, year, item, element) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  spread(item, value) %>%
  # mutate(`Animal prod other` = `Animal Products` -
  #          Milk - Meat) %>%
  #select(-c(`Animal Products`)) %>%
  merge(dfPop, by = c("area", "year")) %>% as.data.frame()
#mutate(`CR&T` = Cereals + `Starchy Roots`) %>%
#select(-c(Cereals, `Starchy Roots`)) #%>%
# mutate(fatRat = `Vegetable Oils` / `Animal fats`) %>%
# select(-c(`Vegetable Oils`, `Animal fats`))
# Replace Muslim country Alcoholic Beverages NA with 0
if("Alcoholic Beverages" %in% theseItems){
  u <- dfFBS$`Alcoholic Beverages`
  dfFBS$`Alcoholic Beverages`[which(is.na(u))] <- 0
}
# Check for NAs
naFn <- function(x){u <- sum(is.infinite(x)); return(u)}
o <- apply(dfFBS[, -c(1:3)], 2, naFn); colNA <- which(o > 0)
# Population is NA in a few small and/or politically volatile ctys. Drop these.
unique(dfFBS$area[which(is.na(dfFBS$Population))])
rowRm <- which(is.na(dfFBS$Population))
# Also get rid of outliers in the errors vs fitted values plot: Mali, Chad
AddToRowRm <- which(dfFBS$area %in% c("Chad", "Mali"))
if(length(rowRm) != 0){rowRm <- unique(c(rowRm, AddToRowRm))}else{rowRm <- AddToRowRm}
if(length(rowRm) != 0){dfFBS <- dfFBS[-rowRm, ]}
# Calculate residual kcal category
colSkip <- which(colnames(dfFBS) %in% c("Grand Total", "Population"))
dfFBS$Residual <- dfFBS$`Grand Total` -
  rowSums(dfFBS[, -c(1:3, colSkip)])
# Make sure dfDaly and dfFBS ctys match
setdiff(dfDaly$area, dfFBS$area)
setdiff(dfFBS$area, dfDaly$area)
#unique(dfFBS$area[grep("Guinea", dfFBS$area)])
# Separate into FBS commodity and FBS macnut dfs
dfFBScom <- dfFBS %>% subset(element == "Food supply (kcal/capita/day)") %>% select(-c(element, `Grand Total`))
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
# dfRaw <- get_faostat_bulk(code = "SDGB", data_folder = thisFolder)
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
thisPicPath <- paste0(picFolder, thisPic)
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
thisPicPath <- paste0(picFolder, thisPic)
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
  subset(year == thisYr & area %in% areaVecPCA_ihme) %>%
  select(-c(year, `Population (1000 persons)`))
# setdiff(areaVecPCA, dfFBSpca$area)
# u <- dfFBSpca$area
# u[grep("Eritrea", u)]
# v <- areaVecPCA
# v[grep("Singapore", v)]
naFn <- function(x){u <- sum(is.na(x)); return(u)}
o <- apply(X[, -1], 2, naFn); colNA <- which(o > 0)
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
  select(-"area") %>% as.matrix() %>%
  apply(2, scale)
eigOut <- X %>% cor() %>% eigen()
eigVals <- eigOut$values
cumFracExpld <- cumsum(eigVals) / sum(eigVals)
cutOff <- which(cumFracExpld > 0.7)[1]
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
saveTo <- paste0(picFolder, saveFile)
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
saveTo <- paste0(picFolder, saveFile)
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
# thisAnimPath <- paste0(picFolder, "PCfbs.gif")
# anim_save(thisAnimPath, gg)
#========================================================================
# Model year
#thisYr <- 2019
# Model 1
# Hunger ~ Commodity
dfMod <- merge(dfDaly, dfFBSmn) %>% #merge(dfClustKey) %>%
  merge(dfGDP) %>%
#  subset(year == thisYr) %>%
  # mutate(`DALYs/capita` = 1000 * val / `Population (1000 persons)`) %>%
  #rename(`DALYs/100,000 capita` = val) %>%
  #select(-c(year))
  #select(-c("year", "Population (1000 persons)"))
  select(-Population)
colnames(dfMod)
notCols <- which(colnames(dfMod) %in% c("area", "year", "Region", "Cat"))
#notCols <- c(1:3)
# Log transform
dfMod[, -notCols] <- as.data.frame(apply(dfMod[, -notCols], 2, log))
#dfMod[, -1] <- as.data.frame(apply(dfMod[, -1], 2, log))
# Lots of places/times where/when people not eating pulses
isInfNan <- function(x){
  nInfNan <- sum(is.nan(x) > 0) + sum(is.infinite(x) > 0) +
    sum(is.na(x) > 0)
  return(nInfNan)
}
infNanLook <- apply(dfMod[, -notCols], 2, isInfNan)
#---
# macnut models
# Where are the infinite values happening?
unique(dfMod$area[which(is.infinite(dfMod$`Food supply (kcal/capita/day)`))])
rowRm <- which(is.infinite(dfMod$`Food supply (kcal/capita/day)`))
dfMod <- dfMod[-rowRm, ]
#---
# commod models
#Where are people not eating x?
unique(dfMod$area[which(is.infinite(dfMod$Pulses))])
unique(dfMod$area[which(is.infinite(dfMod$`Sugar (Raw Equivalent)`))])
indNoPulses <- which(is.infinite(dfMod$Pulses))
dfMod[indNoPulses, ] <- 0
#---
# keepRegs <- c("Africa South\nof the Sahara",
#               "Eur. / N. Amer. /\nAus. / NZ", "CWANA")
# dfMod$Region[which(!(dfMod$Region %in% keepRegs))] <- "Other"
# unique(dfMod$Region)
# library(fastDummies)
# dfMod <- dummy_cols(dfMod, select_columns = "year")
dfMod$Region[which(is.na(dfMod$Region))] <- "Other"
#---
#charCols <- notCols
catCol <- c(5)
dfModHid <- dfMod %>% subset(Cat == "Hid")
ctyVecHid <- dfModHid$area
dfModHid <- dfModHid %>% select (-catCol)
dfModChr <- dfMod %>% subset(Cat == "Chr")
ctyVecChr <- dfModChr$area
dfModChr <- dfModChr %>% select (-catCol)
dfModOve <- dfMod %>% subset(Cat == "OverDev")
ctyVecOve <- dfModOve$area
dfModOve <- dfModOve %>% select (-catCol)
#---
#indDummy <- grep("Region", colnames(dfModChr))
#---
# Chronic hunger
colnames(dfModChr)[4] <- "y"
#colnames(dfModChr)[4] <- paste0("`", colnames(dfModChr)[4], "`")
vars <- colnames(dfModChr)[7:ncol(dfModChr)]
vars <- colnames(dfModChr)[c(5, 7:12)]
vars <- colnames(dfModChr)[c(5, 7, 9)]
vars <- paste0("`", vars, "`")
modFE <- feols(y ~ .[vars] | area, data = dfModChr, cluster = c("area"))
modFE <- feols(y ~ sdi + `F&V` | area, data = dfModChr, cluster = c("area"))
#modFE <- feols(y ~ .[vars] | year, data = dfModChr, cluster = c("area", "year"))
summary(modFE)
plot(modFE$fitted.values, modFE$residuals)
unique(dfModChr$area[which(abs(modFE$residuals) > 0.3)])
hist(fixef(modFE)$area, breaks = 10)
#---
#Bennet's Law
colnames(dfModChr)[c(7)] <- "CRT"
modFE <- feols(CRT ~ `GDP/capita` | year, data = dfModChr, cluster = c("year"))
#modFE <- feols(y ~ .[vars] | year, data = dfModChr, cluster = c("area", "year"))
summary(modFE)
plot(modFE$fitted.values, modFE$residuals)
modelsummary(modFE, statistic = "p.value")
#---
names(fixef(modFE)$area)
library(marginaleffects)
#View(slopes(modFE))
avg_slopes(modFE)
#car::vif(modFE)
models <- list(
  feols(y ~ .[vars] | area, data = dfModChr),
  feols(y ~ .[vars] | area, cluster = ~area+year, data = dfModChr),
  feols(y ~ .[vars] | area+year, cluster = ~area+year, data = dfModChr),
  feols(y ~ .[vars] | year, data = dfModChr),
  feols(y ~ .[vars] | year, cluster = ~year+area, data = dfModChr)
  )
modelsummary(models, statistic = "p.value", output = "chrFEmods.docx")
# pDfModChr <- pdata.frame(dfModChr, index = c("area", "year"))
# modFEc <- plm(y~.-area-year-Region, data = pDfModChr, index = c("area", "year"), model = "within", effect = "individual")
# modFEt <- plm(y~.-area-year-Region, data = pDfModChr, index = c("area", "year"), model = "within", effect = "time")
# summary(modFEt)
# pFtest(modFEt, modFEc)
# plmtest(modFE, c("time"), type=("bp"))
#---
# Hidden hunger
colnames(dfModHid)[4] <- "y"
# dfModHid$sdi <- NULL
# dfModHid$pop14 <- NULL
# dfModHid$Region <- NULL
# dfModHid$Residual <- NULL
# dfModHid$`Food supply (kcal/capita/day)` <- NULL
# dfModHid$`Protein supply (kcal/capita/day)` <- NULL
vars <- colnames(dfModHid)[5:ncol(dfModHid)]
vars <- colnames(dfModHid)[c(5, 7, 9)]
vars <- paste0("`", vars, "`")
modFE <- feols(y ~ .[vars] | area, data = dfModHid)
summary(modFE)
plot(modFE$fitted.values, modFE$residuals)
unique(dfModChr$area[which(modFE$fitted.values > 5.5)])
hist(fixef(modFE)$area, breaks = 10)

models <- list(
  feols(y ~ .[vars] | area, data = dfModHid),
  feols(y ~ .[vars] | area, cluster = ~area+year, data = dfModHid),
  feols(y ~ .[vars] | area+year, cluster = ~area+year, data = dfModHid),
  feols(y ~ .[vars] | year, data = dfModHid),
  feols(y ~ .[vars] | year, cluster = ~year+area, data = dfModHid)
)
modelsummary(models, statistic = "p.value")


# pDfModHid <- pdata.frame(dfModHid, index = c("area", "year"))
# modFE <- plm(`DALYs.100.000.capita`~.-area-year, data = pDfModHid, index = c("area", "year"), model = "within", effect = "time")
# summary(modFE)
# fixef(modFE)
# modFE1 <- modFE
# modFE2 <- modFE
# modFE3 <- modFE
# export_summs(modFE1, modFE2, modFE3, model.names = c("Hid1", "Hid2", "Hid3"), to.file = "docx", file.name = paste0(picFolder, "modHidFE.docx"))

# pDfModHid <- pdata.frame(dfModHid, index = c("area", "year"))
# modP <- plm(`DALYs.100.000.capita`~., data = pDfModHid, model = "pooling")
# summary(modP)
# modP <- lm.cluster(dfModHid, `DALYs/100,000 capita`~.-year-area,
#                    cluster = "year")
# texreg::extract(modP)
# # create table in stargazer
# summary(modP, cluster = "year")
# library(stargazer)
# stargazer(modP, se = list(coef(summary(modP, cluster = "year"))[, 2]), type = "text")

# mod <- lm(`DALYs/100,000 capita` ~., dfModHid)
# summ(mod)
#summary(mod)
# car::vif(mod)
# plot(mod$fitted.values, mod$residuals)
# modHid <- mod
# pVals <- summary(mod)$coefficients[-1, 4]
# colRm <- which(pVals > 0.5) + 1
# dfModHid <- dfModHid[, -colRm]
#---
dfModOve[, -nonCont] <- as.data.frame(apply(dfModOve[, -nonCont], 2, refFn))
# dfModOve$Region <- NULL
# dfModOve$pop14 <- NULL
# dfModOve$sdi <- NULL
# dfModOve$Residual <- NULL
# dfModOve$`Food supply (kcal/capita/day)` <- NULL
# dfModOve$`Protein supply (kcal/capita/day)` <- NULL
colnames(dfModOve)[4] <- "y"
vars <- colnames(dfModOve)[5:ncol(dfModOve)]
vars <- colnames(dfModOve)[c(5, 7, 9)]
vars <- paste0("`", vars, "`")
modFE <- feols(y ~ sdi | area, data = dfModOve)
modFE <- feols(y ~ .[vars] | area, data = dfModOve)
summary(modFE)
plot(modFE$fitted.values, modFE$residuals)
unique(dfModChr$area[which(modFE$fitted.values > 5.5)])
hist(fixef(modFE)$area, breaks = 15)

# pDfModOve <- pdata.frame(dfModOve, index = c("area", "year"))
# modFE <- plm(DALYs.100.000.capita~.-area-year, data = pDfModOve, index = c("area", "year"), model = "within", effect = "time")
# summary(modFE)
# pDfModChr$area<-NULL
# pDfModChr$year<-NULL
# modFE1 <- modFE
# modFE2 <- modFE
# modFE3 <- modFE
# modFE4 <- modFE
# export_summs(modFE1, modFE2, modFE3, modFE4, model.names = c("Ove1", "Ove2", "Ove3", "Ove4"), to.file = "docx", file.name = paste0(picFolder, "modOveFE.docx"))

n <- names(pDfModOve)
f <- as.formula(paste("DALYs.100.000.capita ~", paste(n[!n %in% "y"], collapse = " + ")))
modP <- plm(f, data = pDfModOve, index = c("area", "year"), model = "pooling",
            random.method = "walhus")
summary(modP)
modP <- lm.cluster(dfModOve, `DALYs/100,000 capita`~.-year-area,
                   cluster = "year")
texreg::extract(modP)
stargazer(modP, se = list(coef(summary(modP, cluster = "year"))[, 2]), type = "text")
summary(modP, cluster = "year")
stargazer(modP, type = "text")
# mod <- lm(`DALYs/100,000 capita` ~., dfModOve)
# summ(mod)
#summary(mod)
car::vif(mod)
plot(mod$fitted.values, mod$residuals)
modOve <- mod
pVals <- summary(mod)$coefficients[-1, 4]
colRm <- which(pVals > 0.25) + 1
dfModOve <- dfModOve[, -colRm]
#---
#"mod1Results.docx"
export_summs(modOve, model.nmes = c("Overnut"), to.file = "docx", file.name = paste0(picFolder, "mod1Results.docx"))
export_summs(modChr, modHid, modOve, model.nmes = c("Chronic\nHunger", "Hidden\nHunger", "Overnut"), to.file = "docx", file.name = paste0(picFolder, "mod1Results.docx"))
#---
#-------------------------------------------------------------
# Pooled OLS
dfMod <- merge(dfDaly, dfFBScommod) %>%
  rename(`DALYs/100,000 capita` = val) %>%
  subset(year > 2016) %>%
#  select(-c(year))
#select(-c("year", "Population (1000 persons)"))
select(-c("Population (1000 persons)"))
colnames(dfMod)
charCols <- c(1, 2, 4)
dfMod[, -charCols] <- as.data.frame(apply(dfMod[, -charCols], 2, log))
infNanLook <- apply(dfMod[, -charCols], 2, isInfNan)
infNanLook
indInf <- c(which(is.infinite(dfMod$Pulses)), which(is.infinite(dfMod$`Alcoholic Beverages`)))
dfMod[, -charCols] <- dfMod[, -charCols] %>% apply(2, replaceInf) %>% as.data.frame()
rmRows <- c(which(is.na(dfMod$Oilcrops)), which(is.na(dfMod$`Alcoholic Beverages`)))
rmRows <- unique(rmRows)
if(length(rmRows) != 0){dfMod <- dfMod[-rmRows, ]}
#---
indRm <- which(dfMod$area %in% c("Mali", "South Sudan"))
dfMod <- dfMod[-indRm, ]
#---
rmCharCols <- which(colnames(dfMod) %in% c("area", "Cat"))
dfModHid <- dfMod %>% subset(Cat == "Hid")
ctyVecHid <- dfModHid$area
dfModHid <- dfModHid %>% select(-rmCharCols)
dfModChr <- dfMod %>% subset(Cat == "Chr")
ctyVecChr <- dfModChr$area
dfModChr <- dfModChr %>% select(-rmCharCols)
dfModOve <- dfMod %>% subset(Cat == "OverDev")
ctyVecOve <- dfModOve$area
dfModOve <- dfModOve %>% select(-rmCharCols)
#---
notThese <- c(1, 2)
dfModChr[, -notThese] <- as.data.frame(apply(dfModChr[, -notThese], 2, refFn))
mod <- lm.cluster(dfModChr, `DALYs/100,000 capita`~.-year,
                  cluster = "year")
mod <- lm(`DALYs/100,000 capita`~.-year, dfModChr)
texreg::extract(mod)
# create table in stargazer
summary(mod, cluster = "year")
library(stargazer)
stargazer(mod, se = list(coef(summary(mod, cluster = "year"))[, 2]), type = "text")
#---
dfModHid[, -notThese] <- as.data.frame(apply(dfModHid[, -notThese], 2, refFn))
mod <- lm.cluster(dfModHid, `DALYs/100,000 capita`~.-year,
                  cluster = "year")
#summary(mod)
texreg::extract(mod)
mod <- lm(`DALYs/100,000 capita`~.-year, dfModHid)
summary(mod, cluster = "year")

#---
dfModOve[, -notThese] <- as.data.frame(apply(dfModOve[, -notThese], 2, refFn))
mod <- lm.cluster(dfModOve, `DALYs/100,000 capita`~.-year,
                  cluster = "year")
#summary(mod)
texreg::extract(mod)
mod <- lm(`DALYs/100,000 capita`~.-year, dfModOve)
summary(mod, cluster = "year")

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
#export_summs(mod, model.names = c("Hidden\nHunger"), to.file = "docx", file.name = paste0(picFolder, "mod2Results.docx"))
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
            #"GDP/capita (USD 2015 prices)",
            "Food supply (kcal/capita/day)"))
# dfMod <- merge(dfHidHung, dfFBSmacNut) %>% subset(year == thisYr) %>%
#   select(-c("area", "year", #"GDP/capita (USD 2015 prices)",
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
export_summs(modChr, modHid, modOve, model.names = c("Chronic\nHunger", "Hidden\nHunger", "Over-dev"), to.file = "docx", file.name = paste0(picFolder, "mod3Results.docx"))
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
           "GDP/capita (USD 2015 prices)"))

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
                         size = `GDP/capita (USD 2015 prices)`))#,
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
thisFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/DALYs/Hunger DALY data/"
theseFiles <- paste0(thisFolder, c("IHME-GBD_2019_ALLc1990-2019regions-"), c(1:6), ".csv")
listDf <- lapply(theseFiles, read.csv)
dfCauseRegRaw <- as.data.frame(do.call(rbind, listDf))
theseFiles <- paste0(thisFolder, c("IHME-GBD_2019_ALLr1990-2019regions-"), c(1:2), ".csv")
listDf <- lapply(theseFiles, read.csv)
dfRiskRegRaw <- as.data.frame(do.call(rbind, listDf))
#---------------------------------------------------------------
# From risk factor data get zinc, iron, vit. A deficiency DALYs
# u <- dfRiskRawReg$rei
# unique(u[grep("deficiency", u)])
keepThese <- c("Vitamin A deficiency",
               "Zinc deficiency",
               "Iron deficiency",
               "Child underweight")
dfRiskReg <- dfRiskRegRaw %>% subset(rei %in% keepThese) %>%# &
                                 #age == "All ages" &
                                 #metric == "Rate") %>%
  select(c("location", "rei", "year", "age", "metric", "val"))

dfUnderWgtU5Reg <- dfRiskReg %>%
  subset(rei == "Child underweight" &
           age == "<5 years" &
           metric == "Number") %>%
  rename(valU5 = val) %>%
  select(location, year, valU5)

dfUnderWgtAllReg <- dfRiskReg %>%
  subset(rei == "Child underweight" &
           age == "All ages" &
           metric == "Rate") %>%
  select(location, year, val)
#---
# Also get other (possibly overdevelopment) dietary DALYs
keepThese <- c("Diet high in sugar-sweetened beverages",
               "Diet high in processed meat",
               "Diet low in fiber",
               "Diet low in legumes",
               "Diet low in fruits",
               "Diet low in vegetables",
               "Diet low in whole grains",
               "Diet low in nuts and seeds",
               #"Diet low in seafood omega-3 fatty acids",
               "Diet low in polyunsaturated fatty acids",
               "Diet high in trans fatty acids",
               "Diet high in sodium")
dfOverDevReg <- dfRiskRegRaw %>% subset(rei %in% keepThese &
                                    age == "All ages" &
                                    metric == "Rate") %>%
  select(c("location", "year", "val")) %>%
  group_by(location, year) %>%
  summarize(val = sum(val))
dfOverDevReg$Cat <- "Overnut"
#---
# From cause data get iodine deficiency, protein-energy malnutrition,
# and other deficiencies
keepThese <- c("Iodine deficiency",
               "Protein-energy malnutrition",
               "Other nutritional deficiencies")
dfCauseReg <- dfCauseRegRaw %>% subset(cause %in% keepThese &
                                   age != "Age-standardized" &
                                   metric == "Number") %>%
  select(c("location", "cause", "year", "age", "val"))
dfPEMo5Reg <- dfCauseReg %>%
  subset(cause == "Protein-energy malnutrition") %>%
  spread(age, val) %>%
  mutate(valO5 = `All ages` - `<5 years`) %>%
  select(-c(`All ages`, `<5 years`))
dfCauseRegPop <- dfCauseRegRaw %>% subset(cause == "Cardiovascular diseases" &
                                      age == "All ages") %>%
  select(c("location", "cause", "year", "metric", "val")) %>%
  spread(metric, val) %>%
  mutate(Pop100thous = Number / Rate) %>%
  select(-c(Number, Rate, cause))
dfPEMo5Reg <- dfPEMo5Reg %>% merge(dfCauseRegPop) %>%
  select(location, year, valO5, Pop100thous)
dfChrReg <- merge(dfPEMo5Reg, dfUnderWgtU5Reg) %>%
  mutate(val = (valO5 + valU5) / Pop100thous) %>%
  select(location, year, val)
dfChrReg$Cat <- "Chr"
dfChrReg2 <- dfUnderWgtAllReg
dfChrReg2$Cat <- "Chr2"
#---
dfHid1 <- dfRiskReg %>%
  subset(rei != "Child underweight" &
           metric == "Rate" &
           age == "All ages") %>%
  rename(issue = rei) %>%
  select(location, year, issue, val)
dfHid2 <- dfCauseRegRaw %>%
  subset(cause %in% c("Iodine deficiency",
                      "Other nutritional deficiencies") &
                                     age == "All ages" &
                                     metric == "Rate") %>%
  rename(issue = cause) %>%
  select(location, year, issue, val)
dfHidReg <- as.data.frame(rbind(dfHid1, dfHid2)) %>%
  group_by(location, year) %>%
  summarise(val = sum(val)) %>%
  select(location, year, val)
dfHidReg$Cat <- "Hid"
#----------------------------------------------------------
listDf <- list(dfChrReg, dfHidReg, dfOverDevReg)
dfDalyG <- as.data.frame(do.call(rbind, listDf))
colnames(dfDalyG)[1] <- "area"
keepThese <- c("Sub-Saharan Africa - WB",
               "South Asia - WB",
               "Latin America & Caribbean - WB",
               "Middle East & North Africa - WB",
               "East Asia & Pacific - WB",
               "Europe & Central Asia - WB",
               "North America",
               "Global")
dfDalyG <- dfDalyG %>% subset(area %in% keepThese)
dfPlot <- dfDalyG
dfPlot$area <- gsub(" - WB", "", dfPlot$area)
colnames(dfPlot)[which(colnames(dfPlot) == "val")] <- "DALYs/100,000 people"
#---
gg <- ggplot(dfPlot, aes(x = year, y = `DALYs/100,000 people`,
                         group = Cat, color = Cat))
gg <- gg + geom_line(lwd = 1)
gg <- gg + facet_wrap(~area, ncol = 4, scales = "free_y")
gg <- gg + theme_bw()
gg <- gg + theme(legend.position = "bottom",
                 legend.title = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_text(size = axisTitleSize),
                 axis.text = element_text(size = axisTextSize),
                 legend.text = element_text(size = legendTextSize),
                 strip.text = element_text(size = facetTitleSize))
gg

saveFile <- "overView.png"
saveTo <- paste0(picFolder, saveFile)
ggsave(saveTo, width = 7, height = 4)
#---
# Proof that Godecke et al.'s convoluted calculation of
# chronic hunger DALYs is equal to all age DALYs attributable to
# child underweight
dfX <- as.data.frame(rbind(dfChrReg, dfChrReg2)) %>%
  rename(area = location) %>%
  subset(area %in% keepThese)
gg <- ggplot(dfX, aes(x = year, y = val, group = Cat, color = Cat))
gg <- gg + geom_line()
gg <- gg + facet_wrap(~area)
gg
dfX <- dfX %>% spread(Cat, val) %>%
  mutate(dif = Chr - Chr2)
#---
# Let's look at age-standardized DALYs
dfChrReg3 <- dfRiskReg %>%
  subset(rei == "Child underweight" &
           age == "Age-standardized" &
           metric == "Rate") %>%
  select(location, year, val)
dfChrReg3$Cat <- "Age-standardized"
dfChrReg2$Cat <- "All ages"
dfX <- as.data.frame(rbind(dfChrReg3, dfChrReg2)) %>%
  rename(area = location) %>%
  subset(area %in% keepThese)
gg <- ggplot(dfX, aes(x = year, y = val, group = Cat, color = Cat))
gg <- gg + geom_line()
gg <- gg + facet_wrap(~area, nrow = 2, scales = "free_y")
gg <- gg + labs(title = "DALYs/100,000 due to child underweight")
gg <- gg + theme_bw()
gg <- gg + theme(legend.position = "top",
                 legend.title = element_blank(),
                 axis.title = element_blank())
gg
saveFile <- "Child underwgt age standardized.png"
saveTo <- paste0(picFolder, saveFile)
ggsave(saveTo, width = 7, height = 4)
#==================================================================
# Bar graph main causes of DALYs by region
# thisFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/DALYs/Hunger DALY data/"
# thisFile <- "IHME-GBD_2019_DATA-c1990-2019regions.csv"
# thisFilePath <- paste0(thisFolder, thisFile)
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
dfC <- dfCauseRegRaw %>%
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
thisFilePath <- paste0(thisFolder, thisFile)
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

saveFile <- "riskFactrContrib.png"
saveTo <- paste0(picFolder, saveFile)
ggsave(saveTo, width = 8, height = 6)
# <- c("Chronic kidney disease", "Diabetes mellitus",
#   "Total cancers", "Maternal and neonatal disorders",
#   "Sudden infant death syndrome",
#   "Ischemic heart disease", "Stroke",
#   "Total burden related to Non-alcoholic fatty liver disease (NAFLD)",
#   "Blindness and vision loss",
#   "Total burden related to hepatitis B",
#   "Total burden related to hepatitis C")