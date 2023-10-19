library(tidyverse)
#https://vizhub.healthdata.org/gbd-results/
thisFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/DALYs/Hunger DALY data/"
thisFile <- "IHME-GBD_2019_DATA-fd8d0002-1.csv"
thisFilePath <- paste0(thisFolder, thisFile)
dfRaw <- read.csv(thisFilePath, stringsAsFactors = F)
dfHidHunger <- dfRaw %>% subset(metric == "Number" &
               measure == "DALYs (Disability-Adjusted Life Years)" &
                 cause == "Nutritional deficiencies") %>%
  select(c("location", "year", "val")) %>%
  rename(area = location, `NutDef (DALYs)` = val)
#---------------------------------------------------------------------
# keepCols <- c(colnames(df)[grep("name", colnames(df))], "year", "val")
# df <- df[, keepCols]
# colnames(df) <- gsub("_name", "", colnames(df))
# theseAreas <- c("Sub-Saharan Africa", "North Africa and Middle East",
#                 "South Asia", "Latin America and Caribbean",
#                 "Central Europe, Eastern Europe, and Central Asia",
#                 "Southeast Asia, East Asia, and Oceania")
# theseCauses <- c("Protein-energy malnutrition",
#                  "Other nutritional deficiencies",
#                  "Iodine deficiency",   
#                  "Vitamin A deficiency",
#                  "Dietary iron deficiency")
# dfPlot <- df %>% subset(location %in% theseAreas &
#                    metric == "Number" &
#                    measure == "DALYs (Disability-Adjusted Life Years)" &
#                    cause %in% theseCauses &
#                      age == "All ages") %>%
#   select(c("location", "cause", "year", "val"))
# 
# dfWld <- dfPlot %>% group_by(cause, year) %>%
#   summarise(val = sum(val)) %>%
#   mutate(location = "World") %>%
#   select(colnames(dfPlot))
# dfPlot <- dfPlot %>% rbind(dfWld) %>% as.data.frame()
# 
# gg <- ggplot(dfPlot, aes(x = year, y = val,
#                          fill = cause))
# gg <- gg + geom_area(position = "stack")
# gg <- gg + facet_wrap(~location, scales = "free_y")
# gg
#
#---------------------------------------------------------------------
# thisFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/DALYs/csvs4analysis/"
# theseFiles <- list.files(thisFolder)
# listDfs <- list()
# for(i in 1:length(theseFiles)){
#   thisFile <- theseFiles[i]
#   thisFilePath <- paste0(thisFolder, thisFile)
#   thisDf <- read.csv(thisFilePath, stringsAsFactors = F)
#   listDfs[[i]] <- thisDf
# }
# 
# df <- as.data.frame(do.call(rbind, listDfs))
# df <- subset(df, measure == "DALYs (Disability-Adjusted Life Years)")

#========================================================================
# FAO
library(FAOSTAT)
dfKey <- FAOsearch()
thisFolder <- "FAOSTAT data"
dir.create(thisFolder)
# Food balance sheets (have to unite old and new files)
# Note these include population
dfRaw <- get_faostat_bulk(code = "FBS", data_folder = thisFolder)
keepCols <- c("area", "year", "item", "element", "unit", "value")
dfFBS2 <- dfRaw[, keepCols]; rm(dfRaw)
dfRaw <- get_faostat_bulk(code = "FBSH", data_folder = thisFolder)
keepCols <- c("area", "year", "item", "element", "unit", "value")
dfFBS1 <- dfRaw[, keepCols]
dfFBS <- dfFBS2; rm(dfFBS2)
dfPop <- subset(dfFBS, item == "Population")
# Rectification of names
# setdiff(dfFBS$area, dfHidHunger$area)
# setdiff(dfHidHunger$area, dfFBS$area)
# unique(dfHidHunger$area)
# unique(dfFBS$area)
dfHidHunger$area <- gsub("Turkey", "Türkiye", dfHidHunger$area)
dfFBS$area[grep("United Kingdom", dfFBS$area)] <- "United Kingdom"
notThese <- c("China, Hong Kong SAR", "China, Macao SAR",
              "China, Taiwan Province of", "China, mainland")
dfFBS <- subset(dfFBS, !(area %in% notThese))
#unique(dfHidHunger$area[grep("China", dfHidHunger$area)])
dfHidHunger <- subset(dfHidHunger, !(area %in% c("Taiwan (Province of China)")))
#=====================================================================
# GDP/capita
dfRaw <- get_faostat_bulk(code = "MK", data_folder = thisFolder)
keepCols <- c("area", "year", "item", "element", "unit", "value")
dfGDP <- dfRaw[, keepCols] %>% subset(item == "Gross Domestic Product" &
                                        element == "value_us__per_capita__2015_prices") %>%
  select(c("area", "year", "value")) %>%
  rename(`GDP/capita (USD 2015 prices)` = value)
dfFBS <- merge(dfFBS, dfGDP)
#=====================================================================
# Get df for commodity model
theseItems <- c("Cereals - Excluding Beer",
                "Starchy Roots", "Vegetables",
                "Oilcrops", "Fruits - Excluding Wine", "Pulses",
                "Sugar & Sweeteners", "Animal Products")
dfFBScommod <- dfFBS %>% subset(item %in% theseItems &
                                  element == "food_supply__kcal_capita_day_") %>%
  select(c("area", "year", "item", "value", "GDP/capita (USD 2015 prices)")) %>%
  spread(item, value) %>%
  mutate(`F&V` = `Fruits - Excluding Wine` + Vegetables) %>%
  select(-c("Vegetables", "Fruits - Excluding Wine")) %>%
  rename(Cereals = `Cereals - Excluding Beer`)
# Get df for macronutrient model
theseElems <- c("fat_supply_quantity__g_capita_day_",
                "protein_supply_quantity__g_capita_day_",
                "food_supply__kcal_capita_day_")
dfFBSmacNut <- subset(dfFBS, item == "Grand Total" &
                        element %in% theseElems)
# Convert g to kcal
dfFBSmacNut$value[grep("protein", dfFBSmacNut$element)] <- 4 *
  dfFBSmacNut$value[grep("protein", dfFBSmacNut$element)]
dfFBSmacNut$value[grep("fat", dfFBSmacNut$element)] <- 9 *
  dfFBSmacNut$value[grep("fat", dfFBSmacNut$element)]
dfFBSmacNut$element[grep("protein", dfFBSmacNut$element)] <- "Protein supply (kcal/capita/day)"
dfFBSmacNut$element[grep("fat", dfFBSmacNut$element)] <- "Fat supply (kcal/capita/day)"
dfFBSmacNut$element[grep("food", dfFBSmacNut$element)] <- "Food supply (kcal/capita/day)"
dfFBSmacNut$unit <- NULL
# Calculate carb kcal
dfFBSmacNut <- dfFBSmacNut %>% spread(element, value)
dfFBSmacNut$`Carb supply (kcal/capita/day)` <- dfFBSmacNut$`Food supply (kcal/capita/day)` -
  dfFBSmacNut$`Protein supply (kcal/capita/day)` - dfFBSmacNut$`Fat supply (kcal/capita/day)`
dfFBSmacNut$item <- NULL
# Create % of diet variable
dfFBSmacNutPct <- dfFBSmacNut %>% select(-"Food supply (kcal/capita/day)")
dfFBStot <- dfFBS %>% subset(item == "Grand Total" &
                               element == "food_supply__kcal_capita_day_") %>%
  select(c("area", "year", "value")) %>% rename(total = value)
gathercols <- colnames(dfFBSmacNutPct)[3:ncol(dfFBSmacNutPct)]
dfFBSmacNutPct <- dfFBSmacNutPct %>% gather_("item", "value", gathercols)
dfFBSmacNutPct$item[grep("Protein", dfFBSmacNutPct$Item)] <- "Protein"
dfFBSmacNutPct$item[grep("Fat", dfFBSmacNutPct$Item)] <- "Fat"
dfFBSmacNutPct$item[grep("Carb", dfFBSmacNutPct$Item)] <- "Carbohydrate"
dfFBSmacNutPct <- dfFBSmacNutPct %>% merge(dfFBStot) %>%
  mutate(`Diet share (%)` = round(100 * value / total, 2)) %>%
  select(-c("value", "total")) %>%
  spread(item, `Diet share (%)`)
#=======================================================================
# SDG data
dfRaw <- get_faostat_bulk(code = "SDGB", data_folder = thisFolder)
keepCols <- c("area", "year", "item", "element", "unit", "value")
rmRows <- which(dfRaw$value == "NaN")
dfSDGB <- dfRaw[-rmRows, keepCols]
dfSDGB <- dfSDGB %>%
  #subset(element == "value__2017_constant_prices_") %>%
  subset(element == "value") %>%
  #  select(-c("element", "unit")) %>%
  group_by(area, item) %>%
  mutate(nYrs = length(value)) %>% as.data.frame()
max(dfSDGB$nYrs)
keepRows <- which(dfSDGB$nYrs >= (max(dfSDGB$nYrs) - 3))
dfSDGB <- dfSDGB[keepRows, ]
unique(dfSDGB$item)
dfSDGB$nYrs <- NULL
#========================================================================
# Model year
thisYr <- 2019
# Model 1
# Hunger ~ Commodity
dfMod <- merge(dfHidHunger, dfFBScommod) %>% subset(year == thisYr) %>%
 select(-c("area", "year", "GDP/capita (USD 2015 prices)", "Oilcrops",
           "Sugar & Sweeteners"))
 #select(-c("area", "year"))
dfMod <- as.data.frame(apply(dfMod, 2, log))
isInfNan <- function(x){
  nInfNan <- sum(is.nan(x) > 0) + sum(is.infinite(x) > 0)
  return(nInfNan)
}
infNanLook <- apply(dfMod, 2, isInfNan)
rmRows <- which(is.infinite(dfMod$Pulses))
dfMod <- dfMod[-rmRows, ]
mod <- lm(`NutDef (DALYs)` ~.-1, dfMod)
summary(mod)
car::vif(mod)
#---
dfPca <- dfMod %>% select(-c("NutDef (DALYs)"))
isNA <- function(x){
  nNA <- sum(is.na(x) > 0)
  return(nNA)
}
naLook <- apply(dfPca, 2, isNA)
rmRows <- which(is.na(dfPca$Pulses))
dfPca <- dfPca[-rmRows, ]
p <- princomp(dfPca)

res <- FactoMineR::PCA(dfPca, ncp = 5, graph = T)
eigvals <- as.data.frame(res$eig)$eigenvalue
mat_loads <- res$var$coord
mat_loads_rot <- varimax(mat_loads)[[1]]
mat_eigvecs <- mat_loads %*% diag(1 / sqrt(eigvals))

p$loadings
#---
dfModa <- dfMod %>% select(-"NutDef (DALYs)")
# dfModa <- dfMod %>% select(c("GDP/capita (USD 2015 prices)",
#                              "Animal Products",
#                              "Sugar & Sweeteners"))
mod <- lm(`GDP/capita (USD 2015 prices)` ~., dfModa)
summary(mod)
car::vif(mod)
#========================================================================
# Model 2
# Hunger ~ Macronutrient
dfMod <- merge(dfHidHunger, dfFBSmacNut) %>% subset(year == thisYr) %>%
  select(-c("area", "year", "GDP/capita (USD 2015 prices)"))
  #select(-c("area", "year"))
dfMod <- as.data.frame(apply(dfMod, 2, log))
infNanLook <- apply(dfMod, 2, isInfNan)
dfModa <- dfMod %>% select(-"Food supply (kcal/capita/day)")
mod <- lm(`NutDef (DALYs)` ~.-1, dfModa)
summary(mod)
car::vif(mod)
plot(mod$fitted.values, mod$residuals)
#========================================================================
#========================================================================
#========================================================================
#========================================================================
#========================================================================
matXY <- dfFBSmacNut %>%
  merge(dfFBScommod) %>%
  merge(dfHidHunger) %>%
  subset(year == thisYr) %>%
  select(-c("area", "year",
            "GDP/capita (USD 2015 prices)",
            "Food supply (kcal/capita/day)")) %>%
  apply(2, log)
isNANaNinf <- function(x){
  nNANaNinf <- sum(is.na(x) > 0) + sum(is.nan(x) > 0) +
    sum(is.infinite(x) > 0)
  return(nNANaNinf)
}
rowsNANaNinf <- function(x){
  indNANaNinf <- c(which(is.na(x)),
                 which(is.nan(x)),
                 which(is.infinite(x)))
  return(indNANaNinf)
}

nNANaNinf <- apply(matXY, 2, isNANaNinf)
nNANaNinf
colInd <- which(nNANaNinf > 0)
for(i in 1:length(colInd)){
  rmRows <- rowsNANaNinf(matXY[, colInd[i]])
  matXY <- matXY[-rmRows, ]
}
nNANaNinf <- apply(matXY, 2, isNANaNinf)
nNANaNinf
matXY <- matXY %>% apply(2, scale)
#---
#sigX <- apply(Xcommod, 2, sd)
#sigY <- apply(XmacNut, 2, sd)
#colnames(matXY)
colsX <- c("Animal Products", "Cereals", "Oilcrops", "Pulses", "Starchy Roots", "Sugar & Sweeteners", "F&V")
colsY <- c("Fat supply (kcal/capita/day)", "Protein supply (kcal/capita/day)", "Carb supply (kcal/capita/day)")
X <- matXY[, colsX]
Y <- matXY[, colsY]
Kxx <- cor(X)
eigDecomp <- eigen(Kxx)
eigVals <- eigDecomp$values
G <- diag(eigVals)
P <- eigDecomp$vectors
L <- P %*% sqrt(G)
colnames(L) <- paste0("PC", 1:ncol(X))
row.names(L) <- row.names(Kxx)
# Lrot <- varimax(L)[[1]]
# Lrot <- matrix(as.numeric(Lrot),
#                    attributes(Lrot)$dim,
#                    dimnames = attributes(Lrot)$dimnames)
# # mat_R <- varimax(mat_L)[[2]]
# # mat_R <- matrix(as.numeric(mat_R),
# #                 attributes(mat_R)$dim,
# #                 dimnames = attributes(mat_R)$dimnames)
varNames <- row.names(L)
gathercols <- colnames(L)
dfBar <- as.data.frame(L) %>% mutate(var = varNames) %>%
  gather(PC, val, gathercols)
indCutOff <- which(cumsum(eigVals) / sum(eigVals) > 0.95)[1]
dfBar <- subset(dfBar, PC %in% paste0("PC", c(1:indCutOff)))
gg <- ggplot(dfBar, aes(x = var, y = val))
gg <- gg + geom_bar(stat = "identity")
gg <- gg + geom_hline(yintercept = 0, color = "red")
gg <- gg + coord_flip()
gg <- gg + facet_wrap(~PC, nrow = 1)
gg
#---
n <- nrow(X)
Kxy <- 1 / (n - 1) * (t(X) %*% Y); Lrz <- Kxy
varNames <- row.names(Lrz)
gathercols <- colnames(Lrz)
dfBar <- as.data.frame(Lrz) %>% mutate(var = varNames) %>%
  gather(PC, val, gathercols)
gg <- ggplot(dfBar, aes(x = var, y = val))
gg <- gg + geom_bar(stat = "identity")
gg <- gg + geom_hline(yintercept = 0, color = "red")
gg <- gg + coord_flip()
gg <- gg + facet_wrap(~PC, nrow = 1)
gg
#---
m <- ncol(Kxy)
Ptilde <- P[, 1:m]
GtildeInv <- diag(1 / eigVals[1:m])
RZ <- GtildeInv^(1 / 2) %*% t(Ptilde) %*% Kxy
outQR <- qr(RZ)
R <- qr.Q(outQR)
Z <- qr.R(outQR)
Zinv <- solve(Z)
Lr <- Kxy %*% Zinv
colnames(Lr) <- colnames(Kxy)
dfBar <- as.data.frame(Lr) %>% mutate(var = varNames) %>%
  gather(PC, val, gathercols)
gg <- ggplot(dfBar, aes(x = var, y = val))
gg <- gg + geom_bar(stat = "identity")
gg <- gg + geom_hline(yintercept = 0, color = "red")
gg <- gg + coord_flip()
gg <- gg + facet_wrap(~PC, nrow = 1)
gg
#======================================================================
# Model 3
matMod <- matXY[, c("NutDef (DALYs)", colsY)]
matMod[, -1] <- matMod[, -1] %*% Zinv
dfMod <- as.data.frame(matMod)
mod <- lm(`NutDef (DALYs)` ~.-1, dfMod)
summary(mod)
car::vif(mod)
plot(mod$fitted.values, mod$residuals)








sNames <- c("Carb supply (kcal/capita/day)",
                           "Fat supply (kcal/capita/day)",
                           "Protein supply (kcal/capita/day)")
n <- length(varNames)
sigS <- apply(XmacNut, 2, sd)
Lrot <- 1 / (n - 1) * diag(1 / sigX) %*% t(Xcommod) %*% XmacNut %*% diag(1 / sigS)
colnames(Lrot) <- sNames
# Fix scale so that correlations fall between -1 and 1
# scaleFactr <- max(Lrot)
# Lrot <- 1 / scaleFactr * Lrot
B <- t(L[, 1:3]) %*% Lrot
outQR <- qr(B)
Q <- qr.Q(outQR)
R <- qr.R(outQR)
colnames(R) <- paste0("V", c(1:3))
Lrot <- Lrot %*% solve(R)
t(L[, 1:3]) %*% Lrot - Q
Lrot - L[, 1:3] %*% Q
t(L[, 1:3]) %*% L[, 1:3]
B <- Q
SigSrot <- t(Lrot) %*% Lrot
eigDecomp <- eigen(SigSrot)
gamTilde <- eigDecomp$values
sigS / sqrt(gamTilde)
B <- eigDecomp$vectors
Lrot - L[, 1:3] %*% t(B)
colnames(L) <- sNames
gathercols <- colnames(L)
dfBar <- as.data.frame(L) %>% mutate(var = varNames) %>%
  gather(PC, val, gathercols)
# indCutOff <- which(cumsum(gamTilde) / sum(gamTilde) > 0.95)[1]
# indCutOff <- which(cumsum(sigS^2) / sum(sigS^2) > 0.95)[1]
#dfBar <- subset(dfBar, PC %in% paste0("V", c(1:indCutOff)))
gg <- ggplot(dfBar, aes(x = var, y = val))
gg <- gg + geom_bar(stat = "identity")
gg <- gg + geom_hline(yintercept = 0, color = "red")
gg <- gg + coord_flip()
gg <- gg + facet_wrap(~PC, nrow = 1)
#gg <- gg + theme(axis.text.x = element_text(angle = 60, hjust = 1))
gg

#














# # WDI
# library(WDI)
# theseIndicators <- c("Access to electricity (% of total population)",
#                                             "Age dependency ratio, old (% of working-age population)",
#                                             "Age dependency ratio, young (% of working-age population)",
#                                             "Life expectancy at birth, total (years)",
#                                             "Employment in agriculture (% of total employment) (modeled ILO estimate)",
#                                             "Employment in industry (% of total employment) (modeled ILO estimate)",
#                                             "Employment in services (% of total employment) (modeled ILO estimate)",
#                                             "Ratio of female to male labor force participation rate (%) (modeled ILO estimate)",
#                                             "Total natural resources rents (% of GDP)",
#                                             "School enrollment, primary (% gross)",
#                                             "School enrollment, secondary (% gross)",
#                                             "Proportion of seats held by women in national parliaments (%)",
#                                             "Individuals using the Internet (% of population)")
# 
# theseIndicators <- gsub("\\)", "\\\\)", theseIndicators)
# theseIndicators <- gsub("\\(", "\\\\(", theseIndicators)
# theseIndicators <- gsub("\\%", "\\\\%", theseIndicators)
# listDf <- list()
# for(i in 1:length(theseIndicators)){
#   listDf[[i]] <- WDIsearch(theseIndicators[i])
# }
# dfKey <- as.data.frame(do.call(rbind, listDf))
# 
# dfWDI <- WDI(country = "all", indicator = dfKey$indicator)
# dfWDI$iso2c <- NULL; dfWDI$iso3c <- NULL
# # gatherCols <- colnames(df)[3:ncol(df)]
# # df <- df %>% gather_("var", "val", gatherCols)
# #========================================================================
# # FAOSTAT
# library(FAOSTAT)
# # item_vec <- c("Gross Domestic Product per capita",
# #               #"Gross Fixed Capital Formation",
# #               "Value Added (Agriculture, Forestry and Fishing)",
# #               #"Value Added (Manufacture of food and beverages)",
# #               #"Value Added (Agriculture)",
# #               "Value Added (Total Manufacturing)")
# # this_element <- "Value US$, 2015 prices"
# 
# dfKey <- FAOsearch()
# thisFolder <- "FAOSTAT data"
# dir.create(thisFolder)
# dfSDGB <- get_faostat_bulk(code = "SDGB", data_folder = thisFolder)
# keepCols <- c("area", "year", "item", "element", "unit", "value")
# dfSDGB <- dfSDGB[, keepCols]
# dfCheckNA <- dfSDGB %>% group_by(area, item, element) %>%
#   mutate(nNA = sum(is.na(value))) %>% as.data.frame()
# dfCheckNA <- dfCheckNA[-which(duplicated(dfCheckNA$nNA)),]
# unique(dfSDGB$year)
# 
# 
# 
# df_macro <- subset(df_raw_macro, Item %in% item_vec &
#                      Year %in% year_vec &
#                      Element == this_element)
# #========================================================================
