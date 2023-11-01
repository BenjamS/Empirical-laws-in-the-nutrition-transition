library(tidyverse)
library(FAOSTAT)
#https://vizhub.healthdata.org/gbd-results/
thisFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/DALYs/Hunger DALY data/"
thisFile <- "IHME-GBD_2019_DATA-fd8d0002-1.csv"
thisFilePath <- paste0(thisFolder, thisFile)
dfRaw <- read.csv(thisFilePath, stringsAsFactors = F)
hidHungVec <- c("Other nutritional deficiencies",
                "Iodine deficiency",
                "Vitamin A deficiency",          
                "Dietary iron deficiency")
chrHungVec <- c("Protein-energy malnutrition")
dfHung <- dfRaw %>% subset(metric == "Number" &
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
# dfKey <- FAOsearch()
thisFolder <- "FAOSTAT data"
dir.create(thisFolder)
# Food balance sheets (have to unite old and new files)
# Note these include population
dfRaw <- get_faostat_bulk(code = "FBS", data_folder = thisFolder)
keepCols <- c("area", "year", "item", "element", "unit", "value")
dfFBS2 <- dfRaw[, keepCols]; rm(dfRaw)
# dfRaw <- get_faostat_bulk(code = "FBSH", data_folder = thisFolder)
# keepCols <- c("area", "year", "item", "element", "unit", "value")
# dfFBS1 <- dfRaw[, keepCols]
dfFBS <- dfFBS2; rm(dfFBS2)
# dfPop <- dfFBS %>% subset(item == "Population" &
#                   element == "total_population___both_sexes") %>%
#   rename(Population = value) %>%
#   select("area", "year", "Population")
# Rectification of names
# setdiff(dfFBS$area, dfHidHung$area)
# setdiff(dfHidHung$area, dfFBS$area)
# unique(dfHidHung$area)
# unique(dfFBS$area)
dfHung$area <- gsub("Turkey", "Türkiye", dfHung$area)
dfFBS$area[grep("United Kingdom", dfFBS$area)] <- "United Kingdom"
notThese <- c("China, Hong Kong SAR", "China, Macao SAR",
              "China, Taiwan Province of", "China, mainland")
dfFBS <- subset(dfFBS, !(area %in% notThese))
#unique(dfHung$area[grep("China", dfHung$area)])
dfHung <- subset(dfHung, !(area %in% c("Taiwan (Province of China)")))
# # Divide hidden hunger by population
# dfHidHung <- dfHidHung %>% merge(dfPop) %>%
#   mutate(`NutDef (DALYs) per 1000 capita` = `NutDef (DALYs)` / Population) %>%
#   select(-c("Population", "NutDef (DALYs)"))
#=====================================================================
# GDP/capita
dfRaw <- get_faostat_bulk(code = "MK", data_folder = thisFolder)
keepCols <- c("area", "year", "item", "element", "unit", "value")
dfGDP <- dfRaw[, keepCols] %>% subset(item == "Gross Domestic Product" &
                                        element == "value_us__per_capita__2015_prices") %>%
  select(c("area", "year", "value")) %>%
  rename(`GDP/capita (USD 2015 prices)` = value)
#dfFBS <- merge(dfFBS, dfGDP)
#=====================================================================
# Get df for commodity model
theseItems <- c("Cereals - Excluding Beer",
                "Starchy Roots", "Vegetables",
                "Oilcrops", "Fruits - Excluding Wine", "Pulses",
                "Animal Products",
                #"Sugar & Sweeteners",
                "Grand Total",
                "Population")
dfFBScommod <- dfFBS %>% subset(item %in% theseItems &
                                  element %in% c("food_supply__kcal_capita_day_",
                                                 "total_population___both_sexes")) %>%
  select(c("area", "year", "item", "value"))#, "GDP/capita (USD 2015 prices)"))
dfResidX <- dfFBScommod %>% subset(!(item %in% c("Population", "Grand Total"))) %>%
  group_by(area, year) %>% summarize(value = sum(value, na.rm = T)) %>%
  rename(main = value) %>%
  select(c("area", "year", "main"))
dfResid <- dfFBScommod %>% subset(item == "Grand Total") %>%
  #select(-"GDP/capita (USD 2015 prices)") %>%
  merge(dfResidX) %>% mutate(value = value - main) %>%
  mutate(item = "Residual") %>%
  select(c("area", "year", "item", "value"))
dfFBScommod <- dfFBScommod %>%
  subset(item != "Grand Total") %>%
  #select(-"GDP/capita (USD 2015 prices)") %>%
  rbind(dfResid) %>% as.data.frame() %>%
  spread(item, value) %>%
  mutate(`F&V` = `Fruits - Excluding Wine` + Vegetables) %>%
  select(-c("Vegetables", "Fruits - Excluding Wine")) %>%
  rename(Cereals = `Cereals - Excluding Beer`,
         `Population (1000 persons)` = Population)
rm(dfResidX, dfResid)
# Get df for macronutrient model
theseElems <- c("fat_supply_quantity__g_capita_day_",
                "protein_supply_quantity__g_capita_day_",
                "food_supply__kcal_capita_day_",
                "total_population___both_sexes")
dfFBSmacNut <- subset(dfFBS, item %in% c("Grand Total", "Population") &
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
dfFBSmacNut$item <- NULL
# Calculate carb kcal
dfFBSmacNut <- dfFBSmacNut %>% spread(element, value)
colnames(dfFBSmacNut)[ncol(dfFBSmacNut)] <- "Population (1000 persons)"
dfFBSmacNut$`Carb supply (kcal/capita/day)` <- dfFBSmacNut$`Food supply (kcal/capita/day)` -
  dfFBSmacNut$`Protein supply (kcal/capita/day)` - dfFBSmacNut$`Fat supply (kcal/capita/day)`
# # Create % of diet variable
dfPop <- dfFBSmacNut %>% select("area", "year", "Population (1000 persons)")
dfFBSmacNutPct <- dfFBSmacNut %>% select(-c("Food supply (kcal/capita/day)",
                                            "Population (1000 persons)"))
dfFBStot <- dfFBS %>% subset(item == "Grand Total" &
                               element == "food_supply__kcal_capita_day_") %>%
  select(c("area", "year", "value")) %>% rename(total = value)
gathercols <- colnames(dfFBSmacNutPct)[3:ncol(dfFBSmacNutPct)]
dfFBSmacNutPct <- dfFBSmacNutPct %>% gather_("item", "value", gathercols)
dfFBSmacNutPct$item[grep("Protein", dfFBSmacNutPct$item)] <- "Protein supply (%)"
dfFBSmacNutPct$item[grep("Fat", dfFBSmacNutPct$item)] <- "Fat supply (%)"
dfFBSmacNutPct$item[grep("Carb", dfFBSmacNutPct$item)] <- "Carb supply (%)"
dfFBSmacNutPct <- dfFBSmacNutPct %>% merge(dfFBStot) %>%
  mutate(`Diet share (%)` = round(100 * value / total, 2)) %>%
  select(-c("value", "total")) %>%
  spread(item, `Diet share (%)`) %>%
  merge(dfPop)
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
#========================================================================
# Model year
thisYr <- 2015
# Model 1
# Hunger ~ Commodity
dfMod <- merge(dfHung, dfFBScommod) %>% subset(year %in% thisYr) %>%
  mutate(`DALYs/capita` = 1000 * val / `Population (1000 persons)`) %>%
  #mutate(`country-year` = paste0(area, year)) %>%
  select(-c("area", "year", "val", "Population (1000 persons)"))
dfMod[, -1] <- as.data.frame(apply(dfMod[, -1], 2, log))
#dfMod[, -1] <- as.data.frame(apply(dfMod[, -1], 2, log))
isInfNan <- function(x){
  nInfNan <- sum(is.nan(x) > 0) + sum(is.infinite(x) > 0)
  return(nInfNan)
}
infNanLook <- apply(dfMod, 2, isInfNan)
infNanLook
rmRows <- which(is.infinite(dfMod$Pulses))
dfMod <- dfMod[-rmRows, ]
dfModHid <- dfMod %>% subset(item == "Hid. Hunger") %>% select (-"item")
dfModChr <- dfMod %>% subset(item == "Chr. Hunger") %>% select (-"item")
mod <- lm(`DALYs/capita` ~., dfModHid)
# library(miceadds)
# mod <- lm.cluster(dfMod, `NutDef (DALYs)` ~.-1,
#                   cluster = "year")
# mod <- mod$lm_res
#---
mod <- lm(`DALYs/capita` ~., dfModChr)
summary(mod)
car::vif(mod)
plot(mod$fitted.values, mod$residuals)
mod <- lm(`DALYs/capita` ~., dfModHid)
summary(mod)
car::vif(mod)
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
dfMod <- merge(dfHung, dfFBSmacNutPct) %>% subset(year %in% thisYr) %>%
  mutate(`DALYs/capita` = 1000 * val / `Population (1000 persons)`) %>%
  #mutate(`country-year` = paste0(area, year)) %>%
  select(-c("area", "year", "val", "Population (1000 persons)",
            #"Food supply (kcal/capita/day)",
            "Fat supply (kcal/capita/day)"))
# dfMod <- merge(dfHidHung, dfFBSmacNut) %>% subset(year == thisYr) %>%
#   select(-c("area", "year", #"GDP/capita (USD 2015 prices)",
#             #"Fat supply (kcal/capita/day)",
#             "Protein supply (kcal/capita/day)",
#             "Food supply (kcal/capita/day)"
#             ))
  #select(-c("area", "year"))
#dfMod$`NutDef (DALYs) per 1000 capita` <- log(dfMod$`NutDef (DALYs) per 1000 capita`)
dfMod[, -1] <- as.data.frame(apply(dfMod[, -1], 2, log))
dfMod$`DALYs/capita` <- log(dfMod$`DALYs/capita`)
#dfMod <- as.data.frame(apply(dfMod, 2, scale))
infNanLook <- apply(dfMod, 2, isInfNan)
infNanLook
rmRows <- which(is.infinite(dfMod$Pulses))
dfMod <- dfMod[-rmRows, ]
dfModHid <- dfMod %>% subset(item == "Hid. Hunger") %>% select (-"item")
dfModChr <- dfMod %>% subset(item == "Chr. Hunger") %>% select (-"item")
mod <- lm(`DALYs/capita` ~., dfModHid)
summary(mod)
car::vif(mod)
plot(mod$fitted.values, mod$residuals)
#---
mod <- lm(`DALYs/capita` ~., dfModChr)
summary(mod)
car::vif(mod)
plot(mod$fitted.values, mod$residuals)
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

#========================================================================
#========================================================================
#========================================================================
#========================================================================
#========================================================================
dfXY <- dfFBSmacNut %>%
  merge(dfFBScommod) %>%
  merge(dfHidHung) %>%
  subset(year == thisYr & item == "Hid. Hunger") %>%
  select(-c("area", "year",
            #"GDP/capita (USD 2015 prices)",
            "Food supply (kcal/capita/day)"))
colsY <- colnames(dfXY)[c(2:3)]
colsX <- colnames(dfXY)[5:11]
matXY <- dfXY %>%
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
# colsX <- c("Animal Products", "Cereals", "Oilcrops", "Pulses", "Starchy Roots", "F&V", "Residual")
# colsY <- c("Fat supply (kcal/capita/day)", "Protein supply (kcal/capita/day)", "Carb supply (kcal/capita/day)")
#colsY <- c("Protein supply (kcal/capita/day)", "Carb supply (kcal/capita/day)")
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
Ltilde <- L[, 1:m]
GtildeSqrtInv <- diag(1 / sqrt(eigVals[1:m]))
GtildeSqrtInv %*% t(Ptilde) %*% Ltilde #check
RZ <- GtildeSqrtInv %*% t(Ptilde) %*% Kxy
outQR <- qr(RZ)
R <- qr.Q(outQR)
Z <- qr.R(outQR)
Zinv <- solve(Z)
#LtildeR <- Kxy %*% Zinv
LtildeR <- Ltilde %*% R
colnames(LtildeR) <- colnames(Kxy)
dfBar <- as.data.frame(LtildeR) %>% mutate(var = varNames) %>%
  gather(PC, val, gathercols)
gg <- ggplot(dfBar, aes(x = var, y = val))
gg <- gg + geom_bar(stat = "identity")
gg <- gg + geom_hline(yintercept = 0, color = "red")
gg <- gg + coord_flip()
gg <- gg + facet_wrap(~PC, nrow = 1)
gg
#----------------------------------------------------------------------
errMat <- Ltilde %*% RZ - Kxy
Ltilde %*% R - (Kxy + errMat) %*% Zinv
GtildeSqrtInv %*% t(Ptilde) %*% errMat
GtildeSqrtInv %*% t(Ptilde) %*% (Ltilde %*% RZ - Kxy)
GtildeSqrtInv %*% t(Ptilde) %*% Ltilde %*% RZ - RZ
#======================================================================
# Model 3
PCr <- X %*% Ptilde %*% R
dfMod <- as.data.frame(cbind(matXY[, "DALYs/capita"], PCr))
colnames(dfMod) <- c("NutDef (DALYs)", "PC1", "PC2")
mod <- lm(`NutDef (DALYs)` ~.-1, dfMod)
summary(mod)
car::vif(mod)
plot(mod$fitted.values, mod$residuals)
#
Lrot <- varimax(L)[[1]]
Lrot <- matrix(as.numeric(Lrot),
                   attributes(Lrot)$dim,
                   dimnames = attributes(Lrot)$dimnames)
R <- varimax(L)[[2]]
R <- matrix(as.numeric(R),
                attributes(R)$dim,
                dimnames = attributes(R)$dimnames)

theseCols <- c(1:2, 5)
PC <- X[, theseCols] %*% P[theseCols, theseCols] # %*% R[theseCols, theseCols]
dfMod <- as.data.frame(cbind(matXY[, "NutDef (DALYs)"], PC))
colnames(dfMod) <- c("NutDef (DALYs)", paste0("PC", 1:ncol(PC)))
mod <- lm(`NutDef (DALYs)` ~.-1, dfMod)
summary(mod)
car::vif(mod)
plot(mod$fitted.values, mod$residuals)
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
