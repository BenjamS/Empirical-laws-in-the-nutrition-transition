library(tidyverse)
library(patchwork)
# Create data files used in "Empirical-laws-old-and-new-in-the-nutrition-transition.Rmd"
#===========================================================================
# Define function to group countries into regions
FAOdat_createRegionGroups <- function(df_raw, folder)
{
  #=================================
  filename_vec <- list.files(folder)
  filepath_vec <- paste0(folder, "/", filename_vec)
  list_df <- purrr::map(filepath_vec, read.csv, stringsAsFactors = F)
  region_vec <- gsub("Country list - |\\.csv", "", filename_vec)
  names(list_df) <- region_vec
  #---
  # Extract countries for each region
  list_vec <- list()
  for(i in 1:length(list_df)){
    this_df <- list_df[[i]]
    colnames(this_df) <- gsub("Area", "Country", colnames(this_df))
    list_vec[[i]] <- unique(this_df$Country)
  }
  names(list_vec) <- region_vec
  #---
  # Africa regions
  countries_NAfrica <- list_vec[["Northern Africa"]]
  countries_MAfrica <- list_vec[["Middle Africa"]]
  countries_WAfrica <- list_vec[["Western Africa"]]
  # ind_correct <- which(countries_WAfrica == "C么te d'Ivoire")
  # countries_WAfrica[ind_correct] <- "C魌e d'Ivoire"
  countries_EAfrica <- list_vec[["Eastern Africa"]]
  countries_SAfrica <- list_vec[["Southern Africa"]]
  countries_SSA <- c(countries_MAfrica, countries_WAfrica,
                     countries_EAfrica, countries_SAfrica)
  non_SSA <- c("Niger", "Mali", "Mauritania", "Chad", "Eritrea")
  countries_SSA <- countries_SSA[which(!(countries_SSA %in% non_SSA))]
  countries_NAfrica <- c(countries_NAfrica, non_SSA)
  #--Americas
  countries_SAmer <- list_vec[["South America"]]
  countries_CAmer <- list_vec[["Central America"]]
  countries_Carib <- list_vec[["Caribbean"]]
  countries_NAmer <- list_vec[["Northern America"]]
  countries_LAC <- c(countries_SAmer, countries_CAmer, countries_Carib)
  #--Asia
  countries_EAsia <- list_vec[["Eastern Asia"]]
  #countries_EAsia <- countries_EAsia[!(countries_EAsia %in% c("China, Hong Kong SAR", "China, Macao SAR"))]
  countries_SEAsia <- list_vec[["South-Eastern Asia"]]
  countries_SAsia <- list_vec[["Southern Asia"]]
  countries_WAsia <- list_vec[["Western Asia"]]
  countries_CAsia <- list_vec[["Central Asia"]]
  countries_Asia <- c(countries_SEAsia, countries_SAsia,
                      countries_WAsia, countries_CAsia,
                      countries_EAsia)
  #--Europe
  countries_NEurope <- list_vec[["Northern Europe"]]
  countries_WEurope <- list_vec[["Western Europe"]]
  countries_EEurope <- list_vec[["Eastern Europe"]]
  countries_SEurope <- list_vec[["Southern Europe"]]
  countries_Europe <- c(countries_NEurope, countries_WEurope,
                        countries_EEurope, countries_SEurope)
  #--Oceania
  countries_Oceania <- list_vec[["Oceania"]]
  countries_AusNZea <- c("Australia", "New Zealand")
  countries_PacifIs <- setdiff(countries_Oceania, countries_AusNZea)
  #--Aggregate regions
  regions_FAO <- c("World")
  #-------------------------------------------------------------------------
  # Create more consolidated groups
  countries_AsiaExclWAsia <- setdiff(countries_Asia, countries_WAsia)
  countries_EurNAmerAusNZ <- c(countries_Europe, countries_NAmer, countries_AusNZea)
  countries_NAfricaWAsia <- c(countries_NAfrica, countries_WAsia)
  #-------------------------------------------------------------------------
  #--Create region groupings
  u <- df_raw$Area
  df_raw$Region <- NA
  df_raw$Region[which(u %in% countries_EurNAmerAusNZ)] <- "Europe / N. Amer.\nAustralia / NZ"
  df_raw$Region[which(u %in% countries_LAC)] <- "Latin America\n& Caribbean"
  df_raw$Region[which(u %in% countries_AsiaExclWAsia)] <- "Asia\n(excluding W. Asia)"
  df_raw$Region[which(u %in% countries_NAfricaWAsia)] <- "N. Africa / W. Asia"
  df_raw$Region[which(u %in% countries_SSA)] <- "Sub-Saharan Africa"
  df_raw$Region[which(u %in% regions_FAO)] <- "Aggregated"
  rm(u)
  #------------------------------------------------------------------------
  #--See what countries escaped designation
  #unique(df_raw$Area[which(is.na(df_raw$Region))])
  #--Assign these to their proper regions
  u <- df_raw$Area
  #unique(u[which(is.na(df_raw$Region))])
  df_raw$Region[which(u %in% c("?land Islands", "Isle of Man", "Greenland"))] <- "Europe / N. Amer.\nAustralia / NZ" #Northern Europe
  df_raw$Region[which(u %in% c("Anguilla", "Bermuda", "Cayman Islands", "Cura?ao", "Cuba"))] <- "Latin America\n& Caribbean" #Caribbean
  df_raw$Region[which(u %in% c("C?te d'Ivoire", "C魌e d'Ivoire", "C么te d'Ivoire"))] <- "Sub-Saharan Africa" #Western Africa
  df_raw$Region[which(u %in% c("Palau", "Polynesia", "French Polynesia",
                               "Solomon Islands", "Samoa", "American Samoa", "Micronesia",
                               "Fiji", "Vanuatu", "New Caledonia", "Kiribati"))] <- "Asia\n(excluding W. Asia)"#"Pacific Islands"
  df_raw$Region[which(u %in% c("Maldives", "R?union"))] <- "Asia\n(excluding W. Asia)" #Southern Asia
  df_raw$Region[which(u %in% c("French Guiana"))] <- "Latin America\n& Caribbean"#"South America"
  df_raw$Region[which(u == "China")] <- "Asia\n(excluding W. Asia)"
  rm(u)
  #------------------------------------------------------------------------
  df_raw <- df_raw[which(!is.na(df_raw$Region)), ]
  
  return(df_raw)
}
#===========================================================================
#===========================================================================
#===========================================================================
# BENNETT'S LAW
# Combine FAO FBS old and new datasets
this_folder <- "C:/Users/bensc/OneDrive/Documents/Data/FAO Data/"
this_file <- "FoodBalanceSheetsHistoric_E_All_Data.csv"
this_filepath <- paste0(this_folder, this_file)
df_raw_1 <- read.csv(this_filepath, stringsAsFactors = F)
df_raw_1 <- subset(df_raw_1, Item.Code != 2928)
this_file <- "FoodBalanceSheets_E_All_Data.csv"
this_filepath <- paste0(this_folder, this_file)
df_raw_2 <- read.csv(this_filepath, stringsAsFactors = F)
df_raw_2 <- subset(df_raw_2, Item.Code != 2928)
rm_cols <- c("Area.Code", "Item.Code", "Element.Code")
rm_cols <- which(colnames(df_raw_1) %in% rm_cols)
df_raw_1 <- df_raw_1[, -rm_cols]
df_raw_1$Area[which(df_raw_1$Area == "Swaziland")] <- "Eswatini"
df_raw_2 <- df_raw_2[, -rm_cols]
df_raw <- merge(df_raw_1, df_raw_2, by = c("Area", "Item", "Element", "Unit"))
df_raw$Item <- as.character(df_raw$Item)
df_raw$Element <- as.character(df_raw$Element)
df_raw$Area <- as.character(df_raw$Area)
u <- colnames(df_raw)
df_raw <- df_raw[, -grep("F", u)]
v <- colnames(df_raw)[5:ncol(df_raw)]
colnames(df_raw)[5:ncol(df_raw)] <- gsub("Y", "", v)
df_raw <- gather(df_raw,Year,Value,`1961`:`2017`)
#------------------------------------------------------------------------
# unique(df_raw_2$Area[grep("Congo", df_raw_2$Area)])
# unique(df_raw_1$Area[grep("Congo", df_raw_1$Area)])
# unique(df_raw$Area[grep("Eswatini", df_raw$Area)])
rm(df_raw_1, df_raw_2); gc()
#------------------------------------------------------------------------
element_vec <- c("Protein supply quantity (g/capita/day)",
                 "Fat supply quantity (g/capita/day)",
                 "Food supply (kcal/capita/day)",
                 "Total Population - Both sexes")
item_vec <- c("Grand Total", "Cereals - Excluding Beer",
              "Animal fats", "Starchy Roots", "Vegetables",
              "Oilcrops", "Fruits - Excluding Wine", "Pulses",
              "Sugar & Sweeteners", "Meat", "Animal Products",
              "Population")
#unique(df_raw$Item[grep("sweet", df_raw$Item, ignore.case = T)])
#unique(df_raw$Element)
df_fbal <- subset(df_raw, Element %in% element_vec &
                    Item %in% item_vec)
# unique(df_raw$Area[grep("Taiwan", df_raw$Area)])
# unique(df_fbal$Area[grep("Eswatini", df_fbal$Area)])
# Get pop from FBS
#unique(df_fbal$Element)
df_pop <- subset(df_fbal[, c("Area", "Year", "Element", "Unit", "Value")], Element == "Total Population - Both sexes")
df_pop$Element <- NULL
df_pop$Value <- df_pop$Value / 1000
colnames(df_pop)[ncol(df_pop)] <- "Population (millions)"
df_pop$Unit <- NULL
df_fbal <- subset(df_fbal, Element != "Total Population - Both sexes")
df_fbal <- merge(df_fbal, df_pop, by = c("Area", "Year"))
df_fbal$`Population (millions), logged` <- log(df_fbal$`Population (millions)`)
rm(df_raw, df_pop); gc()
#---------------------------------------------------------------------------
# Merge with FAO GDP/capita data
this_folder <- "C:/Users/bensc/OneDrive/Documents/Data/FAO Data/"
this_file <- "Macro-Statistics_Key_Indicators_E_All_Data.csv"
this_filepath <- paste0(this_folder, this_file)
df_gdp <- read.csv(this_filepath, stringsAsFactors = F)
rm_cols <- c("Area.Code", "Item.Code", "Element.Code")
rm_cols <- which(colnames(df_gdp) %in% rm_cols)
df_gdp <- df_gdp[, -rm_cols]
#unique(df_gdp$Item)
df_gdp$Item <- as.character(df_gdp$Item)
df_gdp$Area <- as.character(df_gdp$Area)
u <- colnames(df_gdp)
df_gdp <- df_gdp[, -grep("F", u)]
u <- colnames(df_gdp)
df_gdp <- df_gdp[, -grep("N", u)]
v <- colnames(df_gdp)[5:ncol(df_gdp)]
colnames(df_gdp)[5:ncol(df_gdp)] <- gsub("Y", "", v)
#---------------------------------------------------------------------------
# Check available years
#colnames(df_gdp)
#---------------------------------------------------------------------------
available_yrs <- as.character(c(1970:2018))
df_gdp <- gather_(df_gdp, "Year", "Value", gather_cols = available_yrs)
#df_gdp$Unit <- NULL
#---------------------------------------------------------------------------
#unique(df_gdp$Item)
#unique(df_gdp$Element)
item_vec <- c("Gross Domestic Product per capita", "Gross Domestic Product")
this_element <- "Value US$" #"Value US$, 2015 prices"
df_gdp <- subset(df_gdp, Item %in% item_vec &
                   Element == this_element)
df_gdp <- df_gdp[, c("Area", "Year", "Item", "Unit", "Value")]
df_gdp$Unit <- NULL
df_gdp <- df_gdp %>% spread(Item, Value)
colnames(df_gdp)[3:4] <- c("GDP (million USD)", "GDP/capita (USD) 2")
df_gdp$`GDP (million USD), logged` <- log(df_gdp$`GDP (million USD)`)
df_gdp$`GDP/capita (USD) 2, logged` <- log(df_gdp$`GDP/capita (USD) 2`)
df_gdp$Year <- as.integer(df_gdp$Year)
# Is GDP/capita GDP over pop? (Looks like no.)
df_gdp$`Population (millions) test` <- df_gdp$`GDP (million USD)` / df_gdp$`GDP/capita (USD) 2`
#---------------------------------------------------------------------------
# Check if GDP is lognormal
# (It is, but GDP/capita is not.)
# Is population lognormal?
# (Yes if you exclude small islands and other land constrained nations.)
#df_x <- subset(df_gdp, Year == 2017)
# landConstrained_vec <- c("Andorra", "Anguilla", "Antigua and Barbuda",
#                          "Aruba", "Bahamas", "Bahrain", "Barbados",
#                          "Bermuda", "British Virgin Islands", "Cabo Verde",
#                          "Cayman Islands", "China, Macao SAR", "Comoros",
#                          "Dominica", "Fiji", "French Polynesia", "Grenada",
#                          "Kiribati", "Lesotho", "Maldives", "Malta",
#                          "Mauritius", "Micronesia", "Montserrat",
#                          "Netherlands Antilles (former)", "New Caledonia",
#                          "Palau", "Polynesia", "Saint Kitts and Nevis",
#                          "Saint Lucia", "Saint Vincent and the Grenadines",
#                          "American Samoa",
#                          "Samoa", "San Marino", "Sao Tome and Principe",
#                          "Seychelles", "Sint Maarten (Dutch Part)",
#                          "Solomon Islands", "Timor-Leste",
#                          "Trinidad and Tobago", "Vanuatu", "Luxembourg",
#                          "Brunei Darussalam", "Cyprus")
# df_x <- subset(df_gdp, !(Area %in% landConstrained_vec))
# this_subfolder <- "region_country_files"
# regions_folder <- paste0(this_folder, this_subfolder)
# df_x <- FAOdat_createRegionGroups(df_x,
#                                      folder = regions_folder)
# normTest_GDP <- c()
# normTest_Pop <- c()
# normTest_GDPpCap <- c()
# for(i in 1:length(unique(df_x$Year))){
#   this_year <- min(df_x$Year) + i - 1
#   this_df <- subset(df_x, Year == this_year)
#   normTest_GDP[i] <- shapiro.test(this_df$`GDP (million USD), logged`)[[2]]
#   normTest_Pop[i] <- shapiro.test(this_df$`Population (millions), logged`)[[2]]
#   normTest_GDPpCap[i] <- shapiro.test(this_df$`GDP/capita (USD), logged`)[[2]]
# }
# 
# df_look <- subset(df_x, Year == 1980)
# hist(df_look$`Population (millions), logged`)
# hist(df_look$`GDP/capita (USD), logged`)
# shapiro.test(df_look$`Population (millions), logged`)
# ggpubr::ggqqplot(df_look$`Population (millions), logged`)
# df_look$Area[which(df_look$`Population (millions), logged` < 0)]
# 
# plot(df_look$`Population (millions), logged`, df_look$`GDP (million USD), logged`)
# df_mod <- df_look[, c("Population (millions), logged", "GDP (million USD), logged")]
# mod <- lm(`GDP (million USD), logged`~., df_mod)
# summary(mod)
# 
# df_x <- df_x %>% group_by(Year) %>%
#   summarise(m_gdp = mean(`GDP (million USD), logged`, na.rm = T),
#             s_gdp = sd(`GDP (million USD), logged`, na.rm = T),
#             m_pop = mean(`Population (millions), logged`, na.rm = T),
#             s_pop = sd(`Population (millions), logged`, na.rm = T)) %>%
#   as.data.frame()
# 
# plot(df_x$m_pop, df_x$s_pop)
# df_x$cv <- df_x$s_pop / df_x$m_pop
# hist(df_x$cv)
#--------------------------------------------------------------------------
df_fbal <- merge(df_fbal, df_gdp, by = c("Area", "Year"))
# unique(df_gdp$Area[grep("China", df_gdp$Area)])
# unique(df_fbal$Area[grep("Taiwan", df_fbal$Area)])
# Create simple GDP/capita (as GDP over pop)
df_fbal$`GDP/capita (USD)` <- df_fbal$`GDP (million USD)` / df_fbal$`Population (millions)`
# Is it the same as the GDP/cap provided by FAO? Compare to `GDP/capita (USD) 2`
# Looks like not.
df_fbal$`GDP/capita (USD), logged` <- log(df_fbal$`GDP/capita (USD)`)
rm(df_gdp)
#--------------------------------------------------------------------------
# Get region groupings
this_subfolder <- "region_country_files"
this_folder <- paste0(this_folder, this_subfolder)
df_fbal <- FAOdat_createRegionGroups(df_fbal,
                                     folder = this_folder)
#--------------------------------------------------------------------------
# Correct/modify country names (partly to harmonize with the WB ICP data)
df_fbal$Area[grep("Tanzania", df_fbal$Area)] <- "Tanzania"
df_fbal$Area[grep("C么te d'Ivoire", df_fbal$Area)] <- "C魌e d'Ivoire"
df_fbal$Area[grep("Viet Nam", df_fbal$Area)] <- "Vietnam"
df_fbal$Area[grep("Iran", df_fbal$Area)] <- "Iran"
df_fbal$Area[grep("Lao", df_fbal$Area)] <- "Lao PDR"
df_fbal$Area[grep("Venezuela", df_fbal$Area)] <- "Venezuela"
df_fbal$Area[grep("Bolivia", df_fbal$Area)] <- "Bolivia"
df_fbal$Area[grep("Democratic Republic of the Congo", df_fbal$Area)] <- "Congo, Dem. Rep."
df_fbal$Area[grep("Micronesia", df_fbal$Area)] <- "Micronesia"
df_fbal$Area[which(df_fbal$Area == "Congo")] <- "Congo, Rep."
df_fbal$Area[which(df_fbal$Area == "Republic of Korea")] <- "Korea, Rep."
df_fbal$Area[which(df_fbal$Area == "United States of America")] <- "United States"
df_fbal$Area[grep("Sint Maarten", df_fbal$Area)] <- "Sint Maarten"
df_fbal$Area[grep("Moldova", df_fbal$Area)] <- "Moldova"
df_fbal$Area[which(df_fbal$Area == "Czechia")] <- "Czech Republic"
df_fbal$Area <- gsub("Saint", "St.", df_fbal$Area)
df_fbal$Unit <- NULL
#unique(df_fbal$Area[grep("Libya", df_fbal$Area)])
#--------------------------------------------------------------------------
# Merge with WDI pop/employment data
# this_folder <- "C:/Users/bensc/OneDrive/Documents/Data/WDI Data/"
# this_file <- "WDIData.csv"
# this_filepath <- paste0(this_folder, this_file)
# df_wdi <- read.csv(this_filepath, stringsAsFactors = F)
# ind_rm <- which(colnames(df_wdi) %in% c("Country.Code", "Indicator.Code"))
# df_wdi <- df_wdi[, -ind_rm]
# colnames(df_wdi)[1:2] <- c("Country", "Indicator")
# colnames(df_wdi) <- gsub("X", "", colnames(df_wdi))
# df_wdi[, ncol(df_wdi)] <- NULL
# df_wdi <- df_wdi %>% gather(Year, Value, `1960`:`2019`)
# #unique(df_wdi$Indicator[grep("value added", df_wdi$Indicator, ignore.case = T)])
# # df_x <- subset(df_wdi, Year == 2017)
# # ind <- which(df_x$Indicator == "Population ages 15-64, total")
# # length(which(!is.na(df_x$Value[ind])))
# wdi_keep <- c("Age dependency ratio (% of working-age population)",
#               "Population ages 15-64, total",
#               "Age dependency ratio, old (% of working-age population)",
#               "Age dependency ratio, young (% of working-age population)",
#               "Unemployment, total (% of total labor force) (modeled ILO estimate)",
#               "Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)",
#               "Employment to population ratio, 15+, total (%) (modeled ILO estimate)",
#               "Employment in agriculture (% of total employment) (modeled ILO estimate)",
#               "Employment in industry (% of total employment) (modeled ILO estimate)",
#               "Employment in services (% of total employment) (modeled ILO estimate)",
#               "Gross savings (% of GDP)",
#               "Gross savings (current US$)",
#               "Gross domestic savings (% of GDP)",
#               "Gross domestic savings (current US$)")
# df_wdi <- subset(df_wdi, Indicator %in% wdi_keep)
# #df_wdi <- 
# df_wdi <- df_wdi %>% spread(Indicator, Value)
# # Harmonize country names for merge with FAO FBS data
# #unique(df_wdi$Country[grep("samoa", df_wdi$Country, ignore.case = T)])
# df_wdi$Country[grep("Gambia", df_wdi$Country)] <- "Gambia"
# df_wdi$Country[grep("Iran", df_wdi$Country)] <- "Iran"
# df_wdi$Country[grep("S茫o Tom茅 and Principe", df_wdi$Country)] <- "Sao Tome and Principe"
# df_wdi$Country[grep("C么te d'Ivoire", df_wdi$Country)] <- "C魌e d'Ivoire"
# df_wdi$Country[grep("Venezuela", df_wdi$Country)] <- "Venezuela"
# df_wdi$Country[grep("Egypt", df_wdi$Country)] <- "Egypt"
# df_wdi$Country[grep("Micronesia", df_wdi$Country)] <- "Micronesia"
# df_wdi$Country[grep("Bahamas", df_wdi$Country)] <- "Bahamas"
# #df_wdi$Country[grep("Samoa", df_wdi$Country)] <- "Samoa"
# df_wdi$Country[grep("Virgin Islands, British", df_wdi$Country)] <- "British Virgin Islands"
# df_wdi$Country[grep("Hong Kong SAR, China", df_wdi$Country)] <- "China, Hong Kong SAR"
# df_wdi$Country[grep("Macao SAR, China", df_wdi$Country)] <- "China, Macao SAR"
# colnames(df_wdi)[1] <- "Area"
# df_fbal <- merge(df_fbal, df_wdi, by = c("Area", "Year"))
# rm(df_wdi)
#---------------------------------------------------------------------------
# Write
this_folder <- "C:/Users/bensc/OneDrive/Documents/Empirical laws, old and new, in the nutrition transition/"
this_file <- "Bennetts Law FAO FBS and WDI data.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(df_fbal, this_filepath, row.names = F)
#--------------------------------------------------------------------------
# Check (classic Bennett analysis)
these_indic <- c("GDP (million USD), logged",
                 "GDP (million USD)",
                 "GDP/capita (USD), logged",
                 "GDP/capita (USD)",
                 "Population (millions), logged",
                 "Population (millions)")
               #   "Population ages 15-64, total",
               # "Age dependency ratio (% of working-age population)",
               # "Unemployment, total (% of total labor force) (modeled ILO estimate)",
               # "Employment in agriculture (% of total employment) (modeled ILO estimate)",
               # "Employment in industry (% of total employment) (modeled ILO estimate)",
               # "Employment in services (% of total employment) (modeled ILO estimate)")
these_cols <- c("Area", "Region", "Year", "Item", "Element", "Value", these_indic)
#setdiff(these_cols, colnames(df_fbal))
df_foodGroups <- subset(df_fbal[, these_cols], Element == "Food supply (kcal/capita/day)")# &
#                          Year == 2017)
df_foodGroups$Year <- as.integer(df_foodGroups$Year)
df_tot <- subset(df_foodGroups[, c("Area", "Year", "Item", "Value", these_indic)], Item == "Grand Total")
colnames(df_tot)[which(colnames(df_tot) == "Value")] <- "Total (kcal/capita/day)"
df_tot$Item <- NULL
df_foodGroups <- subset(df_foodGroups, Item != "Grand Total")
#---
# Consolidate cereals and starchy roots into one category
df_foodGroups$Item[grep("Starchy Roots", df_foodGroups$Item)] <- "Cereals & starchy roots"
df_foodGroups$Item[grep("Cereals - Excluding Beer", df_foodGroups$Item)] <- "Cereals & starchy roots"
# df_foodGroups$Item[grep("Fruits", df_foodGroups$Item)] <- "Fruits & vegetables"
# df_foodGroups$Item[grep("Vegetables", df_foodGroups$Item)] <- "Fruits & vegetables"
#df_foodGroups$Item[grep("Pulses", df_foodGroups$Item)] <- "Cereals & starchy roots"
df_foodGroups <- subset(df_foodGroups, Item == "Cereals & starchy roots")
df_foodGroups <- df_foodGroups %>% 
  group_by(Region, Area, Year, Item) %>%
  summarise(Value = sum(Value, na.rm = T)) %>% as.data.frame()
#---
df_foodGroups <- merge(df_foodGroups, df_tot, by = c("Area", "Year"))
colnames(df_foodGroups)[which(colnames(df_foodGroups) == "Value")] <- "Food supply (kcal/capita/day)"
df_foodGroups$`Food supply (% of total)` <- 100 * df_foodGroups$`Food supply (kcal/capita/day)` /
  df_foodGroups$`Total (kcal/capita/day)`
df_foodGroups$`Food supply (% of total), logged` <- 
  log(df_foodGroups$`Food supply (% of total)`)
df_foodGroups$`Food supply (kcal/capita/day), logged` <- 
  log(df_foodGroups$`Food supply (kcal/capita/day)`)

# df_foodGroups$`Dependent pop.` <- 1 / 100 * df_foodGroups$`Age dependency ratio (% of working-age population)` *
#   df_foodGroups$`Population ages 15-64, total` +
#   1 / 100 * df_foodGroups$`Unemployment, total (% of total labor force) (modeled ILO estimate)` * 
#   df_foodGroups$`Population ages 15-64, total`
# df_foodGroups$`Dependent pop., logged` <- log(df_foodGroups$`Dependent pop.`)
# df_foodGroups$`Population ages 15-64, logged` <- log(df_foodGroups$`Population ages 15-64, total`)
# df_foodGroups$`Unemployment (%), logged` <- log(df_foodGroups$`Unemployment, total (% of total labor force) (modeled ILO estimate)`)
# df_foodGroups$`Dependency (%), logged` <- log(df_foodGroups$`Age dependency ratio (% of working-age population)`)

df_foodGroups$`Total (kcal/capita/day), logged` <- log(df_foodGroups$`Total (kcal/capita/day)`)
df_foodGroups$`Total (kcal/day)` <- 10^6 * df_foodGroups$`Total (kcal/capita/day)` * df_foodGroups$`Population (millions)`
df_foodGroups$`Total (kcal/day), logged` <- log(df_foodGroups$`Total (kcal/day)`)
df_foodGroups$`Food supply (kcal/day)` <- 10^6 * df_foodGroups$`Food supply (kcal/capita/day)` * df_foodGroups$`Population (millions)`
df_foodGroups$`Food supply (kcal/day), logged` <- log(df_foodGroups$`Food supply (kcal/day)`)

df_foodGroups$`GDP/1000 kcal (USD)` <- 1000 * 10^6 / 365 * df_foodGroups$`GDP (million USD)` / df_foodGroups$`Total (kcal/day)`
df_foodGroups$`GDP/1000 kcal (USD), logged` <- log(df_foodGroups$`GDP/1000 kcal (USD)`)

colnames(df_foodGroups) <- gsub("Food supply", "Cereals & starchy roots", colnames(df_foodGroups))
colnames(df_foodGroups) <- gsub("% of total", "% of diet", colnames(df_foodGroups))


# hist(df_foodGroups$`Dependent pop., logged`)
# hist(df_foodGroups$`Population (millions), logged`)
# shapiro.test(df_foodGroups$`Dependent pop., logged`)
# library(mclust)
# df_gm <- subset(df_foodGroups[, c("Area", "Year",
#                            "GDP/capita (USD), logged",
#                            "Population (millions), logged",
#                            "GDP (million USD), logged")],
#                 Year == 2017)
# 
# # fit <- Mclust(df_gm$`GDP/capita (USD), logged`, 2, model = "V")
# # # summary(fit)
# # # plot(fit, what="density")
# # # rug(df_gm$`GDP/capita (USD), logged`)
# # mu_vec <- fit$parameters$mean
# # sd_vec <- sqrt(fit$parameters$variance$sigmasq)
# # df_gm$clust <- fit$classification
# 
# gm <- mixtools::normalmixEM(df_gm$`GDP/capita (USD), logged`, k = 3)
# mu_vec <- gm$mu
# sd_vec <- gm$sigma
# gm$lambda
# mat_prob <- gm$posterior
# clust_vec <- apply(mat_prob, 1, FUN = function(x) which(x == max(x)))
# df_gm$clust <- clust_vec
# gg <- ggplot(df_gm)
# gg <- gg + geom_histogram(aes(x = `GDP/capita (USD), logged`, y = after_stat(density)), bins = 15)
# gg <- gg + stat_density(aes(x = `GDP/capita (USD), logged`,  linetype = as.factor(clust)), position = "stack", geom = "line", show.legend = F, color = "red") +
#   stat_density(aes(x = `GDP/capita (USD), logged`,  linetype = as.factor(clust)), position = "identity", geom = "line")
# gg
# 
# gg <- ggplot(df_gm)
# gg <- gg + geom_histogram(aes(x = `GDP/capita (USD), logged`, y = after_stat(density)), bins = 15)
# gg <- gg + stat_function(fun = dnorm, args = list(mean = mu_vec[1], sd = sd_vec[1]))
# gg <- gg + stat_function(fun = dnorm, args = list(mean = mu_vec[2], sd = sd_vec[2]))
# gg <- gg + stat_function(fun = dnorm, args = list(mean = mu_vec[3], sd = sd_vec[3]))
# gg
# 
# df_gm$Area[which(df_gm$clust == 1)]
# df_gm$Area[which(df_gm$clust == 2)]
# df_gm$Area[which(df_gm$clust == 3)]
# shapiro.test(df_gm$`GDP/capita (USD), logged`[which(df_gm$clust == 1)])
# shapiro.test(df_gm$`GDP/capita (USD), logged`[which(df_gm$clust == 2)])
# shapiro.test(df_gm$`GDP/capita (USD), logged`[which(df_gm$clust == 3)])
# 
# df_gm$clust <- as.character(df_gm$clust)
# gg <- ggplot(df_gm, aes(x = `GDP/capita (USD), logged`,
#                         y = `GDP (million USD), logged`,
#                         group = clust, color = clust))
# gg <- gg + geom_point()
# gg
# 
# gg <- ggplot(df_foodGroups, aes(x = `GDP/capita (USD), logged`))
# gg <- gg + geom_histogram(aes(y = after_stat(density)), bins = 30)
# gg <- gg + stat_function(fun = dnorm, args = list(mean = gm$mu[1], sd = gm$sigma[1]))
# gg <- gg + stat_function(fun = dnorm, args = list(mean = gm$mu[2], sd = gm$sigma[2]))
# gg
#---
df_plot <- subset(df_foodGroups, Year == 2017 & Region != "Aggregated")
mod <- lm(`Cereals & starchy roots (% of diet), logged` ~ `GDP/capita (USD), logged`, df_plot)
#mod <- lm(`Food supply (% of total), logged` ~ `GDP/1000 kcal (USD), logged`, df_plot)
#summary(mod)
#car::vif(mod)
#cov(df_plot$`GDP (million USD), logged`, df_plot$`Population (millions), logged`)
m <- round(mod$coefficients[2], 4)
b <- round(mod$coefficients[1], 4)
df_out <- as.data.frame(broom::glance(mod))
adjR2 <- round(df_out$adj.r.squared, 2)
facet_labels <- paste0("Adj. R-squared = ", adjR2, ", Slope = ", m, ", Y intercept = ", b)
# adjR2_vec[i] <- adjR2
# names(facet_labels) <- these_elements

shape_vec <- c(21:24, 4)
n <- length(unique(df_plot$Region))
bag_of_colors <- randomcoloR::distinctColorPalette(k = 2 * n)
color_vec <- sample(bag_of_colors, n)

# Rsq slightly better by modeling gdp and pop as separate terms (as opposed to gdp/capita).
# Stands to reason that dependent pop better control var than pop, but there is almost no difference.

gg <- ggplot(df_plot, aes(x = `GDP/capita (USD), logged`,
                          #x = `GDP/1000 kcal (USD), logged`,
                          y = `Cereals & starchy roots (% of diet), logged`,
                          #y = `Cereals & starchy roots (kcal/capita/day), logged`,
                          group = Region, fill = Region,
                          shape = Region))
gg <- gg + geom_point(alpha = 0.6, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
gg <- gg + labs(subtitle = facet_labels)
#gg <- gg + facet_wrap(~Item, scales = "free_y")
# gg <- gg + guides(fill = guide_legend(nrow = 2, byrow = T, override.aes = list(linetype = 0)),
#                   color = guide_legend(override.aes = list(linetype = 0)))
gg


mod <- lm(`Total (kcal/capita/day), logged` ~ `GDP/capita (USD), logged`, df_plot)
#mod <- lm(`Total (kcal/capita/day), logged` ~ `GDP/1000 kcal (USD), logged`, df_plot)
#summary(mod)
a_2017 <- round(mod$coefficients[2], 4)
b_2017 <- round(mod$coefficients[1], 4)
df_out <- as.data.frame(broom::glance(mod))
adjR2 <- round(df_out$adj.r.squared, 2)
facet_labels <- paste0("Adj. R-squared = ", adjR2, ", Slope = ", a_2017, ", Y intercept = ", b_2017)
# adjR2_vec[i] <- adjR2
# names(facet_labels) <- these_elements

shape_vec <- c(21:24, 4)
n <- length(unique(df_plot$Region))
bag_of_colors <- randomcoloR::distinctColorPalette(k = 2 * n)
color_vec <- sample(bag_of_colors, n)

gg <- ggplot(df_plot, aes(#x = `GDP/1000 kcal (USD), logged`,
                          x = `GDP/capita (USD), logged`,
                          y = `Total (kcal/capita/day), logged`,
                          group = Region, fill = Region,
                          shape = Region))
gg <- gg + geom_point(alpha = 0.6, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
gg <- gg + labs(subtitle = facet_labels)
#gg <- gg + facet_wrap(~Item, scales = "free_y")
# gg <- gg + guides(fill = guide_legend(nrow = 2, byrow = T, override.aes = list(linetype = 0)),
#                   color = guide_legend(override.aes = list(linetype = 0)))
gg
#---
# Check demand
# getDemEst <- function(slope, yint, N, gdp_vec, var_vec){
#   a <- slope
#   b <- yint
#   #---
#   shapTest_lgdpPa <- round(shapiro.test(log(gdp_vec^a * var_vec^(1 - a)))[[2]], 3)
#   shapTest_lgdp <- round(shapiro.test(log(gdp_vec))[[2]], 3)
#   shapTest_lvar <- round(shapiro.test(log(var_vec))[[2]], 3)
#   cov_lvarGdp <- cov(log(gdp_vec), log(var_vec))#, use = "complete.obs")
#   #---
#   mu_gdpPa_samp <- mean(gdp_vec^a * var_vec^(1 - a))
#   m_gdp <- mean(log(gdp_vec))
#   s_gdp <- sd(log(gdp_vec))
#   m_var <- mean(log(var_vec))
#   s_var <- sd(log(var_vec))
#   #---
#   dem_samp <- N * exp(b) * mu_gdpPa_samp
#   mu_gdpa <- exp(a * m_gdp + a^2 * s_gdp^2 / 2)
#   mu_varam1 <- exp((1 - a) * m_var + (1 - a)^2 * s_var^2 / 2)
#   mu_gdpPa_est <- mu_gdpa * mu_varam1 * exp(a * (1 - a) * cov_lvarGdp)
#   mu_gdpPa_est_corrct <- mu_gdpa * mu_varam1 * exp(a * (1 - a) * cov_lvarGdp / 2)
#   dem_est <- N * exp(b) * mu_gdpPa_est
#   dem_est_corrct <- N * exp(b) * mu_gdpPa_est_corrct
#   pctDiff_muSamp <- (mu_gdpPa_est - mu_gdpPa_samp) / mu_gdpPa_samp * 100
#   #---
#   tot_var <- sum(var_vec, na.rm = T)
#   tot_gdp <- sum(gdp_vec, na.rm = T)
#   #---
#   dem_est2 <- exp(b) * tot_gdp^a * tot_var^(1 - a)
#   #---
#   outvec <- c(dem_samp, dem_est, dem_est_corrct, dem_est2,
#               m_gdp, s_gdp,
#               m_var, s_var, mu_gdpPa_samp,
#               shapTest_lvar, shapTest_lgdp, shapTest_lgdpPa,
#               a, b, N, tot_var, tot_gdp, cov_lvarGdp, pctDiff_muSamp)
#   names(outvec) <- c("dem_samp", "dem_est", "dem_est_corrct", "dem_est2",
#                      "m_gdp", "s_gdp", "m_var", "s_var", "mu_gdpPa_samp",
#                      "shapTest_lvar", "shapTest_lgdp", "shapTest_lgdpPa",
#                      "a", "b", "N", "tot_var", "tot_gdp", "cov_lvarGdp",
#                      "pctDiff_muSamp")
#   return(outvec)
# }

getDemEst <- function(df_dem){
#  df_dem <- df_foodGroups
n_yrs <- length(unique(df_dem$Year))
yr_min <- min(df_dem$Year)
list_DemTot <- list()
list_DemStarch <- list()
  for(i in 1:n_yrs){
    this_year <- yr_min + i - 1
    df_mod <- subset(df_dem, Year == this_year & Region != "Aggregated")
    df_world <- subset(df_dem, Year == this_year & Area == "World")
    worldGDP <- 10^6 * df_world$`GDP (million USD)`
    worldPop <- 10^6 * df_world$`Population (millions)`
    worldGDPpCap <- df_world$`GDP/capita (USD)`
    #---Total kcal demand
    mod <- lm(`Total (kcal/capita/day), logged` ~ `GDP/capita (USD), logged`, df_mod)
    #summary(mod)
    a <- round(mod$coefficients[2], 4)
    b <- round(mod$coefficients[1], 4)
    df_out <- as.data.frame(broom::glance(mod))
    adjR2 <- round(df_out$adj.r.squared, 2)
    #facet_labels <- paste0("Adj. R-squared = ", adjR2, ", Slope = ", a_2017, ", Y intercept = ", b_2017)
    worldDemTot <- df_world$`Total (kcal/day)`
    worldDemTot_est <- exp(b) * worldGDP^a * worldPop^(1 - a)
    pctDiff_DemTot <- 100 * (worldDemTot_est - worldDemTot) / worldDemTot
    outDemTot <- c(this_year, a, b, adjR2, worldDemTot_est, worldDemTot, pctDiff_DemTot)
    list_DemTot[[i]] <- outDemTot
    #---Cereal and starchy root demand
    mod <- lm(`Cereals & starchy roots (% of diet)` ~ `GDP/capita (USD), logged`, df_mod)
    #summary(mod)
    a <- round(mod$coefficients[2], 4)
    b <- round(mod$coefficients[1], 4)
    df_out <- as.data.frame(broom::glance(mod))
    adjR2 <- round(df_out$adj.r.squared, 2)
    #facet_labels <- paste0("Adj. R-squared = ", adjR2, ", Slope = ", a_2017, ", Y intercept = ", b_2017)
    worldDemStarch <- df_world$`Cereals & starchy roots (kcal/day)`
    worldDemStarchPctShare <- df_world$`Cereals & starchy roots (% of diet)`
    #worldDemStarchPctShare_est <- exp(b) * worldGDPpCap^a
    worldDemStarchPctShare_est <- b + a * log(worldGDPpCap)
    worldDemStarch_est <- worldDemTot_est * worldDemStarchPctShare_est / 100
    pctDiff_DemStarch <- 100 * (worldDemStarch_est - worldDemStarch) / worldDemStarch
    ppDiff_DemStarchPctShare <- worldDemStarchPctShare_est - worldDemStarchPctShare
    outDemStarch <- c(this_year, a, b, adjR2,
                      #worldDemStarch_est, worldDemStarch,
                      worldDemStarchPctShare_est, worldDemStarchPctShare,
                      ppDiff_DemStarchPctShare)
    list_DemStarch[[i]] <- outDemStarch
    
  }
  df_DemTot <- as.data.frame(do.call(rbind, list_DemTot))
  colnames(df_DemTot) <- c("Year", "Slope", "Y intercept", "Adj. R-squared",
                        "Estimated",
                        "Real",
                        "Pct. Difference")
  df_DemTot$Item <- "Total"
  df_DemStarch <- as.data.frame(do.call(rbind, list_DemStarch))
  colnames(df_DemStarch) <- c("Year", "Slope", "Y intercept", "Adj. R-squared",
                           "Estimated",
                           "Real",
                           "Pct. Difference")
  df_DemStarch$Item <- "Cereals & starchy roots"
  df_out <- as.data.frame(rbind(df_DemTot, df_DemStarch))
  
  return(df_out)
}


df_out <- getDemEst(df_dem = df_foodGroups)

df_plotTot <- subset(df_out, Item == "Total")
df_plotStarch <- subset(df_out, Item == "Cereals & starchy roots")

df_plotTot1 <- df_plotTot
df_plotTot1$Item <- NULL
df_plotTot1$`Pct. Difference` <- NULL
df_plotTot1 <- df_plotTot1 %>% gather(Type, `Demand (kcal/day)`, Estimated:Real)
df_plotTot2 <- df_plotTot[, c("Year", "Pct. Difference")]
gg <- ggplot(df_plotTot1, aes(x = Year, y = `Demand (kcal/day)`,
                          group = Type, color = Type))
gg <- gg + geom_line(lwd = 1)
ggTot <- gg

gg <- ggplot(df_plotTot2, aes(x = Year, y = `Pct. Difference`))
gg <- gg + geom_line(lwd = 1)
ggTotDiff <- gg

ggTot + ggTotDiff + plot_layout(ncol = 1, heights = c(1, 1 / 4))

# df_plotStarch1 <- df_plotStarch
# df_plotStarch1$Item <- NULL
# df_plotStarch1$`Pct. Difference` <- NULL
# df_plotStarch1 <- df_plotStarch1 %>% gather(Type, `Demand (kcal/day)`, Estimated:Real)
# df_plotStarch2 <- df_plotStarch[, c("Year", "Pct. Difference")]
# gg <- ggplot(df_plotStarch1, aes(x = Year, y = `Demand (kcal/day)`,
#                               group = Type, color = Type))
# gg <- gg + geom_line(lwd = 1)
# ggStarch <- gg
# 
# gg <- ggplot(df_plotStarch2, aes(x = Year, y = `Pct. Difference`))
# gg <- gg + geom_line(lwd = 1)
# ggStarchDiff <- gg
# 
# ggStarch + ggStarchDiff + plot_layout(ncol = 1, heights = c(1, 1 / 4))

df_plotSlopeYint <- subset(df_plotTot, Year >= 1980)
gg <- ggplot(df_plotSlopeYint, aes(x = Slope, y = `Y intercept`,
                          label = Year))
gg <- gg + geom_point()
gg <- gg + ggrepel::geom_text_repel()
gg

mod <- lm(`Y intercept`~Slope, df_plotSlopeYint)
summary(mod)
m_b <- mod$coefficients["Slope"]
b_b <- mod$coefficients["(Intercept)"]
# Estimate total kcal demand in 2050
a_fut <- 0.08
b_fut <- m_b * a_fut + b_b

annual_growth <- 0.04
n_yrs <- 2050 - 2020
pctChng_GDP <- (1 + annual_growth)^n_yrs
worldGDP2017 <- 10^6 * subset(df_dem, Area == "World" &
                                Year == 2017)$`GDP (million USD)`
worldGDP2050 <- worldGDP2017 * pctChng_GDP
worldPop2050 <- 9.7 * 10^9
worldPop2017 <- 10^6 * subset(df_dem, Area == "World" &
                                Year == 2017)$`Population (millions)`
DemTot2017 <- subset(df_dem, Area == "World" &
                       Year == 2017)$`Total (kcal/day)`
DemTot2050_est <- exp(b_fut) * worldGDP2050^a_fut * worldPop2050^(1 - a_fut)
(DemTot2050_est - DemTot2017) / DemTot2017

#--------------------------------------------------------------------------
# Same analysis this time with macronutrient groups (fat, protein, carbs)
these_cols <- c("Area", "Region", "Year", "Element", "Item", "Value", "GDP/capita (USD, 2015 prices), logged")
df_kcalGroups <- subset(df_fbal[, these_cols], Item == "Grand Total")
df_kcalGroups$Item <- NULL
# Convert g to kcal
df_kcalGroups$Value[grep("Protein", df_kcalGroups$Element)] <- 4 *
  df_kcalGroups$Value[grep("Protein", df_kcalGroups$Element)]
df_kcalGroups$Value[grep("Fat", df_kcalGroups$Element)] <- 9 *
  df_kcalGroups$Value[grep("Fat", df_kcalGroups$Element)]
df_kcalGroups$Element[grep("Protein", df_kcalGroups$Element)] <- "Protein supply (kcal/capita/day)"
df_kcalGroups$Element[grep("Fat", df_kcalGroups$Element)] <- "Fat supply (kcal/capita/day)"
# Calculate carb kcal
df_kcalGroups <- df_kcalGroups %>% spread(Element, Value)
df_kcalGroups$`Carb supply (kcal/capita/day)` <- df_kcalGroups$`Food supply (kcal/capita/day)` -
  df_kcalGroups$`Protein supply (kcal/capita/day)` - df_kcalGroups$`Fat supply (kcal/capita/day)`
# Create % of diet variable
df_tot <- df_kcalGroups[, c("Area", "Year", "Food supply (kcal/capita/day)")]
df_kcalGroups$`Food supply (kcal/capita/day)` <- NULL
gathercols <- colnames(df_kcalGroups)[5:ncol(df_kcalGroups)]
df_kcalGroups <- df_kcalGroups %>% gather_("Item", "Value", gathercols)
df_kcalGroups$Item[grep("Protein", df_kcalGroups$Item)] <- "Protein"
df_kcalGroups$Item[grep("Fat", df_kcalGroups$Item)] <- "Fat"
df_kcalGroups$Item[grep("Carb", df_kcalGroups$Item)] <- "Carbohydrate"
df_kcalGroups <- merge(df_kcalGroups, df_tot, by = c("Area", "Year"))
colnames(df_kcalGroups)[6:7] <- c("Supply (kcal/capita/day)", "Total (kcal/capita/day)")
df_kcalGroups$`Supply (% of total)` <- 100 * df_kcalGroups$`Supply (kcal/capita/day)` / df_kcalGroups$`Total (kcal/capita/day)`
df_kcalGroups$`Supply (% of total), logged` <- log(df_kcalGroups$`Supply (% of total)`)
df_kcalGroups$`Supply (kcal/capita/day), logged` <- log(df_kcalGroups$`Supply (kcal/capita/day)`)
#---
df_plot <- subset(df_kcalGroups, Year == 2017)

gg <- ggplot(df_plot, aes(x = `GDP/capita (USD, 2015 prices), logged`,
                          y = `Supply (% of total), logged`,
                          #y = `Supply (kcal/capita/day), logged`,
                          group = Region, fill = Region,
                          shape = Region))
gg <- gg + geom_point(alpha = 0.6, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
gg <- gg + facet_wrap(~Item, ncol = 2, scales = "free_y")
# gg <- gg + guides(fill = guide_legend(nrow = 2, byrow = T, override.aes = list(linetype = 0)),
#                   color = guide_legend(override.aes = list(linetype = 0)))
gg

# df_plot2 <- df_plot[, c("Area", "Region", "Item", "Supply (kcal/capita/day)")]
# df_plot2$`Supply (kcal/capita/day)` <- log(df_plot2$`Supply (kcal/capita/day)`)
# df_plot2 <- df_plot2 %>% spread(Item, `Supply (kcal/capita/day)`)

df_plot2 <- df_plot[, c("Area", "Region", "Item", "Supply (% of total)")]
df_plot2 <- df_plot2 %>% spread(Item, `Supply (% of total)`)

mod <- lm(`Fat`~`Carbohydrate`, df_plot2)
#summary(mod)
m <- round(mod$coefficients[2], 2)
b <- round(mod$coefficients[1], 2)
df_out <- as.data.frame(broom::glance(mod))
adjR2 <- round(df_out$adj.r.squared, 2)
facet_labels <- paste0("The fat-carb frontier", "\nAdj. R-squared = ", adjR2, ", Slope = ", m, ", Y intercept = ", b)

#colnames(df_plot2)[3:5] <- paste(colnames(df_plot2)[3:5], "(kcal/capita/day)")
colnames(df_plot2)[3:5] <- paste(colnames(df_plot2)[3:5], "(% of diet)")

gg <- ggplot(df_plot2, aes(x = `Carbohydrate (% of diet)`,
                          y = `Fat (% of diet)`,
                          group = Region, fill = Region,
                          shape = Region))
gg <- gg + geom_point(alpha = 0.6, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
gg <- gg + labs(subtitle = facet_labels)
gg
#--------------------------------------------------------------------------
rm(df_foodGroups, df_kcalGroups, df_plot, df_plot2, df_tot); gc()
#==========================================================================
#==========================================================================
#==========================================================================
# ENGEL'S LAW
# Create file for expenditure share formulation of Engel's Law
this_folder <- "C:/Users/bensc/OneDrive/Documents/Empirical laws, old and new, in the nutrition transition/"
this_file <- "World Bank ICP Data raw.csv"
this_filepath <- paste0(this_folder, this_file)
df_icp <- read.csv(this_filepath, stringsAsFactors = F)
#colnames(df_icp)
keep_cols <- c(1, 3, 5, 7)
df_icp <- df_icp[, keep_cols]
colnames(df_icp) <- c("Country", "Element", "Good", "Value")
df_icp$Value <- as.numeric(df_icp$Value)
df_icp$Year <- 2017
#unique(df_icp$Element)
element_vec <- c("Expenditure, market exchange rate-based (US$, billions)",
                 "Expenditure per capita, market exchange rate-based (US$)",
                 "Expenditure component share of GDP (GDP = 100%)",
                 "Expenditure per capita, PPP-based (US$)",
                 "Expenditure, PPP-based (US$, billions)")
# Population
# "Expenditure, market exchange rate-based (US$, billions)"
# "Expenditure per capita, PPP-based (US$)"
df_icp <- subset(df_icp, Element %in% element_vec)
#---
region_vec <- unique(df_icp$Country[grep("ICP", df_icp$Country)])
#region_vec <- c("WORLD", region_vec)
df_icp <- subset(df_icp, !(Country %in% region_vec))
#---
df_icp$Country[which(df_icp$Country == "WORLD")] <- "World"
df_icp$Country[grep("Gambia", df_icp$Country)] <- "Gambia"
df_icp$Country[grep("Iran", df_icp$Country)] <- "Iran"
df_icp$Country[grep("S茫o Tom茅 and Principe", df_icp$Country)] <- "Sao Tome and Principe"
df_icp$Country[grep("C么te d'Ivoire", df_icp$Country)] <- "C魌e d'Ivoire"
df_icp$Country[grep("Venezuela", df_icp$Country)] <- "Venezuela"
df_icp$Country[grep("Egypt", df_icp$Country)] <- "Egypt"
df_icp$Country[grep("Micronesia", df_icp$Country)] <- "Micronesia"
df_icp$Country[grep("Bahamas", df_icp$Country)] <- "Bahamas"
#df_icp$Country[grep("Samoa", df_icp$Country)] <- "Samoa"
df_icp$Country[grep("Virgin Islands, British", df_icp$Country)] <- "British Virgin Islands"
df_icp$Country[grep("Hong Kong SAR, China", df_icp$Country)] <- "China, Hong Kong SAR"
df_icp$Country[grep("Macao SAR, China", df_icp$Country)] <- "China, Macao SAR"
#---------------------------------------------------------------------------
# Merge with df_fbal to get Region groupings
df_key <- subset(df_fbal[, c("Area", "Region", "Year")], Year == 2017)
colnames(df_key)[1] <- "Country"
ind_dup <- which(duplicated(df_key$Country))
df_key <- df_key[-ind_dup, ]
# length(unique(df_fbal$Country))
# length(unique(df_icp$Country))
# setdiff(unique(df_key$Country), unique(df_icp$Country))
# setdiff(unique(df_icp$Country), unique(df_key$Country))
# unique(df_key$Country[grep("Congo", df_key$Country)])
df_icp <- merge(df_icp, df_key, by = c("Country", "Year"))
#---
df_gdpPcap <- subset(df_icp, Good %in% c("1000000:GROSS DOMESTIC PRODUCT") &
                       Element != "Expenditure component share of GDP (GDP = 100%)")
#"SP.POP.TOTL.ICP:Population"
df_gdpPcap$Region <- NULL
df_gdpPcap <- df_gdpPcap %>% spread(Element, Value)
u <- colnames(df_gdpPcap)
colnames(df_gdpPcap) <- gsub("Expenditure", "GDP", u)
df_gdpPcap$Good <- NULL
#---
#unique(df_icp$Good)
df_foodGdpPcap <- subset(df_icp, Good %in% c("1101100:FOOD")  &
                           Element != "Expenditure component share of GDP (GDP = 100%)")
df_foodGdpPcap$Region <- NULL
df_foodGdpPcap <- df_foodGdpPcap %>% spread(Element, Value)
u <- colnames(df_foodGdpPcap)
colnames(df_foodGdpPcap) <- gsub("Expenditure", "Food GDP", u)
df_foodGdpPcap$Good <- NULL
#---
df_gdpPcap <- merge(df_gdpPcap, df_foodGdpPcap, by = c("Country", "Year"))
#---
colnames(df_icp)[(ncol(df_icp) - 1)] <- "Expenditure"
df_icp$`Expenditure, logged` <- log(df_icp$Expenditure)
ind_rm <- which(is.nan(df_icp$`Expenditure, logged`)|is.infinite(df_icp$`Expenditure, logged`)|is.na(df_icp$`Expenditure, logged`))
df_icp <- df_icp[-ind_rm, ]
df_icp <- subset(df_icp, Good != "1000000:GROSS DOMESTIC PRODUCT")
#---
df_icp <- merge(df_icp, df_gdpPcap, by = c("Country", "Year"))
#--------------------------------------------------------------------------
# Write
this_folder <- "C:/Users/bensc/OneDrive/Documents/Empirical laws, old and new, in the nutrition transition/"
this_file <- "Engels Law WB ICP data.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(df_icp, this_filepath, row.names = F)
#--------------------------------------------------------------------------
# Check
shape_vec <- c(21:24, 4)
point_size <- 1.5
smallPoint_size <- 1
label_size <- 2.5
smallLabel_size <- 2
title_size <- 8
subtitle_size <- 7
legendText_size <- 7
axisText_size <- 6
axisTitle_size <- 7
facetTitle_size <- 7
shape_vec <- c(21:24, 4)

df_plot <- subset(df_icp, Good == "1101100:FOOD" & Year == 2017)

df_plot$Element[grep("share", df_plot$Element)] <- "Expenditure as % of GDP"
df_plot$Element[grep("per capita", df_plot$Element)] <- "Expenditure/capita (USD, market exchange rate-based)"
these_elements <- unique(df_plot$Element)

facet_labels <- c()
adjR2_vec <- c()
for(i in 1:length(these_elements)){
  this_element <- these_elements[i]
  this_df_plot <- subset(df_plot, Element == this_element)
  mod <- lm(`Expenditure, logged` ~ `GDP/capita (USD), logged`, this_df_plot)
  #summary(mod)
  m <- round(mod$coefficients[2], 2)
  b <- round(mod$coefficients[1], 2)
  df_out <- as.data.frame(broom::glance(mod))
  adjR2 <- round(df_out$adj.r.squared, 2)
  facet_labels[i] <- paste0(this_element, "\nAdj. R-squared = ", adjR2, "\nSlope = ", m, "\nY intercept = ", b)
  adjR2_vec[i] <- adjR2
}

names(facet_labels) <- these_elements

n <- length(unique(df_plot$Region))
bag_of_colors <- randomcoloR::distinctColorPalette(k = 2 * n)
color_vec <- sample(bag_of_colors, n)

gg <- ggplot(df_plot, aes(x = `GDP/capita (USD), logged`,
                          y = `Expenditure, logged`,
                          #label = label_these,
                          group = Region, fill = Region, shape = Region))
gg <- gg + geom_point(alpha = 0.6, size = smallPoint_size, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
#gg <- gg + geom_smooth(method = lm, se = F)
gg <- gg + facet_wrap(~Element, scales = "free_y",
                      labeller = labeller(Element = facet_labels))
#gg <- gg + geom_text_repel(color = "black", size = smallLabel_size)
gg <- gg + theme(strip.background = element_blank(),
                 strip.text.x = element_text(hjust = -0.01, size = facetTitle_size),
                 #legend.direction = "vertical",
                 #legend.spacing.y = unit(1, 'cm'),
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendText_size),
                 axis.text = element_text(size = axisText_size),
                 axis.title = element_text(size = axisTitle_size))
gg <- gg + guides(fill = guide_legend(#nrow = 2,
  override.aes = list(linetype = 0)),
  color = guide_legend(override.aes = list(linetype = 0)))
gg


these_goods <- c("1111000:RESTAURANTS AND HOTELS", "9120000:ACTUAL EDUCATION",
                "9110000:ACTUAL RECREATION AND CULTURE",
                "1103000:CLOTHING AND FOOTWEAR",
                #"1101100:FOOD",
                "9080000:ACTUAL HEALTH",
                "1501100:MACHINERY AND EQUIPMENT",
                "1501200:CONSTRUCTION",
                "1105000:FURNISHINGS, HOUSEHOLD EQUIPMENT AND ROUTINE HOUSEHOLD MAINTENANCE",
                "1107000:TRANSPORT",
                "1107100:PURCHASE OF VEHICLES",
                "1108000:COMMUNICATION",
                # "1101160:Fruit",
                # "1101170:Vegetables", 
                # "1101180:Sugar, jam, honey, chocolate and confectionery",
                #"1101130:Fish and seafood",
                # "1101140:Milk, cheese and eggs",
                # "1101120:Meat",
                # "1101150:Oils and fats",
                "9060000:ACTUAL HOUSING, WATER, ELECTRICITY, GAS AND OTHER FUELS")
df_plot <- subset(df_icp, Good %in% these_goods & Year == 2017 &
                    Element == "Expenditure per capita, market exchange rate-based (US$)")

colnames(df_plot)[5] <- "Expenditure/capita, logged"

df_plot$Good[grep("RESTAURANTS", df_plot$Good)] <- "Restaurants and hotels"
df_plot$Good[grep("EDUCATION", df_plot$Good)] <- "Education"
df_plot$Good[grep("RECREATION", df_plot$Good)] <- "Recreation and culture"
df_plot$Good[grep("CLOTHING", df_plot$Good)] <- "Clothing and footwear"
df_plot$Good[grep("CONSTRUCTION", df_plot$Good)] <- "Construction"
df_plot$Good[grep("MACHINERY", df_plot$Good)] <- "Machinery and equipment"
df_plot$Good[grep("FURNISHINGS", df_plot$Good)] <- "Furnishings, household equipment,\n& routine household maintenance"
df_plot$Good[grep("ACTUAL HOUSING", df_plot$Good)] <- "Housing, electricity,\n& other utilities"
df_plot$Good[grep("VEHICLES", df_plot$Good)] <- "Purchase of vehicles"
df_plot$Good[grep("TRANSPORT", df_plot$Good)] <- "Transport"
df_plot$Good[grep("COMMUNICATION", df_plot$Good)] <- "Communication"
df_plot$Good[grep("HEALTH", df_plot$Good)] <- "Health"
these_goods <- unique(df_plot$Good)
# # Remove outliers
# good_vec <- unique(df_plot$Good)
# # for(i in 1:length(good_vec)){
# #   this_df <- subset(df_plot, Good == good_vec[i])
# #   hist(this_df$`Expenditure (% of GDP), logged`)
# #   ind_rm <- which(this_df$`Expenditure (% of GDP), logged` < 0)
# #   print(good_vec[i])
# #   print(this_df$Country[ind_rm])
# # }
# df_plot <- subset(df_plot, !(Country %in% c("Chad", "Suriname", "Botswana", "Guyana", "Kuwait")))

gg <- ggplot(df_plot, aes(x = `GDP/capita (USD), logged`,
                          y = `Expenditure/capita, logged`))
gg <- gg + geom_point()
gg <- gg + facet_wrap(~Good)
gg

facet_labels <- c()
adjR2_vec <- c()
for(i in 1:length(these_goods)){
  this_good <- these_goods[i]
  this_df_plot <- subset(df_plot, Good == this_good)
  mod <- lm(`Expenditure/capita, logged` ~ `GDP/capita (USD), logged`, this_df_plot)
  #summary(mod)
  m <- round(mod$coefficients[2], 2)
  b <- round(mod$coefficients[1], 2)
  df_out <- as.data.frame(broom::glance(mod))
  adjR2 <- round(df_out$adj.r.squared, 2)
  facet_labels[i] <- paste0(this_good, "\nAdj. R-squared = ", adjR2, "\nSlope = ", m, ", Y intercept = ", b)
  adjR2_vec[i] <- adjR2
}

names(facet_labels) <- these_goods

gg <- ggplot(df_plot, aes(x = `GDP/capita (USD), logged`,
                          y = `Expenditure/capita, logged`,
                          #label = label_these,
                          group = Region, fill = Region, shape = Region))
gg <- gg + geom_point(alpha = 0.6, size = smallPoint_size, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
#gg <- gg + geom_smooth(method = lm, se = F)
gg <- gg + facet_wrap(~Good, ncol = 4,
                      labeller = labeller(Good = facet_labels))
#gg <- gg + geom_text_repel(color = "black", size = smallLabel_size)
gg <- gg + theme(strip.background = element_blank(),
                 strip.text.x = element_text(hjust = -0.01, size = facetTitle_size),
                 #legend.direction = "vertical",
                 #legend.spacing.y = unit(1, 'cm'),
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendText_size),
                 axis.text = element_text(size = axisText_size),
                 axis.title = element_text(size = axisTitle_size))
gg <- gg + guides(fill = guide_legend(#nrow = 2,
  override.aes = list(linetype = 0)),
  color = guide_legend(override.aes = list(linetype = 0)))
gg
# gg <- shift_legend2(gg)
# gg <- ggplotify::as.ggplot(gg)
#==========================================================================
# Scrape data from Houthakker paper
# library(tabulizer)
# this_folder <- "C:/Users/bensc/OneDrive/Documents/Empirical laws, old and new, in the nutrition transition/"
# this_file <- "HouthakkerEngelCentEC1957.pdf"
# this_filepath <- paste0(this_folder, this_file)
# outlist <- extract_tables(this_filepath, pages = c(11), method = "stream")
# #View(outlist[[1]])
# df_raw <- as.data.frame(outlist[[2]])
# df_raw <- df_raw %>% mutate_if(is.factor, as.character)
# df_raw <- df_raw[-c(1, 2, 15:nrow(df_raw)), ]
# replace_this <- "-|_|, | ,|,"; with_this <- ""
# df_raw <- as.data.frame(apply(df_raw,
#                               2, function(x) gsub(replace_this, with_this, x)))
# replace_this <- "\\. | \\."; with_this <- "\\."
# df_raw <- as.data.frame(apply(df_raw,
#                               2, function(x) gsub(replace_this, with_this, x)))

