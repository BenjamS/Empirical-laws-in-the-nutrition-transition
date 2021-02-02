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
df_gdpRaw <- read.csv(this_filepath, stringsAsFactors = F)
rm_cols <- c("Area.Code", "Item.Code", "Element.Code")
rm_cols <- which(colnames(df_gdpRaw) %in% rm_cols)
df_gdpRaw <- df_gdpRaw[, -rm_cols]
#unique(df_gdpRaw$Item)
df_gdpRaw$Item <- as.character(df_gdpRaw$Item)
df_gdpRaw$Area <- as.character(df_gdpRaw$Area)
u <- colnames(df_gdpRaw)
df_gdpRaw <- df_gdpRaw[, -grep("F", u)]
u <- colnames(df_gdpRaw)
df_gdpRaw <- df_gdpRaw[, -grep("N", u)]
v <- colnames(df_gdpRaw)[5:ncol(df_gdpRaw)]
colnames(df_gdpRaw)[5:ncol(df_gdpRaw)] <- gsub("Y", "", v)
#---------------------------------------------------------------------------
# Check available years
#colnames(df_gdpRaw)
#---------------------------------------------------------------------------
available_yrs <- as.character(c(1970:2018))
df_gdpRaw <- gather_(df_gdpRaw, "Year", "Value", gather_cols = available_yrs)
#df_gdpRaw$Unit <- NULL
#---------------------------------------------------------------------------
#unique(df_gdpRaw$Item)
#unique(df_gdpRaw$Element)
item_vec <- c("Gross Domestic Product per capita", "Gross Domestic Product")
this_element <- "Value US$, 2015 prices" #"Value US$" #Use 2015 constant prices to see linear relation between Bennett's Law (and carbohydrate law) slope and y intercept.
df_gdpRaw <- subset(df_gdpRaw, Item %in% item_vec &
                   Element == this_element)
df_gdpRaw <- df_gdpRaw[, c("Area", "Year", "Item", "Unit", "Value")]
df_gdpRaw$Unit <- NULL
df_gdpRaw <- df_gdpRaw %>% spread(Item, Value)
colnames(df_gdpRaw)[3:4] <- c("GDP (million USD)", "GDP/capita (USD)")
df_gdpRaw$`GDP (million USD), logged` <- log(df_gdpRaw$`GDP (million USD)`)
df_gdpRaw$`GDP/capita (USD), logged` <- log(df_gdpRaw$`GDP/capita (USD)`)
df_gdpRaw$Year <- as.integer(df_gdpRaw$Year)
#--------------------------------------------------------------------------
# Is GDP/capita GDP over pop? (Looks like no.)
#df_gdpRaw$`Population (millions) test` <- df_gdpRaw$`GDP (million USD)` / df_gdpRaw$`GDP/capita (USD)`
#--------------------------------------------------------------------------
df_bennett <- merge(df_fbal, df_gdpRaw, by = c("Area", "Year"))
# unique(df_gdp$Area[grep("China", df_gdp$Area)])
# unique(df_fbal$Area[grep("Taiwan", df_fbal$Area)])
# Create simple GDP/capita (as GDP over pop)
df_bennett$`GDP/capita (USD) test` <- df_bennett$`GDP (million USD)` / df_bennett$`Population (millions)`
# Is it the same as the GDP/cap provided by FAO?
# Pretty close but not exact.
df_bennett$`GDP/capita (USD) test` <- NULL
#--------------------------------------------------------------------------


# item_vec <- c("Gross Domestic Product per capita", "Gross Domestic Product")
# these_elements <- c("Value US$", "Value US$, 2015 prices")
# df_gdpRaw <- subset(df_gdpRaw, Item %in% item_vec &
#                    Element == these_elements)
# df_gdpRaw <- df_gdpRaw[, c("Area", "Year", "Item", "Element", "Unit", "Value")]
# #---------------------------------------------------------------------------
# df_gdpConstPrice <- subset(df_gdpRaw, Element == "Value US$, 2015 prices")
# df_gdpConstPrice$Element <- NULL
# df_gdpConstPrice$Unit <- NULL
# df_gdpConstPrice <- df_gdpConstPrice %>% spread(Item, Value)
# colnames(df_gdpConstPrice)[3:4] <- c("GDP (million USD), 2015 prices", "GDP/capita (USD), 2015 prices")
# 
# df_gdpConstPrice$`GDP (million USD), 2015 prices, logged` <- log(df_gdpConstPrice$`GDP (million USD), 2015 prices`)
# df_gdpConstPrice$`GDP/capita (USD), 2015 prices, logged` <- log(df_gdpConstPrice$`GDP/capita (USD), 2015 prices`)
# df_gdpConstPrice$Year <- as.integer(df_gdpConstPrice$Year)
# #---------------------------------------------------------------------------
# df_gdpCurrPrice <- subset(df_gdpRaw, Element == "Value US$")
# df_gdpCurrPrice$Element <- NULL
# df_gdpCurrPrice$Unit <- NULL
# df_gdpCurrPrice <- df_gdpCurrPrice %>% spread(Item, Value)
# colnames(df_gdpCurrPrice)[3:4] <- c("GDP (million USD), current prices", "GDP/capita (USD), current prices")
# 
# df_gdpCurrPrice$`GDP (million USD), current prices, logged` <- log(df_gdpCurrPrice$`GDP (million USD), current prices`)
# df_gdpCurrPrice$`GDP/capita (USD), current prices, logged` <- log(df_gdpCurrPrice$`GDP/capita (USD), current prices`)
# df_gdpCurrPrice$Year <- as.integer(df_gdpCurrPrice$Year)
# #---------------------------------------------------------------------------
# df_gdp <- merge(df_gdpCurrPrice, df_gdpConstPrice, by = c("Area", "Year"), all = T)
# #---------------------------------------------------------------------------
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
#df_bennett <- merge(df_fbal, df_gdp, by = c("Area", "Year"), all = T)
#rm(df_gdp, df_gdpRaw, df_gdpConstPrice, df_gdpCurrPrice)
#--------------------------------------------------------------------------
# Get region groupings
this_subfolder <- "region_country_files"
this_folder <- paste0(this_folder, this_subfolder)
df_bennett <- FAOdat_createRegionGroups(df_bennett,
                                     folder = this_folder)
#--------------------------------------------------------------------------
# Correct/modify country names (partly to harmonize with the WB ICP data)
df_bennett$Area[grep("Tanzania", df_bennett$Area)] <- "Tanzania"
df_bennett$Area[grep("C么te d'Ivoire", df_bennett$Area)] <- "C魌e d'Ivoire"
df_bennett$Area[grep("Viet Nam", df_bennett$Area)] <- "Vietnam"
df_bennett$Area[grep("Iran", df_bennett$Area)] <- "Iran"
df_bennett$Area[grep("Lao", df_bennett$Area)] <- "Lao PDR"
df_bennett$Area[grep("Venezuela", df_bennett$Area)] <- "Venezuela"
df_bennett$Area[grep("Bolivia", df_bennett$Area)] <- "Bolivia"
df_bennett$Area[grep("Democratic Republic of the Congo", df_bennett$Area)] <- "Congo, Dem. Rep."
df_bennett$Area[grep("Micronesia", df_bennett$Area)] <- "Micronesia"
df_bennett$Area[which(df_bennett$Area == "Congo")] <- "Congo, Rep."
df_bennett$Area[which(df_bennett$Area == "Republic of Korea")] <- "Korea, Rep."
df_bennett$Area[which(df_bennett$Area == "United States of America")] <- "United States"
df_bennett$Area[grep("Sint Maarten", df_bennett$Area)] <- "Sint Maarten"
df_bennett$Area[grep("Moldova", df_bennett$Area)] <- "Moldova"
df_bennett$Area[which(df_bennett$Area == "Czechia")] <- "Czech Republic"
df_bennett$Area <- gsub("Saint", "St.", df_bennett$Area)
df_bennett$Unit <- NULL
#unique(df_bennett$Area[grep("Libya", df_bennett$Area)])
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
# unique(df_wdi$Indicator[grep("GDP", df_wdi$Indicator, ignore.case = T)])
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
# df_bennett <- merge(df_bennett, df_wdi, by = c("Area", "Year"))
# rm(df_wdi)
#---------------------------------------------------------------------------
# Write
this_folder <- "C:/Users/bensc/OneDrive/Documents/Empirical laws, old and new, in the nutrition transition/"
this_file <- "Bennetts Law FAO FBS and WDI data.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(df_bennett, this_filepath, row.names = F)
#--------------------------------------------------------------------------
# Check (classic Bennett analysis)
gdpPop_vars <- c("GDP (million USD), logged",
                 "GDP (million USD)",
                 "GDP/capita (USD), logged",
                 "GDP/capita (USD)",
                 "Population (millions), logged",
                 "Population (millions)")

these_cols <- c("Area", "Year", "Region", "Element",
                "Item", "Value", gdpPop_vars)
# these_items <- c("Cereals - Excluding Beer", "Starchy Roots",
#                  "Grand Total")
exclude_items <- c("Animal fats", "Meat")
#unique(df_bennett$Item)
#setdiff(these_cols, colnames(df_bennett))
df_foodShare <- subset(df_bennett[, these_cols], !(Item %in% exclude_items) &
                         Element == "Food supply (kcal/capita/day)")

df_tot <- subset(df_foodShare[, c("Area", "Year", "Item", gdpPop_vars, "Value")], Item == "Grand Total")

df_tot$Item <- NULL
colnames(df_tot)[ncol(df_tot)] <- "Total (kcal/capita/day)"
df_foodShare <- subset(df_foodShare, Item != "Grand Total")
df_foodShare$Item[grep("Cereals|Starchy", df_foodShare$Item)] <- "Cereals & starchy roots"
df_foodShare$Item[grep("Fruits|Vegetables", df_foodShare$Item)] <- "Fruits & vegetables"
df_foodShare$Item[grep("Animal Products", df_foodShare$Item)] <- "Animal products"
df_foodShare$Item[grep("Sugar", df_foodShare$Item)] <- "Sugar & sweeteners"
df_foodShare <- df_foodShare %>% 
  group_by(Region, Area, Year, Item) %>%
  summarise(Value = sum(Value, na.rm = T)) %>% as.data.frame()
#---
df_foodShare <- merge(df_foodShare, df_tot, by = c("Area", "Year"))
colnames(df_foodShare)[which(colnames(df_foodShare) == "Value")] <- "Diet share (kcal/capita/day)"
df_foodShare$`Diet share (%)` <- 100 * df_foodShare$`Diet share (kcal/capita/day)` /
  df_foodShare$`Total (kcal/capita/day)`
df_foodShare$`Diet share (%), logged` <- 
  log(df_foodShare$`Diet share (%)`)
df_foodShare$`Diet share (kcal/capita/day), logged` <- 
  log(df_foodShare$`Diet share (kcal/capita/day)`)
#-----------------------------------------------------------------------------
# Subset data for just cereals and starchy roots
df_plot <- subset(df_foodShare, Item == "Cereals & starchy roots" &
                    Year == 2017 & Region != "Aggregated")
colnames(df_plot) <- gsub("Diet share", "Cereals & starchy roots", colnames(df_plot))
colnames(df_plot) <- gsub("%", "% of diet", colnames(df_plot))
#-----------------------------------------------------------------------------
# Fit parameters
mod <- lm(`Cereals & starchy roots (% of diet), logged` ~ `GDP/capita (USD), logged`, data = df_plot)
#summary(mod)
#plot(mod$fitted.values, mod$residuals)
#ind_rm <- which(mod$residuals == min(mod$residuals))
#df_plot$Area[ind_rm]
m <- round(mod$coefficients[2], 4)
b <- round(mod$coefficients[1], 4)
df_out <- as.data.frame(broom::glance(mod))
adjR2 <- round(df_out$adj.r.squared, 2)
#-----------------------------------------------------------------------------
# Get colors for region groupings and title
n <- length(unique(df_plot$Region))
bag_of_colors <- randomcoloR::distinctColorPalette(k = 2 * n)
color_vec <- sample(bag_of_colors, n)
this_subtitle <- paste0("Adj. R-squared = ", adjR2, "\nSlope = ", m, ", Y intercept = ", b)
#-----------------------------------------------------------------------------
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
#-----------------------------------------------------------------------------
gg <- ggplot(df_plot, aes(x = `GDP/capita (USD), logged`,
                          y = `Cereals & starchy roots (% of diet), logged`,
                          group = Region, fill = Region,
                          shape = Region))
gg <- gg + geom_smooth(aes(group = NULL, fill = NULL, shape = NULL), method = lm, se = F)
gg <- gg + geom_point(alpha = 0.6, size = point_size, color = "black", stroke = 0.5)
gg <- gg + scale_fill_manual(values = color_vec)
gg <- gg + scale_shape_manual(values = shape_vec)
gg <- gg + labs(subtitle = this_subtitle)#,
gg <- gg + guides(fill = guide_legend(nrow = 2, byrow = T, override.aes = list(linetype = 0)),
                  color = guide_legend(override.aes = list(linetype = 0)))
gg <- gg + theme(legend.position = "bottom",
                 legend.spacing.x = unit(0.25, 'cm'),
                 legend.title = element_blank(),
                 legend.text = element_text(size = legendText_size),
                 plot.title = element_text(size = title_size),
                 plot.subtitle = element_text(size = subtitle_size),
                 axis.title = element_text(size = axisTitle_size),
                 axis.text = element_text(size = axisText_size))
gg_bennett <- gg
#---------------------------------------------------------------------------
these_cols <- c("Area", "Year", "Region", "Item", "Diet share (%), logged",
                gdpPop_vars)
df_cerRootShare <- subset(df_foodShare, Item == "Cereals & starchy roots")
u <- colnames(df_cerRootShare)
colnames(df_cerRootShare) <- gsub("Diet share", "Cereals & starchy roots", u)
u <- colnames(df_cerRootShare)
colnames(df_cerRootShare) <- gsub("%", "% of diet", u)
df_cerRootShare$Item <- NULL
df_params <- df_cerRootShare
df_params <- subset(df_params, Region != "Aggregated")
yr_vec <- unique(df_params$Year)
n_yrs <- length(yr_vec)
mBennett_vec <- c()
bBennett_vec <- c()
adjR2Bennett_vec <- c()
NBennett_vec <- c()
for(i in 1:n_yrs){
  this_year <- yr_vec[i]
  this_df <- subset(df_params, Year == this_year)
  this_df$Year <- NULL
  #---------------------------------------------------
  mod <- lm(`Cereals & starchy roots (% of diet), logged` ~ `GDP/capita (USD), logged`, this_df)
  mBennett_vec[i] <- round(mod$coefficients[2], 6)
  bBennett_vec[i] <- round(mod$coefficients[1], 6)
  df_out <- as.data.frame(broom::glance(mod))
  adjR2Bennett_vec[i] <- df_out$adj.r.squared
  NBennett_vec[i] <- length(mod$fitted.values)
  
}
df_plotBennett <- data.frame(Year = yr_vec, Slope = mBennett_vec, bBennett_vec, adjR2Bennett_vec, NBennett_vec)
colnames(df_plotBennett)[3:5] <- c("Y intercept", "Adj. R-squared", "Sample size")
df_plotBennett$Year <- as.integer(as.character(df_plotBennett$Year))


df_plot <- df_plotBennett %>% gather(Type, Value, Slope:`Sample size`)
gg <- ggplot(df_plot, aes(x = Year, y = Value))
gg <- gg + geom_line()
# gg <- gg + scale_x_continuous(breaks = seq(yr_vec[1], yr_vec[n_yrs - 1], length.out = 24))
gg <- gg + facet_wrap(~Type, ncol = 1, scales = "free_y")
gg <- gg + labs(subtitle = "Bennet's Law parameter trajectory")
gg <- gg + theme(axis.title = element_blank(),
                 #axis.text.x = element_text(angle = 60, hjust = 1),
                 strip.background = element_blank(),
                 strip.text = element_text(size = facetTitle_size),
                 plot.title = element_text(size = title_size),
                 plot.subtitle = element_text(size = subtitle_size),
                 axis.text = element_text(size = axisText_size))
gg_bennettStab <- gg
#----------------------------------------------------------------------------
gg_bennett + gg_bennettStab + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = 'bottom')
#----------------------------------------------------------------------------
# If using GDP/capita in USD at constant prices (2010 or 2015), then there is a
# linear relation between the slope and y-intercept of Bennett's Law.
# Works for the Carbohydrate Law as well.
# Works for the fat carb frontier at current or constant prices
df_plotBen <- subset(df_plotBennett, Year >= 1980)
mod <- lm(Slope ~ `Y intercept`, df_plotBen)
#summary(mod)
mBen <- round(mod$coefficients[2], 3)
bBen <- round(mod$coefficients[1], 3)
df_out <- as.data.frame(broom::glance(mod))
adjR2Ben <- round(df_out$adj.r.squared, 2)
this_subtitleBen <- paste0("Adj. R-squared = ", adjR2Ben, "\nSlope = ", mBen, ", Y intercept = ", bBen)

gg <- ggplot(df_plotBen, aes(x = Slope, y = `Y intercept`,
                             label = Year))
gg <- gg + geom_point()
gg <- gg + geom_text_repel(size = label_size)
gg <- gg + labs(subtitle = this_subtitleBen)
gg <- gg + theme(axis.title = element_text(size = axisTitle_size),
                 axis.text = element_text(size = axisText_size),
                 plot.subtitle = element_text(size = subtitle_size))
gg

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
# Merge with df_bennett to get Region groupings
df_key <- subset(df_bennett[, c("Area", "Region", "Year")], Year == 2017)
colnames(df_key)[1] <- "Country"
ind_dup <- which(duplicated(df_key$Country))
df_key <- df_key[-ind_dup, ]
# length(unique(df_bennett$Country))
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

