library(tidyverse)

# dfConfirmed <- read.csv(file = "csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
# dfDeaths <- read.csv(file = "csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
# dfRecovered <- read.csv(file = "csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

dfConfirmedv2 <- read.csv(file = "csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
dfDeathsv2 <- read.csv(file = "csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

cleanDF <- function(df, valueColName) { 
  df$Province.State <- as.character(df$Province.State)
  df$Country.Region <- as.character(df$Country.Region)
  
  df$Province.State[df$Province.State == ""] <- df$Country.Region[df$Province.State == ""]
  
  df <- df %>% mutate(keyCol = unite(df[, 1:4], col = "key", sep = ";")$key)
  df <- df[, -c(1:4)]
  df <- df[, c(ncol(df), 2:ncol(df)-1)]
  
  df <- gather(df, "date", valueCol, -keyCol)
  df$date <- as.Date(str_replace_all(df$date, "X", ""), format = "%m.%d.%y")
  colnames(df)[colnames(df) == "valueCol"] <- valueColName
  colnames(df)[colnames(df) == "date"] <- "Date"
  
  df %>% separate(col = 1, into = c("Province", "Country", "Latitude", "Longitude"), sep = ";")
}

dfConfirmed.Clean <- cleanDF(dfConfirmedv2, "Confirmed")
dfDeaths.Clean <- cleanDF(dfDeathsv2, "Deaths")
# dfRecovered.Clean <- cleanDF(dfRecovered, "Recovered")

dfFull <- full_join(dfConfirmed.Clean, dfDeaths.Clean, by = c("Province", "Country", "Latitude", "Longitude", "Date"))

# dfFull <- full_join(dfFull, dfRecovered.Clean, by = c("Province", "Country", "Latitude", "Longitude", "Date"))

dfCountryContinentMapping <- read.csv(file = "Data/CountryContinentCrosswalk.csv")


dfFull <- left_join(dfFull, dfCountryContinentMapping[, c("Country", "continent", "sub_region")], by = "Country")

dfFull$continent <- as.character(dfFull$continent)
dfFull$sub_region <- as.character(dfFull$sub_region)

##### Lists of locations (Continent, Sub-Region, Country) #####
dfTmp <- unique(dfFull[, c("Country", "continent", "sub_region")])
dfTmp$continent_sub_region <- paste0(dfTmp$continent, " - ", dfTmp$sub_region)
dfTmp <- unique(dfTmp[, c("continent_sub_region", "Country")])

listCountries <- split(dfTmp, dfTmp$continent_sub_region, drop = T)
for (i in seq_along(listCountries)) {
    listCountries[[i]]<- listCountries[[i]]$Country
}


dfTmp <- unique(dfFull[, c("continent", "sub_region")])
listSubRegions <- split(dfTmp, dfTmp$continent, drop = T)
for (i in seq_along(listSubRegions)) { 
  listSubRegions[[i]] <- listSubRegions[[i]]$sub_region
  }

listContinents <- list(unique(dfFull$continent))
names(listContinents) <- "Continents"

dfFull <- dfFull %>% arrange(Province, Country, Date) %>%
  group_by(Province, Country) %>%
  mutate(LaggedCases = dplyr::lag(Confirmed, 1, default = 0),
         LaggedDeaths = dplyr::lag(Deaths, 1, default = 0)
  )

dfFull <- dfFull %>% mutate(NewCases = Confirmed - LaggedCases,
                            NewDeaths = Deaths - LaggedDeaths,
) %>% select(-c(LaggedCases, LaggedDeaths))

dfFull$World <- "World"

save(dfFull, 
     dfConfirmed.Clean, 
     dfDeaths.Clean, 
     listCountries,
     listSubRegions,
     listContinents,
     file = "Data/data.RData")

