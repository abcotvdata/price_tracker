#install.packages("blscrapeR")
library(blscrapeR)
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(blsR)
library(r2r)
options(timeout=300)

#library(usethis)
#edit_r_environ()
#readRenviron("~/.Renviron")
Sys.getenv("BLS_KEY")

# create empty dataframe to hold inflation data for all geographies
all_geographies_inflation <- data.frame(
  year = numeric(),
  period = character(),
  periodName = character(),
  value = numeric(),
  footnotes = character(),
  seriesID = character(),
  date = Date(),
  inflation_adjustment = numeric(),
  geography = character()
)

# create matrix of geographies & corresponding seriesIDs
cpi_by_geo <- cbind(geography = c("National", "Northeast", "Midwest", "South", "West"),
                    seriesid = c("CUUR0000SA0", "CUUR0100SA0", "CUUR0200SA0", "CUUR0300SA0", "CUUR0400SA0"))

# calculate inflation rates for each geography using its all items CPI series
calc_inflation_rates <- function(geo_series) {
  inflation <- bls_api(geo_series[["seriesid"]], startyear = 2015) #all items, all consumers price index CPI
  inflation_current <- bls_api(geo_series[["seriesid"]], startyear = 2025) #%>% select(-4) #for some reason, the above isn't including 2025
  inflation <- rbind(inflation, inflation_current)
  
  # create date from year & period fields
  inflation$date <- paste(inflation$year,inflation$period,sep="-")
  inflation$date <- gsub("M","", inflation$date)
  inflation$date <- paste(inflation$date,"01",sep = "-")
  inflation$date <- as.Date(inflation$date, format = "%Y-%m-%d")
  
  # find latest CPI month & create table with only values from that month
  inflation_latest_idx <- which.max(inflation$date)
  latest_values <- inflation$value[inflation_latest_idx]
  
  # calculate inflation adjustment factor & add to current inflation table
  inflation$inflation_adjustment <- round((latest_values/inflation$value),2)
  
  # set geography for current inflation table
  inflation$geography <- geo_series[["geography"]]

  # merge inflation rows for current geography with matrix of all geographies
  all_geographies_inflation <- merge(cpi_by_geo, inflation, by = "geography", all = TRUE)
  
   # return merged matrix
  return(as.data.frame(all_geographies_inflation))
}

# call function above for each geography/seriesID in matrix
all_geographies_inflation <- apply(cpi_by_geo, 1, calc_inflation_rates)

# turn list into dataframe
all_geographies_inflation <- do.call(rbind.data.frame, all_geographies_inflation)

# remove date NAs, arrange by region and date
all_geographies_inflation <- all_geographies_inflation %>% 
  drop_na(date) %>% 
  arrange(geography, date)

# select only required columns to reduce inflation adjustment file size
inflation_adjustment <- all_geographies_inflation %>% select(date, inflation_adjustment, geography)

# rename geography column to match other scripts
inflation_adjustment <- inflation_adjustment %>% rename(region = geography)

# save file with 3 columns (date, inflation_adjustment, geography)
write_csv(inflation_adjustment, "scripts/inflation_adjustment.csv")
