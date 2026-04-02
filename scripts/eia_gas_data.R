library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(formattable)
library(data.table)
library(sf)
library(zoo)
library(readxl)
library(jsonlite)

options(timeout=300)

download.file("https://www.eia.gov/petroleum/gasdiesel/xls/pswrgvwall.xls", "eia_gas_prices.xls")

#load in file
gas <- read_excel("eia_gas_prices.xls", sheet = 4, skip =2) %>% clean_names()

#change names for regions needed, take out extraneous columns
gas1 <- gas %>% rename(us = weekly_u_s_regular_all_formulations_retail_gasoline_prices_dollars_per_gallon, east_coast = weekly_east_coast_regular_all_formulations_retail_gasoline_prices_dollars_per_gallon, midwest = weekly_midwest_regular_all_formulations_retail_gasoline_prices_dollars_per_gallon, gulf_coast = weekly_gulf_coast_regular_all_formulations_retail_gasoline_prices_dollars_per_gallon, rocky_mountain = weekly_rocky_mountain_regular_all_formulations_retail_gasoline_prices_dollars_per_gallon, west_coast = weekly_west_coast_regular_all_formulations_retail_gasoline_prices_dollars_per_gallon)

gas2 <- gas1 %>% select("date","us","east_coast","midwest","gulf_coast","rocky_mountain","west_coast")

#fix date column
gas2$date <- as.Date(gas2$date, format = "%Y-%m-%d")

#filter to last 10 years
gas3 <- gas2 %>% 
  filter(floor_date(as.Date(date), "month") >= floor_date(Sys.Date() %m-% years(10)

gas_long <- gas3 %>%
  pivot_longer(
    cols = 2:7,
    names_to = "region",
    values_to = "value"
  )

gas_long1 <- gas_long %>% mutate(category = "Regular Gasoline, All Formulations")

gas_long2 <- gas_long1 %>% mutate(region = case_when(
  region == "us" ~ "United States",
  region == "east_coast" ~ "East Coast",
  region == "midwest" ~ "Midwest",
  region == "gulf_coast" ~ "Gulf Coast",
  region == "rocky_mountain" ~ "Rocky Mountain",
  region == "west_coast" ~ "West Coast"
))

#add in location column 
locations <- read_csv("https://raw.githubusercontent.com/abcotvdata/price_tracker/refs/heads/main/housing/housing_data.csv") %>% select("location","state_abbreviation","state_spelled_out","region")
locations <- locations[! duplicated(locations), ]
gas_long3 <- left_join(locations, gas_long2, by = "region")

#adjust for inflation
inflation <- read_csv("https://raw.githubusercontent.com/abcotvdata/price_tracker/refs/heads/main/inflation/inflation_adjustment.csv")
inflation <- inflation %>% rename(month = date)

#add a column to match date from inflation file
gas_long3$month <- floor_date(gas_long3$date, "month")

#calcuate inflation
gas_long4 <- left_join(gas_long3, inflation, by = "month")

#remove NA/NULL inflation values
gas_long5 <- gas_long4 %>% 
  mutate(inflation_adjustment = coalesce(inflation_adjustment, 1))

gas_long6 <- gas_long5 %>% mutate(value_inflation_adjusted = round(value*inflation_adjustment,2))

#add date updated column and remove month column
gas_long7 <- gas_long6 %>% mutate(date_updated = (date)+1) %>% select(-"month")

#reorder columns
gas_final <- gas_long7 %>% select("location","state_abbreviation","state_spelled_out","region","category","date","value","inflation_adjustment","value_inflation_adjusted","date_updated")

#write to files
write_csv(gas_final,"transportation/gas_data.csv")
write_json(gas_final, "transportation/gas_data.json", pretty = TRUE)

#clean up temp files
file.remove("eia_gas_prices.xls")
