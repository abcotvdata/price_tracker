
library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(formattable)
library(data.table)
library(sf)
library(zoo)
library(readxl)

options(timeout=300)

download.file("https://www.eia.gov/petroleum/gasdiesel/xls/pswrgvwall.xls", "eia_gas_prices.xls")

#load in file
gas <- read_excel("eia_gas_prices.xls", sheet = 4, skip =2) %>% clean_names()

#change names for regions needed, take out extraneous columns
gas <- gas %>% rename(us = weekly_u_s_regular_all_formulations_retail_gasoline_prices_dollars_per_gallon, east_coast = weekly_east_coast_regular_all_formulations_retail_gasoline_prices_dollars_per_gallon, midwest = weekly_midwest_regular_all_formulations_retail_gasoline_prices_dollars_per_gallon, gulf_coast = weekly_gulf_coast_regular_all_formulations_retail_gasoline_prices_dollars_per_gallon, rocky_mountain = weekly_rocky_mountain_regular_all_formulations_retail_gasoline_prices_dollars_per_gallon, west_coast = weekly_west_coast_regular_all_formulations_retail_gasoline_prices_dollars_per_gallon)

gas <- gas %>% select(1,2,3,7,8,9,10)

#fix date column
gas$date <- as.Date(gas$date, format = "%Y-%m-%d")

#filter to last 10 years
gas <- gas %>% filter(date >= "2015-01-01")

gas_long <- gas %>%
  pivot_longer(
    cols = 2:7,
    names_to = "region",
    values_to = "value"
  )

gas_long <- gas_long %>% mutate(category = "Regular Gasoline, All Formulations")

gas_long <- gas_long %>% mutate(region = case_when(
  region == "us" ~ "United States",
  region == "east_coast" ~ "East Coast",
  region == "midwest" ~ "Midwest",
  region == "gulf_coast" ~ "Gulf Coast",
  region == "rocky_mountain" ~ "Rocky Mountain",
  region == "west_coast" ~ "West Coast"
))

#add in location column 
locations <- read_csv("housing_data.csv") %>% select(2,3,4,5)
locations <- locations[! duplicated(locations), ]
gas_long <- left_join(locations, gas_long, by = "region")

#adjust for inflation
inflation <- read_csv("inflation_adjustment.csv")
inflation <- inflation %>% rename(month = date)

#add a column to match date from inflation file

gas_long$month <- floor_date(gas_long$date, "month")

#calcuate inflation
gas_long <- left_join(gas_long, inflation, by = "month")

gas <- gas_long %>% mutate(value_inflation_adjusted = round(value*inflation_adjustment,2))


#add date updated column and remove month column
gas <- gas %>% mutate(date_updated = (date)+1) %>% select(-8)

#reorder columns

gas <- gas %>% select(1,2,3,4,7,5,6,8,9,10)

write_csv(gas,"gas_data.csv")
write_json(gas, "gas_data.json", pretty = TRUE)
