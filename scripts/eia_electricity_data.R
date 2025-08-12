library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(formattable)
library(data.table)
library(sf)
library(zoo)
library(jsonlite)
library(readxl)

options(timeout=300)

#call file from API, split into 2 calls because of limit

download.file("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=xd9gsM696jmdw5b6UNv2ikUhv2t6ANohM19lwbW5&data[0]=price&facets[sectorid][]=RES&start=2015-01&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000", "eia_electricity_1.json")

electricity_1 <- fromJSON("eia_electricity_1.json")
electricity_1 <- as.data.frame(electricity_1) %>% clean_names() %>% select(6,7,8,11)

download.file("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=xd9gsM696jmdw5b6UNv2ikUhv2t6ANohM19lwbW5&data[0]=price&facets[sectorid][]=RES&start=2015-01&sort[0][column]=period&sort[0][direction]=desc&offset=5000&length=5000", "eia_electricity_2.json")

electricity_2 <- fromJSON("eia_electricity_2.json")
electricity_2 <- as.data.frame(electricity_2) %>% clean_names() %>% select(6,7,8,11)

#combine
electricity <- rbind(electricity_1,electricity_2)

rm(electricity_1,electricity_2)

#get rid of duplicates in data
electricity <- electricity[! duplicated(electricity), ]

#refine values to match other data
electricity$response_data_state_description <- gsub("U.S. Total","United States", electricity$response_data_state_description)

#rename columns to match other data
electricity <- electricity %>% rename(date = response_data_period, state_abbreviation = response_data_stateid, state_spelled_out = response_data_state_description, price_per_kWh = response_data_price)


#download average monthly use, most recent available data to calculate average monthly cost
download.file("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=xd9gsM696jmdw5b6UNv2ikUhv2t6ANohM19lwbW5&frequency=monthly&data[0]=customers&data[1]=sales&facets[sectorid][]=RES&start=2015-01&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000","eia_customers_and_sales_1.json")

customers_and_sales_1 <- fromJSON("eia_customers_and_sales_1.json")
customers_and_sales_1 <- as.data.frame(customers_and_sales_1) %>% clean_names() %>% select(6,7,8,11,12)

#download average monthly use, most recent available data to calculate average monthly cost
download.file("https://api.eia.gov/v2/electricity/retail-sales/data/?api_key=xd9gsM696jmdw5b6UNv2ikUhv2t6ANohM19lwbW5&frequency=monthly&data[0]=customers&data[1]=sales&facets[sectorid][]=RES&start=2015-01&sort[0][column]=period&sort[0][direction]=desc&offset=5000&length=5000","eia_customers_and_sales_2.json")

customers_and_sales_2 <- fromJSON("eia_customers_and_sales_2.json")
customers_and_sales_2 <- as.data.frame(customers_and_sales_2) %>% clean_names() %>% select(6,7,8,11,12)

customers_and_sales <- rbind(customers_and_sales_1,customers_and_sales_2)

rm(customers_and_sales_1,customers_and_sales_2)


#get rid of duplicates in data
customers_and_sales <- customers_and_sales[! duplicated(customers_and_sales), ]

#refine values to match other data
customers_and_sales$response_data_state_description <- gsub("U.S. Total","United States", customers_and_sales$response_data_state_description)

#rename columns to match other data
customers_and_sales <- customers_and_sales %>% rename(date = response_data_period, state_abbreviation = response_data_stateid, state_spelled_out = response_data_state_description)

customers_and_sales$response_data_customers <- as.numeric(customers_and_sales$response_data_customers)
customers_and_sales$response_data_sales <- as.numeric(customers_and_sales$response_data_sales)

#convert million kWhr to kWhr and calculate monthly kWh per customer
customers_and_sales <- customers_and_sales %>% mutate(response_data_sales = response_data_sales*1000000) %>% mutate(kWh_per_customer = response_data_sales/response_data_customers) %>% select(-4,-5)


#make values numerical

electricity$price_per_kWh <- as.numeric(electricity$price_per_kWh)

#convert price to dollars for ease with other values

electricity <- electricity %>% mutate(price_per_kWh = price_per_kWh/100)

#calculate avg cost per month
electricity <- left_join(electricity, customers_and_sales, by = c("date", "state_abbreviation", "state_spelled_out"))

electricity <- electricity %>% mutate(value = price_per_kWh*kWh_per_customer) %>% select(-4,-5)

#adjust dates to match other data
electricity$date <- paste(electricity$date,"01",sep="-")

electricity$date <- as.Date(electricity$date, format = "%Y-%m-%d")

#remove regions from state abbrev/spelled out

regions <- c("MAT","ENC","SAT","ESC","WNC","PACN","PACC","MTN","WSC","NEW")

electricity <- electricity %>% filter(! state_abbreviation %in% regions)

east_coast <- c("ME", "VT","NH","MA","CT","RI","NY","PA","NJ","MD","DE","WV","DC","VA","NC","SC","GA","FL")
west_coast <- c("WA","OR","CA","NV","AZ", "AK","HI")
rocky_mountain <- c("ID","MT","WY","UT","CO")
gulf_coast <- c("NM","TX","AR","LA","MS","AL")
midwest <- c("ND","SD","NE","KS","OK","MN","IA","MO","WI","IL","MI","IN","OH","KY","TN")


#add in regions column
electricity <- electricity %>% mutate(region = case_when(
  state_abbreviation %in% east_coast ~ "East Coast",
  state_abbreviation %in% west_coast ~ "West Coast",
  state_abbreviation %in% rocky_mountain ~ "Rocky Mountain",
  state_abbreviation %in% gulf_coast ~ "Gulf Coast",
  state_abbreviation %in% midwest ~ "Midwest",
  state_spelled_out == "United States" ~ "United States"
)) %>% relocate(region, .after = state_spelled_out)

electricity <- electricity %>% mutate(category = "Residential Electricity") %>% relocate(category, .after = region)

#adjust for inflation
inflation <- read_csv("https://raw.githubusercontent.com/abcotvdata/price_tracker/refs/heads/main/scripts/inflation_adjustment.csv")

electricity <- left_join(electricity, inflation, by = "date")

electricity <- electricity %>% mutate(value_inflation_adjusted = round(value*inflation_adjustment,2))


#add in location column 
locations <- read_csv("https://raw.githubusercontent.com/abcotvdata/price_tracker/refs/heads/main/housing/housing_data.csv") %>% select(2,3)
locations <- locations[! duplicated(locations), ]
electricity <- left_join(locations, electricity, by = "state_abbreviation")


#put in old values for date updated column

electricity <- electricity %>% mutate(date_updated = case_when(
  date == "2025-03-01" ~ "2025-05-22",
  date == "2025-02-01" ~ "2025-04-24",
  date == "2025-01-01" ~ "2025-03-25",
  date == "2024-12-01" ~ "2025-02-27",
  date == "2024-11-01" ~ "2025-01-27",
  date == "2024-10-01" ~ "2024-12-20",
  date == "2024-09-01" ~ "2024-11-27",
  date == "2024-08-01" ~ "2024-10-28",
  date == "2024-07-01" ~ "2024-09-25",
  date == "2024-06-01" ~ "2024-08-27",
  date == "2024-05-01" ~ "2024-07-25",
  date == "2024-04-01" ~ "2024-06-26",
  date == "2024-03-01" ~ "2024-05-23",
  date == "2024-02-01" ~ "2024-04-25",
  date == "2024-01-01" ~ "2024-03-27",
  date == "2023-12-01" ~ "2024-10-28",
  date == "2023-11-01" ~ "2024-10-28",
  date == "2023-10-01" ~ "2024-10-28",
  date == "2023-09-01" ~ "2024-10-28",
  date == "2023-08-01" ~ "2024-10-28",
  date == "2023-07-01" ~ "2024-10-28",
  date == "2023-06-01" ~ "2024-10-28",
  date == "2023-05-01" ~ "2024-10-28",
  date == "2023-04-01" ~ "2024-10-28",
  date == "2023-03-01" ~ "2024-10-28",
  date == "2023-02-01" ~ "2024-10-28",
  date == "2023-01-01" ~ "2024-10-28",
  date == "2022-12-01" ~ "2023-10-05",
  date == "2022-11-01" ~ "2023-10-05",
  date == "2022-10-01" ~ "2023-10-05",
  date == "2022-09-01" ~ "2023-10-05",
  date == "2022-08-01" ~ "2023-10-05",
  date == "2022-07-01" ~ "2023-10-05",
  date == "2022-06-01" ~ "2023-10-05",
  date == "2022-05-01" ~ "2023-10-05",
  date == "2022-04-01" ~ "2023-10-05",
  date == "2022-03-01" ~ "2023-10-05",
  date == "2022-02-01" ~ "2023-10-05",
  date == "2022-01-01" ~ "2023-10-05",
  date == "2021-12-01" ~ "2022-11-23",
  date == "2021-11-01" ~ "2022-11-23",
  date == "2021-10-01" ~ "2022-11-23",
  date == "2021-09-01" ~ "2022-11-23",
  date == "2021-08-01" ~ "2022-11-23",
  date == "2021-07-01" ~ "2022-11-23",
  date == "2021-06-01" ~ "2022-11-23",
  date == "2021-05-01" ~ "2022-11-23",
  date == "2021-04-01" ~ "2022-11-23",
  date == "2021-03-01" ~ "2022-11-23",
  date == "2021-02-01" ~ "2022-11-23",
  date == "2021-01-01" ~ "2022-11-23",
  date == "2020-12-01" ~ "2021-11-29",
  date == "2020-11-01" ~ "2021-11-29",
  date == "2020-10-01" ~ "2021-11-29",
  date == "2020-09-01" ~ "2021-11-29",
  date == "2020-08-01" ~ "2021-11-29",
  date == "2020-07-01" ~ "2021-11-29",
  date == "2020-06-01" ~ "2021-11-29",
  date == "2020-05-01" ~ "2021-11-29",
  date == "2020-04-01" ~ "2021-11-29",
  date == "2020-03-01" ~ "2021-11-29",
  date == "2020-02-01" ~ "2021-11-29",
  date == "2020-01-01" ~ "2021-11-29",
  date == "2019-12-01" ~ "2020-10-29",
  date == "2019-11-01" ~ "2020-10-29",
  date == "2019-10-01" ~ "2020-10-29",
  date == "2019-09-01" ~ "2020-10-29",
  date == "2019-08-01" ~ "2020-10-29",
  date == "2019-07-01" ~ "2020-10-29",
  date == "2019-06-01" ~ "2020-10-29",
  date == "2019-05-01" ~ "2020-10-29",
  date == "2019-04-01" ~ "2020-10-29",
  date == "2019-03-01" ~ "2020-10-29",
  date == "2019-02-01" ~ "2020-10-29",
  date == "2019-01-01" ~ "2020-10-29",
  date == "2018-12-01" ~ "2019-10-29",
  date == "2018-11-01" ~ "2019-10-29",
  date == "2018-10-01" ~ "2019-10-29",
  date == "2018-09-01" ~ "2019-10-29",
  date == "2018-08-01" ~ "2019-10-29",
  date == "2018-07-01" ~ "2019-10-29",
  date == "2018-06-01" ~ "2019-10-29",
  date == "2018-05-01" ~ "2019-10-29",
  date == "2018-04-01" ~ "2019-10-29",
  date == "2018-03-01" ~ "2019-10-29",
  date == "2018-02-01" ~ "2019-10-29",
  date == "2018-01-01" ~ "2019-10-29",
  date == "2017-12-01" ~ "2018-10-29",
  date == "2017-11-01" ~ "2018-10-29",
  date == "2017-10-01" ~ "2018-10-29",
  date == "2017-09-01" ~ "2018-10-29",
  date == "2017-08-01" ~ "2018-10-29",
  date == "2017-07-01" ~ "2018-10-29",
  date == "2017-06-01" ~ "2018-10-29",
  date == "2017-05-01" ~ "2018-10-29",
  date == "2017-04-01" ~ "2018-10-29",
  date == "2017-03-01" ~ "2018-10-29",
  date == "2017-02-01" ~ "2018-10-29",
  date == "2017-01-01" ~ "2018-10-29",
  date == "2016-12-01" ~ "2017-12-06",
  date == "2016-11-01" ~ "2017-12-06",
  date == "2016-10-01" ~ "2017-12-06",
  date == "2016-09-01" ~ "2017-12-06",
  date == "2016-08-01" ~ "2017-12-06",
  date == "2016-07-01" ~ "2017-12-06",
  date == "2016-06-01" ~ "2017-12-06",
  date == "2016-05-01" ~ "2017-12-06",
  date == "2016-04-01" ~ "2017-12-06",
  date == "2016-03-01" ~ "2017-12-06",
  date == "2016-02-01" ~ "2017-12-06",
  date == "2016-01-01" ~ "2017-12-06",
  date == "2015-12-01" ~ "2016-12-14",
  date == "2015-11-01" ~ "2016-12-14",
  date == "2015-10-01" ~ "2016-12-14",
  date == "2015-09-01" ~ "2016-12-14",
  date == "2015-08-01" ~ "2016-12-14",
  date == "2015-07-01" ~ "2016-12-14",
  date == "2015-06-01" ~ "2016-12-14",
  date == "2015-05-01" ~ "2016-12-14",
  date == "2015-04-01" ~ "2016-12-14",
  date == "2015-03-01" ~ "2016-12-14",
  date == "2015-02-01" ~ "2016-12-14",
  date == "2015-01-01" ~ "2016-12-14",
))

electricity$date_updated <- as.Date(electricity$date_updated, format = "%Y-%m-%d")

electricity <- electricity %>%
  arrange(location, date) %>% 
  group_by(location)

electricity <- electricity[order(electricity$location != "United States"), ]

#now add most recent value (turn off until april data is added)
#last_month <- sort(unique(electricity$date), decreasing = TRUE)[2]

#electricity_new <- electricity %>% filter(date > last_month) %>% mutate(date_updated = Sys.Date())

#electricity <- rbind(electricity_new,electricity)

write_csv(electricity, "utilities/electricity_data.csv")
write_json(electricity, "utilities/electricity_data.json", pretty = TRUE)

# Clean up temp files
file.remove(c("eia_electricity_1.json", "eia_electricity_2.json", "eia_customers_and_sales_1.json", "eia_customers_and_sales_2.json"))

#download.file("https://www.eia.gov/electricity/sales_revenue_price/xls/table_5A.xlsx", "avg_use.xlsx")

#avg_use <- read_excel("avg_use.xlsx", sheet = 1, skip =2) %>% clean_names()

