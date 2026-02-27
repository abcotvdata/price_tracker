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
library(x13binary)
library(seasonal)

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
inflation <- read_csv("https://raw.githubusercontent.com/abcotvdata/price_tracker/refs/heads/main/inflation/inflation_adjustment.csv")

electricity <- left_join(electricity, inflation, by = "date")

electricity <- electricity %>% 
  mutate(inflation_adjustment = coalesce(inflation_adjustment, 1))

electricity <- electricity %>% mutate(value_inflation_adjusted = round(value*inflation_adjustment,2))

#add in location column 
locations <- read_csv("https://raw.githubusercontent.com/abcotvdata/price_tracker/refs/heads/main/housing/housing_data.csv") %>% select(2,3)
locations <- locations[! duplicated(locations), ]
electricity <- left_join(locations, electricity, by = "state_abbreviation")

#now add most recent value
electricity <- electricity %>% mutate(date_updated = Sys.Date())

#get rid of duplicates in data and places where date updated = NA
electricity <- electricity[! duplicated(electricity), ]

checkX13()

#build a monthly sequence once shared between all cities
mseq <- seq(from = min(electricity$date), to = max(electricity$date), by = "month")

run_x13_one_city <- function(d) {
  d <- d %>%
    group_by(date) %>%
    summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(date) %>%
    right_join(tibble(date = mseq), by = "date") %>%
    arrange(date)
  
  #handle missing values by returning NA
  if (any(!is.finite(d$value) | is.na(d$value))) {
    return(d %>% mutate(sa_value = NA_real_, x13_ok = FALSE, x13_msg = "Missing/invalid values"))
  }

  x_ts <- ts(d$value, start = c(year(min(d$date)), month(min(d$date))), frequency = 12)  
  
  stopifnot(is.ts(x_ts))
  #print(frequency(x_ts))
  #print(start(x_ts))
  #print(end(x_ts))
  
  fit <- tryCatch(
    seas(x_ts, regression.aictest = c("td","easter"), outlier = ""),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(d %>% mutate(sa_value = NA_real_, x13_ok = FALSE, x13_msg = conditionMessage(fit)))
  }
  
  d %>% mutate(
    sa_value = as.numeric(final(fit)),
    x13_ok = TRUE,
    x13_msg = NA_character_
  )
  
}

electricity1 <- electricity %>%
  select(location, date, value) %>%
  group_by(location) %>%
  group_modify(~ run_x13_one_city(.x)) %>%
  ungroup()


#repeat for inflation-adjusted value

run_x13_inflation_city <- function(d) {
  d <- d %>%
    group_by(date) %>%
    summarize(value = mean(value_inflation_adjusted, na.rm = TRUE), .groups = "drop") %>%
    arrange(date) %>%
    right_join(tibble(date = mseq), by = "date") %>%
    arrange(date)
  
  #handle missing values by returning NA
  if (any(!is.finite(d$value) | is.na(d$value))) {
    return(d %>% mutate(sa_value = NA_real_, x13_ok = FALSE, x13_msg = "Missing/invalid values"))
  }
  
  x_ts <- ts(d$value, start = c(year(min(d$date)), month(min(d$date))), frequency = 12)  
  
  stopifnot(is.ts(x_ts))
  #print(frequency(x_ts))
  #print(start(x_ts))
  #print(end(x_ts))
  
  fit <- tryCatch(
    seas(x_ts, regression.aictest = c("td","easter"), outlier = ""),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(d %>% mutate(sa_value = NA_real_, x13_ok = FALSE, x13_msg = conditionMessage(fit)))
  }
  
  d %>% mutate(
    sa_value_inflation_adjusted = as.numeric(final(fit)),
    x13_ok = TRUE,
    x13_msg = NA_character_
  )
  
}

electricity2 <- electricity %>%
  select(location, date, value_inflation_adjusted) %>%
  group_by(location) %>%
  group_modify(~ run_x13_inflation_city(.x)) %>%
  ungroup()

#combine two

electricity1 <- electricity1 %>% select(1,2,3,4) %>% rename(sa_value_raw = sa_value)
electricity2 <- electricity2 %>% select(1,2,3,4) %>% rename(value_inflation_adjusted = value)

electricity3 <- left_join(electricity1, electricity2, by = c("location", "date"))

electricity4 <- left_join(electricity, electricity3, by = c("location","date","value","value_inflation_adjusted")) 

electricity4 <- electricity4 %>% relocate(date_updated, .after = sa_value_inflation_adjusted)

electricity4 <- electricity4 %>%
  arrange(location, date) %>% 
  group_by(location)

electricity4 <-electricity4[order(electricity4$location != "United States"), ]

lubridate:: day(electricity4$date) <- 5

# filter for last 10 years (plus three months, since the data lags by that amount)
electricity <- electricity %>% 
  filter(floor_date(as.Date(date), "month") >= floor_date(Sys.Date() - months(3) - years(10), "month"))

write_csv(electricity4, "utilities/electricity_data.csv")
write_json(electricity4, "utilities/electricity_data.json", pretty = TRUE)

# Clean up temp files
file.remove(c("eia_electricity_1.json", "eia_electricity_2.json", "eia_customers_and_sales_1.json", "eia_customers_and_sales_2.json"))

#download.file("https://www.eia.gov/electricity/sales_revenue_price/xls/table_5A.xlsx", "avg_use.xlsx")

#avg_use <- read_excel("avg_use.xlsx", sheet = 1, skip =2) %>% clean_names()

