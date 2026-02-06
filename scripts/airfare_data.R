library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(httr)
library(jsonlite)
library(r2r)
library(rlang)
library(stringr)
library(readr)
options(timeout=300)

#pull dataset via API call
url <- "https://data.transportation.gov/resource/tfrh-tu9e.json?$limit=500000"

app_token <- Sys.getenv("US_DOT_KEY")

response <- GET(url, add_headers(`X-App-Token` = app_token))

full_data <- fromJSON(content(response, as="text", encoding = "UTF-8"))

#remove extraneous columns

data <- full_data %>% select(2:20)


#clean up for memory, can comment this out later if needed
rm(response, full_data)


#remove data before 2015

data <- data %>% filter(year >= "2015")

#change all number columns to numeric

data$nsmiles <- as.numeric(data$nsmiles)
data$passengers <- as.numeric(data$passengers)
data$fare <- as.numeric(data$fare)
data$large_ms <- as.numeric(data$large_ms)
data$fare_lg <- as.numeric(data$fare_lg)
data$lf_ms <- as.numeric(data$lf_ms)
data$fare_low <- as.numeric(data$fare_low)


#change year/quarter to date value. Using the last month in the quarter as a reference, but we can change this if we decide to use the first instead
#this is mostly to make the inflation adjustment easier. We can switch it back to quarters later if we want.


data1 <- data %>% mutate(date = case_when(
  quarter == "1" ~ paste(year,"-03-01", sep = ""),
  quarter == "2" ~ paste(year,"-06-01", sep = ""),
  quarter == "3" ~ paste(year, "-09-01", sep = ""),
  quarter == "4" ~ paste(year, "-12-01", sep = "")
))

data1$date <- as.Date(data1$date, format = "%Y-%m-%d")

data1 <- data1 %>% relocate(date, .before = year)

#add in inflation adjustment  

inflation <- read_csv("https://raw.githubusercontent.com/abcotvdata/price_tracker/refs/heads/main/inflation/inflation_adjustment.csv") %>% clean_names()

data2 <- left_join(data1, inflation, by = "date")

#calculate adjusted values

data2 <- data2 %>% rename(avg_fare_raw = fare, fare_lg_raw = fare_lg, fare_low_raw = fare_low) %>%
  mutate(avg_fare_adjusted = round(avg_fare_raw*inflation_adjustment,2), fare_lg_adjusted = round(fare_lg_raw*inflation_adjustment,2), fare_low_adjusted = round(fare_low_raw*inflation_adjustment,2)) %>%
  relocate(avg_fare_adjusted, .after = avg_fare_raw) %>% relocate(fare_lg_adjusted, .after = fare_lg_raw) %>% relocate(fare_low_adjusted, .after = fare_low_raw)


#add column for latest value and oldest value

latest_data <- data2 %>%
  group_by(airport_1,airport_2) %>%
  filter(date == max(date)) %>%
  select(1,2,3,4,5,6,7,8,9,10,11,24,14,15,16,17,18,19,20,21,22,23) %>%
  rename(
    date_latest = date,
    year_latest = year,
    quarter_latest = quarter,
    fare_latest_raw = avg_fare_raw,
    fare_latest_adjusted = avg_fare_adjusted,
    carrier_lg_latest = carrier_lg,
    large_ms_latest = large_ms,
    fare_lg_latest_raw = fare_lg_raw,
    fare_lg_latest_adjusted = fare_lg_adjusted,
    carrier_low_latest = carrier_low,
    lf_ms_latest = lf_ms,
    fare_low_latest_raw = fare_low_raw,
    fare_low_latest_adjusted = fare_low_adjusted
  )
oldest_data <- data2 %>%
  group_by(airport_1, airport_2) %>%
  filter(date == min(date)) %>%
  select(1,2,3,4,5,6,7,8,9,10,11,24,14,15,16,17,18,19,20,21,22,23) %>%
  rename(
    date_oldest = date,
    year_oldest = year,
    quarter_oldest = quarter,
    fare_oldest_raw = avg_fare_raw,
    fare_oldest_adjusted = avg_fare_adjusted,
    carrier_lg_oldest = carrier_lg,
    large_ms_oldest = large_ms,
    fare_lg_oldest_raw = fare_lg_raw,
    fare_lg_oldest_adjusted = fare_lg_adjusted,
    carrier_low_oldest = carrier_low,
    lf_ms_oldest = lf_ms,
    fare_low_oldest_raw = fare_low_raw,
    fare_low_oldest_adjusted = fare_low_adjusted
  )
#combine oldest and latest data
oldest_latest_data <- left_join(latest_data, oldest_data, by = c("citymarketid_1","citymarketid_2", "city1", "city2", "airportid_1", "airportid_2", "airport_1", "airport_2"))

#combine oldest and latest data with full dataset and calculate inflation for quarterly values
data3 <- data2 %>%
  group_by(airport_1,airport_2) %>%
  arrange(airport_1,airport_2, date) %>%
  left_join(oldest_latest_data, by = c("citymarketid_1","citymarketid_2", "city1", "city2", "airportid_1", "airportid_2", "airport_1", "airport_2"))


#calculate percent change oldest to newest
#We can change this to 5/10 years if we want, I'm just following what we did for food.
data4 <- data3 %>% mutate(p_change_avg_fare_oldest_newest_raw = round(((fare_latest_raw-fare_oldest_raw)/fare_oldest_raw)*100,1)) %>%
  mutate(p_change_avg_fare_oldest_newest_adjusted = round(((fare_latest_adjusted-fare_oldest_adjusted)/fare_oldest_adjusted)*100,1)) %>%
  mutate(p_change_lg_fare_oldest_newest_raw = round(((fare_lg_latest_raw-fare_lg_oldest_raw)/fare_lg_oldest_raw)*100,1)) %>%
  mutate(p_change_lg_fare_oldest_newest_adjusted = round(((fare_lg_latest_adjusted-fare_lg_oldest_adjusted)/fare_lg_oldest_adjusted)*100,1)) %>%
  mutate(p_change_low_fare_oldest_newest_raw = round(((fare_low_latest_raw-fare_low_oldest_raw)/fare_low_oldest_raw)*100,1)) %>%
  mutate(p_change_low_fare_oldest_newest_adjusted = round(((fare_low_latest_adjusted-fare_low_oldest_adjusted)/fare_low_oldest_adjusted)*100,1))



#add in airport names

# Use relative path for airport_names.csv
airports <- read_csv("https://raw.githubusercontent.com/abcotvdata/price_tracker/refs/heads/main/transportation/airport_names.csv") %>% clean_names() %>% select(1,2) %>% distinct(airport, .keep_all = TRUE)

data5 <- data4 %>% left_join(
  airports %>% rename(airport_1 = airport, origin_airport_name = display_airport_name),
  by = "airport_1"
) %>% left_join(
  airports %>% rename(airport_2 = airport, destination_airport_name = display_airport_name),
  by = "airport_2"
) %>% relocate(origin_airport_name, .after = airport_2) %>% relocate(destination_airport_name, .after = origin_airport_name)


#remove towers with fewer than 10 average passengers

data6 <- data5 %>% filter(passengers > 10)

#Select only specified columns, this is a test json file that will only output location/city/airport for the test search. Remember to run the "output" line before exporting
#output_path <- file.path(output_dir, "airfare_locations.json")

#Create 2 jsons with correct airport and location structure, remove duplicates in data6, so there's only 1 json with a unique aiport entry for origins and 1 unique for destination
state_mapping <- data.frame(
  state_abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                 "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                 "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                 "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                 "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"),
  state_full = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                 "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
                 "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
                 "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                 "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
                 "New Hampshire", "New Jersey", "New Mexico", "New York",
                 "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
                 "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
                 "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
                 "West Virginia", "Wisconsin", "Wyoming", "District of Columbia")
)

# Create origin airports JSON (keeping original column names)
origin_json <- data6 %>%
  # Keep original column names
  select(city1, airport_1, origin_airport_name) %>%
  # Remove rows with missing values
  filter(!is.na(city1), !is.na(airport_1), !is.na(origin_airport_name)) %>%
  # Clean airport names - ensure airport code is at beginning
  mutate(
    origin_airport_name = trimws(origin_airport_name),
    origin_airport_name = ifelse(
      str_detect(origin_airport_name, paste0("^", airport_1, "\\s")) | 
        str_detect(origin_airport_name, paste0("^", airport_1, "$")),
      origin_airport_name,
      paste(origin_airport_name)
    )
  ) %>%
  # Get UNIQUE airport codes per city - remove duplicates
  distinct(city1, airport_1, .keep_all = TRUE) %>%
  # Extract state abbreviation from city name
  mutate(
    state_abbr = str_extract(city1, ", [A-Z]{2}") %>% 
      str_remove(", ") %>% 
      trimws()
  ) %>%
  # Add full state name
  left_join(state_mapping, by = "state_abbr") %>%
  # Group by city and create airports list structure
  group_by(city1, state_abbr, state_full) %>%
  summarize(
    airports = list(
      # Create unique airport objects
      unique(
        lapply(1:n(), function(i) {
          list(
            airport_1 = airport_1[i],
            origin_airport_name = origin_airport_name[i]
          )
        })
      )
    ),
    .groups = 'drop'
  ) %>%
  # Remove any empty airport lists
  filter(lengths(airports) > 0)

# Create destination airports JSON (keeping original column names)
destination_json <- data6 %>%
  # Keep original column names
  select(city2, airport_2, destination_airport_name) %>%
  # Remove rows with missing values
  filter(!is.na(city2), !is.na(airport_2), !is.na(destination_airport_name)) %>%
  # Clean airport names
  mutate(
    destination_airport_name = trimws(destination_airport_name),
    destination_airport_name = ifelse(
      str_detect(destination_airport_name, paste0("^", airport_2, "\\s")) | 
        str_detect(destination_airport_name, paste0("^", airport_2, "$")),
      destination_airport_name,
      paste(destination_airport_name)
    )
  ) %>%
  # Get UNIQUE airport codes per city
  distinct(city2, airport_2, .keep_all = TRUE) %>%
  # Extract state abbreviation
  mutate(
    state_abbr = str_extract(city2, ", [A-Z]{2}") %>% 
      str_remove(", ") %>% 
      trimws()
  ) %>%
  # Add full state name
  left_join(state_mapping, by = "state_abbr") %>%
  # Group by city and create airports list
  group_by(city2, state_abbr, state_full) %>%
  summarize(
    airports = list(
      unique(
        lapply(1:n(), function(i) {
          list(
            airport_2 = airport_2[i],
            destination_airport_name= destination_airport_name[i]
          )
        })
      )
    ),
    .groups = 'drop'
  ) %>%
  # Remove any empty airport lists
  filter(lengths(airports) > 0)

# Check for duplicates
cat("Checking origin JSON for duplicate airports within cities:\n")
origin_json$has_duplicates <- sapply(origin_json$airports, function(airports_list) {
  codes <- sapply(airports_list, function(x) x$airport_code)
  any(duplicated(codes))
})

if (any(origin_json$has_duplicates)) {
  cat("WARNING: Found duplicates in these cities:\n")
  print(origin_json %>% filter(has_duplicates) %>% select(city1))
} else {
  cat("✓ Origin JSON has no duplicate airports within cities\n")
}

cat("\nChecking destination JSON for duplicate airports within cities:\n")
destination_json$has_duplicates <- sapply(destination_json$airports, function(airports_list) {
  codes <- sapply(airports_list, function(x) x$airport_code)
  any(duplicated(codes))
})

if (any(destination_json$has_duplicates)) {
  cat("WARNING: Found duplicates in these cities:\n")
  print(destination_json %>% filter(has_duplicates) %>% select(city2))
} else {
  cat("✓ Destination JSON has no duplicate airports within cities\n")
}

# Remove duplicate check columns before writing
origin_json <- origin_json %>% select(-has_duplicates)
destination_json <- destination_json %>% select(-has_duplicates)

# Write JSON files
write_json(origin_json, "transportation/origin_airports.json", pretty = TRUE, auto_unbox = TRUE)
write_json(destination_json, "transportation/destination_airports.json", pretty = TRUE, auto_unbox = TRUE)


airport_city_search <- data6 %>%
  select(
    "city1",
    "city2",
    "airport_1",
    "airport_2",
    "origin_airport_name",
    "destination_airport_name",
  )

#export final_data for trimmed down columns. If you want to export ALL columns,export data6.
#write_json(airport_city_search, output_path, pretty = TRUE, na = "null")
#write_csv(airport_city_search, file.path(output_dir, "airfare_location.csv"))

#Select only specified columns
final_data <- data6 %>%
  select(
    "year",
    "quarter",
    "city1",
    "city2",
    "airport_1",
    "airport_2",
    "origin_airport_name",
    "destination_airport_name",
    "avg_fare_raw",
    "avg_fare_adjusted",
    "year_latest",
    "quarter_latest",
    "fare_latest_raw",
    "fare_latest_adjusted",
    "date_oldest",
    "year_oldest",
    "quarter_oldest",
    "fare_oldest_raw",
    "fare_oldest_adjusted",
    "p_change_avg_fare_oldest_newest_raw",
    "p_change_avg_fare_oldest_newest_adjusted"
  )

#export final_data for trimmed down columns. If you want to export ALL columns,export data6.
write_json(final_data, "transportation/airfare_data.json", pretty = TRUE, na = "null")
write_csv(final_data, "transportation/airfare_data.csv", row.names = FALSE)

