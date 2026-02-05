options(vsc.no_debuggefrdhdfddhjtgnr = TRUE)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(data.table)
library(zoo)

options(timeout=300)

#The "Zillow Home Value Index" (ZHVI) is a smoothed, seasonally adjusted figure that represents home prices for the 35th-65th percentile of single-family homes and condos/co-ops in a given geographic area.

#import home value data by metro area

download.file("https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv", "zhvi_metro.csv")

zhvi_metro <- read_csv("zhvi_metro.csv")

#fix column names. All column names are dates, fix so they are displayed as Jan 31, 2025, not 2025-01-31

indices <- 6:ncol(zhvi_metro)
date_names <- as.Date(colnames(zhvi_metro)[indices], format = "%Y-%m-%d")
colnames(zhvi_metro)[indices] <- format(date_names, "%b-%d-%Y")

zhvi_metro <- zhvi_metro %>% clean_names() %>% select(-1,-2,-4)

#import rent data by metro area. ZORI is the Zillow Observed Rent Index.

download.file("https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_uc_sfrcondomfr_sm_month.csv", "zori_metro.csv")

zori_metro <- read_csv("zori_metro.csv")

#fix column names. All column names are dates, fix so they are displayed as Jan 31, 2025, not 2025-01-31

indices <- 6:ncol(zori_metro)
date_names <- as.Date(colnames(zori_metro)[indices], format = "%Y-%m-%d")
colnames(zori_metro)[indices] <- format(date_names, "%b-%d-%Y")

zori_metro <- zori_metro %>% clean_names() %>% select(-1,-2,-4)

#filter zhvi to last 10 years
zhvi_metro <- zhvi_metro %>% select(1,2,183:ncol(zhvi_metro))

#import in MSA population file

msa <- read_csv("https://raw.githubusercontent.com/abcotvdata/price_tracker/refs/heads/main/metros/metro_areas_US_top_100.csv")

#clean names of metro areas to match zillow file
msa$name <- gsub("Metro Area","", msa$name)
msa$name <- gsub("Micro Area", "", msa$name)

# Step 1: remove everything from the first “-” up to the comma
tmp <- sub("^([^-]+)-.*(,.*)", "\\1\\2", msa$name)

# Step 2: keep only the comma plus the next three characters (comma + space + 2-letter state)
msa$name_clean <- sub("(,.{3}).*", "\\1", tmp)

#rename column to match zhvi/zori file

msa <- msa %>% rename(region_name = name_clean, msa_full_name = name)

#fix anomalies -- Poughkeepsie, NY, Louisville KY

msa$region_name <- gsub("Kiryas Joel, NY", "Poughkeepsie, NY", msa$region_name)
msa$region_name <- gsub("Louisville/Jefferson County, KY", "Louisville, KY", msa$region_name)

#combine files

zhvi_msa <- left_join(msa, zhvi_metro, by = "region_name")

zori_msa <- left_join(msa, zori_metro, by = "region_name")

#make the files long instead of wide

zhvi_long <- zhvi_msa %>% select(1,2,6:ncol(zhvi_msa))

zhvi_long <- zhvi_long %>%
  pivot_longer(
    cols = 3:ncol(zhvi_long),
    names_to = "month",
    values_to = "value"
  )

zhvi_long <- zhvi_long %>% mutate(category = "Home Sales")

zori_long <- zori_msa %>% select(1,2,6:ncol(zori_msa))

zori_long <- zori_long %>%
  pivot_longer(
    cols = 3:ncol(zori_long),
    names_to = "month",
    values_to = "value"
  )

zori_long <- zori_long %>% mutate(category = "Rental Prices")

#combine into one dataset that will become the final


data <- rbind(zhvi_long, zori_long)

#change names to match other data

data <- data %>% relocate(category, .before = month) %>% rename(date = month, location = msa_full_name)

data$date <- as.Date(data$date, format = "%b_%d_%Y")

#create state columns

data <- data %>% mutate(state_spelled_out = case_when(
  grepl(", AL", location) ~ "Alabama",
  grepl(", AK", location) ~ "Alaska",
  grepl(", AZ", location) ~ "Arizona",
  grepl(", AR", location) ~ "Arkansas",
  grepl(", CA", location) ~ "California",
  grepl(", CO", location) ~ "Colorado",
  grepl(", CT", location) ~ "Connecticut",
  grepl(", DE", location) ~ "Delaware",
  grepl(", FL", location) ~ "Florida",
  grepl(", GA", location) ~ "Georgia",
  grepl(", HI", location) ~ "Hawaii",
  grepl(", ID", location) ~ "Idaho",
  grepl(", IL", location) ~ "Illinois",
  grepl(", IN", location) ~ "Indiana",
  grepl(", IA", location) ~ "Iowa",
  grepl(", KS", location) ~ "Kansas",
  grepl(", KY", location) ~ "Kentucky",
  grepl(", LA", location) ~ "Louisiana",
  grepl(", ME", location) ~ "Maine",
  grepl(", MD", location) ~ "Maryland",
  grepl(", MA", location) ~ "Massachusetts",
  grepl(", MI", location) ~ "Michigan",
  grepl(", MN", location) ~ "Minnesota",
  grepl(", MS", location) ~ "Mississippi",
  grepl(", MO", location) ~ "Missouri",
  grepl(", MT", location) ~ "Montana",
  grepl(", NE", location) ~ "Nebraska",
  grepl(", NV", location) ~ "Nevada",
  grepl(", NH", location) ~ "New Hampshire",
  grepl(", NJ", location) ~ "New Jersey",
  grepl(", NM", location) ~ "New Mexico",
  grepl(", NY", location) ~ "New York",
  grepl(", NC", location) ~ "North Carolina",
  grepl(", ND", location) ~ "North Dakota",
  grepl(", OH", location) ~ "Ohio",
  grepl(", OK", location) ~ "Oklahoma",
  grepl(", OR", location) ~ "Oregon",
  grepl(", PA", location) ~ "Pennsylvania",
  grepl(", RI", location) ~ "Rhode Island",
  grepl(", SC", location) ~ "South Carolina",
  grepl(", SD", location) ~ "South Dakota",
  grepl(", TN", location) ~ "Tennessee",
  grepl(", TX", location) ~ "Texas",
  grepl(", UT", location) ~ "Utah",
  grepl(", VT", location) ~ "Vermont",
  grepl(", VA", location) ~ "Virginia",
  grepl(", WA", location) ~ "Washington",
  grepl(", WV", location) ~ "West Virginia",
  grepl(", WI", location) ~ "Wisconsin",
  grepl(", WY", location) ~ "Wyoming",
  grepl(", DC", location) ~ "Washington, D.C.",
  location == "United States" ~ "United States"
)) %>% mutate(state_abbreviation = case_when(
  grepl(", AL", location) ~ "AL",
  grepl(", AK", location) ~ "AK",
  grepl(", AZ", location) ~ "AZ",
  grepl(", AR", location) ~ "AR",
  grepl(", CA", location) ~ "CA",
  grepl(", CO", location) ~ "CO",
  grepl(", CT", location) ~ "CT",
  grepl(", DE", location) ~ "DE",
  grepl(", FL", location) ~ "FL",
  grepl(", GA", location) ~ "GA",
  grepl(", HI", location) ~ "HI",
  grepl(", ID", location) ~ "ID",
  grepl(", IL", location) ~ "IL",
  grepl(", IN", location) ~ "IN",
  grepl(", IA", location) ~ "IA",
  grepl(", KS", location) ~ "KS",
  grepl(", KY", location) ~ "KY",
  grepl(", LA", location) ~ "LA",
  grepl(", ME", location) ~ "ME",
  grepl(", MD", location) ~ "MD",
  grepl(", MA", location) ~ "MA",
  grepl(", MI", location) ~ "MI",
  grepl(", MN", location) ~ "MN",
  grepl(", MS", location) ~ "MS",
  grepl(", MO", location) ~ "MO",
  grepl(", MT", location) ~ "MT",
  grepl(", NE", location) ~ "NE",
  grepl(", NV", location) ~ "NV",
  grepl(", NH", location) ~ "NH",
  grepl(", NJ", location) ~ "NJ",
  grepl(", NM", location) ~ "NM",
  grepl(", NY", location) ~ "NY",
  grepl(", NC", location) ~ "NC",
  grepl(", ND", location) ~ "ND",
  grepl(", OH", location) ~ "OH",
  grepl(", OK", location) ~ "OK",
  grepl(", OR", location) ~ "OR",
  grepl(", PA", location) ~ "PA",
  grepl(", RI", location) ~ "RI",
  grepl(", SC", location) ~ "SC",
  grepl(", SD", location) ~ "SD",
  grepl(", TN", location) ~ "TN",
  grepl(", TX", location) ~ "TX",
  grepl(", UT", location) ~ "UT",
  grepl(", VT", location) ~ "VT",
  grepl(", VA", location) ~ "VA",
  grepl(", WA", location) ~ "WA",
  grepl(", WV", location) ~ "WV",
  grepl(", WI", location) ~ "WI",
  grepl(", WY", location) ~ "WY",
  grepl(", DC", location) ~ "DC",
  location == "United States" ~ "US"
)) %>% relocate(state_abbreviation, .after = location) %>% relocate(state_spelled_out, .after = state_abbreviation)

#string split

data <- data %>% 
  tidyr::separate(location, into = c("location", "delete"), sep = ", ") %>% 
  select(-delete)


#create inflation adjustment column
#prices adjusted for April 2025
inflation_small <- read_csv("https://raw.githubusercontent.com/abcotvdata/price_tracker/refs/heads/main/inflation/inflation_adjustment.csv") %>% rename(month = date)

data$month <- floor_date(data$date, "month")

data <- left_join(data,inflation_small, by = "month")

data <- data %>% 
  mutate(inflation_adjustment = coalesce(inflation_adjustment, 1))

data <- data %>% select(-8) %>% mutate(value_inflation_adjusted = value*inflation_adjustment)

data$year <- as.numeric(format(data$date, "%Y"))
data$month <- as.numeric(format(data$date, "%m"))

data <- data %>% mutate(date_updated = case_when(
  month <= 11 ~ paste(year,month+1,16,sep="-"),
  month == 12 ~ paste(year+1,01,16,sep="-")
))
data$date_updated <- as.Date(data$date_updated, format = "%Y-%m-%d")

data <- data %>% select(-10,-11)

east_coast <- c("ME", "VT","NH","MA","CT","RI","NY","PA","NJ","MD","DE","WV","DC","VA","NC","SC","GA","FL")
west_coast <- c("WA","OR","CA","NV","AZ", "AK","HI")
rocky_mountain <- c("ID","MT","WY","UT","CO")
gulf_coast <- c("NM","TX","AR","LA","MS","AL")
midwest <- c("ND","SD","NE","KS","OK","MN","IA","MO","WI","IL","MI","IN","OH","KY","TN")

data <- data %>% mutate(region = case_when(
  state_abbreviation %in% east_coast ~ "East Coast",
  state_abbreviation %in% west_coast ~ "West Coast",
  state_abbreviation %in% rocky_mountain ~ "Rocky Mountain",
  state_abbreviation %in% gulf_coast ~ "Gulf Coast",
  state_abbreviation %in% midwest ~ "Midwest",
  location == "United States" ~ "United States"
)) %>% relocate(region, .before = category)

data$value<-as.character(data$value)

  data <- data %>% 
  mutate(value = replace_na(value, ""))
  
  data$value<-as.numeric(data$value)
  
  data$date_updated <- as.Date(data$date_updated, format = "%b_%d_%Y")

#dir.create("../my-app/src/lib/data", recursive = TRUE, showWarnings = FALSE)
write.csv(data, "housing/housing_data.csv", row.names = FALSE)
write_json(data, "housing/housing_data.json", pretty = TRUE)

# Clean up temp files
file.remove(c("zhvi_metro.csv", "zori_metro.csv", "census_metros.csv"))
