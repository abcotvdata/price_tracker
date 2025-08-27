library(blscrapeR)
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(blsR)
library(jsonlite)
library(r2r)
library(rlang)
library(usethis)
library(tidycensus)
options(timeout=300)

Sys.getenv("BLS_KEY")
Sys.getenv("CENSUS_KEY")

us <- c("APU0000712311", "APU0000FL2101", "APU0000712112", "APU0000714233", "APU0000711415", "APU0000711211", "APU0000711311", "APU0000711412", "APU0000710212",
            "APU0000709112", "APU0000FJ4101", "APU0000708111", "APU0000FD2101", "APU0000FC1101", "APU0000706111", "APU0000704111", "APU0000701111", "APU0000702111", 
            "APU0000701322", "APU0000701312", "APU0000718311", "APU0000702421", "APU0000710411", "APU0000715211", "APU0000FN1101")
us2 <- c("APU0000720311", "APU0000720111","APU0000FF1101", "APU0000FN1102", "APU0000FJ1101","CUUR0000SAF11")
west <- c("APU0400712311", "APU0400FL2101", "APU0400712112", "APU0400714233", "APU0400711415", "APU0400711211", "APU0400711311", "APU0400711412", "APU0400710212", 
            "APU0400709112", "APU0400FJ4101", "APU0400708111", "APU0400FD2101", "APU0400FC1101", "APU0400706111", "APU0400704111", "APU0400701111", "APU0400702111", 
            "APU0400701322", "APU0400701312", "APU0400718311", "APU0400702421", "APU0400710411", "APU0400715211", "APU0400FN1101")
west2 <- c("APU0400720311", "APU0400720111","APU0400FF1101","APU0400FN1102","APU0400FJ1101","CUUR0400SAF11")
south <- c("APU0300712311", "APU0300FL2101", "APU0300712112", "APU0300714233", "APU0300711415", "APU0300711211", "APU0300711311", "APU0300711412", "APU0300710212", 
            "APU0300709112", "APU0300FJ4101", "APU0300708111", "APU0300FD2101", "APU0300FC1101", "APU0300706111", "APU0300704111", "APU0300701111", "APU0300702111", 
            "APU0300701322", "APU0300701312", "APU0300718311", "APU0300702421", "APU0300710411", "APU0300715211","APU0300FN1101")
south2 <- c("APU0300720311", "APU0300720111","APU0300FF1101","APU0300FN1102","APU0300FJ1101","CUUR0300SAF11")
midwest <- c("APU0200712311", "APU0200FL2101", "APU0200712112", "APU0200714233", "APU0200711415", "APU0200711211", "APU0200711311", "APU0200711412", "APU0200710212",
            "APU0200709112", "APU0200FJ4101", "APU0200708111", "APU0200FD2101", "APU0200FC1101", "APU0200706111", "APU0200704111", "APU0200701111", "APU0200702111",
            "APU0200701322", "APU0200701312", "APU0200718311", "APU0200702421", "APU0200710411", "APU0200715211", "APU0200FN1101")
midwest2 <- c("APU0200720311", "APU0200720111","APU0200FF1101","APU0200FN1102","APU0200FJ1101","CUUR0200SAF11")
northeast <- c("APU0100712311", "APU0100FL2101", "APU0100712112", "APU0100714233", "APU0100711415", "APU0100711211", "APU0100711311", "APU0100711412", "APU0100710212", 
            "APU0100709112", "APU0100FJ4101", "APU0100708111", "APU0100FD2101", "APU0100FC1101", "APU0100706111", "APU0100704111", "APU0100701111", "APU0100702111", 
            "APU0100701322", "APU0100701312", "APU0100718311", "APU0100702421", "APU0100710411", "APU0100715211", "APU0100FN1101")
northeast2 <- c("APU0100720311", "APU0100720111","APU0100FF1101","APU0100FN1102","APU0100FJ1101","CUUR0100SAF11")


#one time download older files
us_food <- bls_api(us, startyear = 2015)
us_food2 <- bls_api(us2, startyear = 2015)

west_food <- bls_api(west, startyear = 2015) 
west_food2 <- bls_api(west2, startyear = 2015)

south_food <- bls_api(south, startyear = 2015)
south_food2 <- bls_api(south2, startyear = 2015)

midwest_food <- bls_api(midwest, startyear = 2015) 
midwest_food2 <- bls_api(midwest2, startyear = 2015) 

northeast_food <- bls_api(northeast, startyear = 2015)
northeast_food2 <- bls_api(northeast2, startyear = 2015)

west_food <- west_food %>% select(-7)
south_food2 <- south_food2 %>% select(-7)
midwest_food <- midwest_food %>% select(-7)
midwest_food2 <- midwest_food2 %>% select(-7)
northeast_food <- northeast_food %>% select(-7)
northeast_food2 <- northeast_food2 %>% select(-7)

us_food3 <- bls_api(us, startyear = 2025)
us_food4 <- bls_api(us2, startyear = 2025)

west_food3 <- bls_api(west, startyear = 2025) 
west_food4 <- bls_api(west2, startyear = 2025)

south_food3 <- bls_api(south, startyear = 2025) 
south_food4 <- bls_api(south2, startyear = 2025)

midwest_food3 <- bls_api(midwest, startyear = 2025)
midwest_food4 <- bls_api(midwest2, startyear = 2025)

northeast_food3 <- bls_api(northeast, startyear = 2025)
northeast_food4 <- bls_api(northeast2, startyear = 2025)

us_food3 <- us_food3 %>% select(-4)
west_food3 <- west_food3 %>% select(-4)
south_food3 <- south_food3 %>% select(-4)
midwest_food3 <- midwest_food3 %>% select(-4)
northeast_food3 <- northeast_food3 %>% select(-4)
us_food4 <- us_food4 %>% select(-4)
west_food4 <- west_food4 %>% select(-4)
south_food4 <- south_food4 %>% select(-4)
midwest_food4 <- midwest_food4 %>% select(-4)
northeast_food4 <- northeast_food4 %>% select(-4)



us_food <- rbind(us_food, us_food2,us_food3,us_food4)
west_food <- rbind(west_food, west_food2,west_food3,west_food4)
south_food <- rbind(south_food, south_food2,south_food3,south_food4)
midwest_food <- rbind(midwest_food, midwest_food2,midwest_food3,midwest_food4)
northeast_food <- rbind(northeast_food, northeast_food2,northeast_food3,northeast_food4)


us_food <- us_food %>% mutate(region = "United States")
west_food <- west_food %>% mutate(region = "West")
south_food <- south_food %>% mutate(region = "South")
midwest_food <- midwest_food %>% mutate(region = "Midwest")
northeast_food <- northeast_food %>% mutate(region = "Northeast")


food <- rbind(us_food, west_food, south_food, midwest_food, northeast_food)

food <- food %>% mutate(category = case_when(
  grepl("00712311", seriesID) ~ "Tomatoes, field grown, per lb.",
  grepl("00FL2101", seriesID) ~ "Lettuce, romaine, per lb.",
  grepl("00712112", seriesID) ~ "Potatoes, white, per lb.",
  grepl("00714233", seriesID) ~ "Beans, dried, any type, all sizes, per lb.",
  grepl("00711415", seriesID) ~ "Strawberries, dry pint, per 12 oz.",
  grepl("00711211", seriesID) ~ "Bananas, per lb.",
  grepl("00711311", seriesID) ~ "Oranges, Navel, per lb.",
  grepl("00711412", seriesID) ~ "Lemons, per lb.",
  grepl("00710212", seriesID) ~ "Cheddar cheese, natural, per lb.",
  grepl("00709112", seriesID) ~ "Milk, fresh, whole, fortified, per gal.",
  grepl("00FJ4101", seriesID) ~ "Yogurt, per 8 oz.",
  grepl("00708111", seriesID) ~ "Eggs, grade A, large, per doz.",
  grepl("00FD2101", seriesID) ~ "All Ham (Excluding Canned Ham and Luncheon Slices), per lb.",
  grepl("00FC1101", seriesID) ~ "All uncooked ground beef, per lb.",
  grepl("00706111", seriesID) ~ "Chicken, fresh, whole, per lb.",
  grepl("00704111", seriesID) ~ "Bacon, sliced, per lb.",
  grepl("00701111", seriesID) ~ "Flour, white, all purpose, per lb.",
  grepl("00702111", seriesID) ~ "Bread, white, pan, per lb.",
  grepl("00701312", seriesID) ~ "Rice, white, long grain, uncooked, per lb.",
  grepl("00718311", seriesID) ~ "Potato chips, per 16 oz.",
  grepl("00701322", seriesID) ~ "Spaghetti and macaroni, per lb.",
  grepl("00702421", seriesID) ~ "Cookies, chocolate chip, per lb.",
  grepl("00710411", seriesID) ~ "Ice cream, prepackaged, bulk, regular, per 1/2 gal.",
  grepl("00715211", seriesID) ~ "Sugar, white, all sizes, per lb.",
  grepl("00FN1101", seriesID) ~ "All soft drinks, per 2 liters",
  grepl("00720311", seriesID) ~ "Wine, red and white table, all sizes, any origin, per 1 liter",
  grepl("00720111", seriesID) ~ "Malt beverages, all types, all sizes, any origin, per 16 oz.",
  grepl("00FF1101", seriesID) ~ "Chicken breast, boneless, per lb.",
  grepl("00FN1102", seriesID) ~ "All soft drinks, 12 pk, 12 oz., cans, per 12 pk.",
  grepl("00FJ1101", seriesID) ~ "Milk, fresh, low-fat, reduced fat, skim, per gal.",
  grepl("00SAF11", seriesID) ~ "All food at home"
))

food$period <- gsub("M", "", food$period)
food$year <- as.character(food$year)


food$date <- paste(food$year,food$period,"01",sep="-")
food$date <- as.Date(food$date, format = "%Y-%m-%d")

food <- food %>% relocate(date, .before = year) %>% relocate(region, .after = date) %>% relocate(category, .after = region) %>% select(-c(4,5,6,8,9))

#adjust soft drink cans value so it's per 12pk instead of per can

softdrinks <- food %>% filter(category == "All soft drinks, 12 pk, 12 oz., cans, per 12 pk.") %>% mutate(value = value*12)
food <- food %>% filter(category != "All soft drinks, 12 pk, 12 oz., cans, per 12 pk.")
food <- rbind(food, softdrinks)


#adjust for inflation
inflation <- read_csv("https://raw.githubusercontent.com/abcotvdata/price_tracker/refs/heads/main/scripts/inflation_adjustment.csv")


food <- left_join(food, inflation, by = "date")

food <- food %>% mutate(value_inflation_adjusted = round(value*inflation_adjustment,2))


#add in location column 
locations <- read_csv("https://raw.githubusercontent.com/abcotvdata/price_tracker/refs/heads/main/housing/housing_data.csv") %>% select(2,3,4)
locations <- locations[! duplicated(locations), ]

west <- c("WA","OR","CA","ID","MT","WY","NV","UT","CO","AZ","NM","AK","HI")
midwest <- c("ND","SD","NE","KS","MN","IA","MO","WI","IL","IN","OH","MI")
south <- c("TX","OK","AR","LA","MS","AL","FL","GA","TN","KY","WV","MD","DC","DE","VA","NC","SC")
northeast <- c("NJ","PA","NY","CT","RI","MA","VT","NH","ME")

locations <- locations %>% mutate(region = case_when(
  state_abbreviation %in% west ~ "West",
  state_abbreviation %in% midwest ~ "Midwest",
  state_abbreviation %in% south ~ "South",
  state_abbreviation %in% northeast ~ "Northeast",
  state_abbreviation == "US" ~ "United States"
))
food <- left_join(locations, food, by = "region")

food <- food %>% select(1,2,3,4,6,5,7,8,9)

#produce <- c("Tomatoes, field grown, per lb.","Lettuce, romaine, per lb.","Potatoes, white, per lb.","Strawberries, dry pint, per 12 oz.","Bananas, per lb.", "Oranges, Navel, per lb.", "Lemons, per lb.")
#dry_goods <- c("Beans, dried, any type, all sizes, per lb.","Flour, white, all purpose, per lb.", "Bread, white, pan, per lb.","Rice, white, long grain, uncooked, per lb.","Spaghetti and macaroni, per lb.","Sugar, white, all sizes, per lb.")
#meat <- c("All Ham (Excluding Canned Ham and Luncheon Slices), per lb.", "All uncooked ground beef, per lb.","Chicken, fresh, whole, per lb.", "Bacon, sliced, per lb.","Chicken breast, boneless, per lb.")
#dairy <- c("Cheddar cheese, natural, per lb.", "Milk, fresh, whole, fortified, per gal.","Yogurt, per 8 oz.", "Eggs, grade A, large, per doz.","Milk, fresh, low-fat, reduced fat, skim, per gal.")
#snacks <- c("Potato chips, per 16 oz.","Cookies, chocolate chip, per lb.","Ice cream, prepackaged, bulk, regular, per 1/2 gal.")
#drinks <- c("All soft drinks, per 2 liters","Wine, red and white table, all sizes, any origin, per 1 liter","Malt beverages, all types, all sizes, any origin, per 16 oz.","All soft drinks, 12 pk, 12 oz., cans, per 12 pk.")


#food <- food %>% mutate(category_bin = case_when(
#category %in% produce ~ "Produce",
#category %in% dry_goods ~ "Dry Goods",
#category %in% meat ~ "Meat and Poultry",
#category %in% dairy ~ "Dairy and Eggs",
#category %in% snacks ~ "Snacks and Desserts",
#category %in% drinks ~ "Drinks"
#)) %>% relocate(category_bin, .after = category)

# get all possible dates available per metro, even if NA

all_metros <- unique(food$location)
all_dates <- unique(food$date)
all_categories <- unique(food$category)

full_grid <- crossing(location = all_metros, date = all_dates, category = all_categories) %>% 
  arrange(location, date)

full_grid$date <- as.Date(full_grid$date, format = "%Y-%m-%d")

#fix error where it's not including the state or region when aligning full_grid and food
full_grid <- left_join(locations, full_grid, by = "location")


food1 <- full_grid %>%
  left_join(food, by = c("location", "state_abbreviation","state_spelled_out","region","date", "category"))



# clean/organize data

food1 <- food1 %>% 
tidyr::separate(category, into = c("item", "measurement"), sep = ", per") %>% 
tidyr::separate(location, into = c("location", "delete"), sep = ",") %>% 
select(-delete) %>% 
tidyr::separate(item, into = c("item", "description"), sep = ",", extra = "merge") %>% 
mutate(date_updated = Sys.Date()) %>% 
arrange(location, item, date) %>% 
  group_by(location)

food1$item <- str_to_title(food1$item, locale = "en")

food1 <- food1[order(food1$location != "United States"), ]

food1$description <- str_trim(food1$description, "left")
food1$measurement <- str_trim(food1$measurement, "left")

food1 <-food1 %>% 
  fill(region) %>% 
  mutate(item = if_else(item == "All Ham (Excluding Canned Ham And Luncheon Slices)", "Ham", item))
  

food1 <- food1 %>% 
  mutate(category_bin = case_when(
  item == "Tomatoes" ~ "Produce",
  item == "Lettuce" ~ "Produce",
  item == "Potatoes" ~ "Produce",
  item == "Strawberries" ~ "Produce",
  item == "Bananas" ~ "Produce",
  item == "Oranges" ~ "Produce",
  item == "Lemons" ~ "Produce",
  item == "Flour" ~ "Dry Goods",
  item == "Beans" ~ "Dry Goods",
  item == "Bread" ~ "Dry Goods",
  item == "Rice" ~ "Dry Goods",
  item == "Spaghetti And Macaroni" ~ "Dry Goods",
  item == "Sugar" ~ "Dry Goods",
  item == "Ham" ~ "Meat and Poultry",
  item == "All Uncooked Ground Beef" ~ "Meat and Poultry",
  item == "Chicken" ~ "Meat and Poultry",
  item == "Bacon" ~ "Meat and Poultry",
  item == "Cheddar Cheese" ~ "Dairy and Eggs",
  item == "Yogurt" ~ "Dairy and Eggs",
  item == "Milk" ~ "Dairy and Eggs",
  item == "Eggs" ~ "Dairy and Eggs",
  item == "Potato Chips" ~ "Snacks and Desserts",
  item == "Cookies" ~ "Snacks and Desserts",
  item == "Ice Cream" ~ "Snacks and Desserts",
  item == "All Soft Drinks" ~ "Drinks",
  item == "All Soft Drinks" ~ "Drinks",
  item == "Malt Beverages" ~ "Drinks",
  item == "Wine" ~ "Drinks",
  ))


food1 <- food1 %>% relocate(category_bin, .after = measurement)

#find oldest and newest for text

latest_data <- food1 %>%
  group_by(location,item) %>%
  filter(!is.na(value)) %>%
  filter(date == max(date)) %>%
  rename(latest_value_raw = value, latest_value_adjusted = value_inflation_adjusted, max_date = date) %>% select(-11,-13)


oldest_data <- food1 %>%
  group_by(location, item) %>%
  filter(!is.na(value)) %>%
  filter(date == min(date)) %>%
  rename(oldest_value_adjusted = value_inflation_adjusted, min_date = date, oldest_value_raw = value) %>% select(-13)



food2 <- left_join(latest_data, oldest_data, by = c("location","description", "state_abbreviation","state_spelled_out","region","item","measurement", "category_bin"))

food2 <- food2 %>% mutate(p_change_oldest_newest_adjusted = round(((latest_value_adjusted - oldest_value_adjusted)/(oldest_value_adjusted))*100,1)) %>% select(-14)

food3 <- left_join(food1,food2, by = c("location","state_abbreviation","state_spelled_out","region","item","description","measurement","category_bin"))

food3 <- food3 %>% 
  filter(!((item == "All Soft Drinks" & region == 'Northeast' & measurement == '12 pk.'))) %>% 
  filter(!((item == "All Soft Drinks" & region == 'West' & measurement == '2 liters'))) %>% 
  filter(!((item == "All Soft Drinks" & region == 'South' & measurement == '2 liters'))) %>% 
  filter(!((item == "All Soft Drinks" & region == 'Midwest' & measurement == '2 liters'))) %>% 
  filter(!((item == "Milk" & region == 'Midwest' & description == 'fresh, whole, fortified'))) %>% 
  filter(!((item == "Milk" & region == 'West' & description == 'fresh, low-fat, reduced fat, skim'))) %>% 
  filter(!((item == "Milk" & region == 'South' & description == 'fresh, low-fat, reduced fat, skim'))) %>% 
  filter(!((item == "Milk" & region == 'Northeast' & description == 'fresh, low-fat, reduced fat, skim'))) %>% 
  mutate(description = if_else(item == "Ham", "Excluding Canned Ham And Luncheon Slices", description))

#find items with at least 50% of all years

food4 <- food3 %>% filter(!is.na(value)) %>% filter(location != "United States") %>%
  group_by(location,item) %>%
  summarize(count = n()) %>%
  mutate(data_complete = case_when(
    count >= (max(count))/2 ~ "True",
    count < (max(count))/2 ~ "False"
  )) %>%
  mutate(test = paste(location, item, sep = ", "))

#fnd items with at least 80% of last 3 years

food5 <- food3 %>% filter(!is.na(value)) %>% filter(date >= "2022-01-01") %>% filter(location != "United States") %>%
  group_by(location,item) %>%
  summarize(count = n()) %>%
  filter(count >= (max(count))*.8) %>%
  mutate(data_complete = "True") %>%
  mutate(test = paste(location, item, sep = ", "))

later_items <- food5$test

#replace items marked false for <50% that have >= 80% in last 3 years

food6 <- food4 %>% filter(test %in% later_items) %>% filter(data_complete == "False")
food7 <- food6 %>% mutate(data_complete = "True")
to_replace <- food7$test

food8 <- food4 %>% filter(! test %in% to_replace)
food9 <- rbind(food8, food7)
food9 <- food9 %>% select(-3,-5)

#add back in US values

food10 <- food3 %>% filter(location == "United States") %>%
  group_by(location,item) %>%
  summarize(count = n()) %>%
  mutate(data_complete = case_when(
    count >= (max(food4$count))/2 ~ "True",
    count < (max(food4$count))/2 ~ "False"
  )) %>% select(-3)

food9 <- rbind(food9, food10)

#now mark everything with all data missing as false

food11 <- food3 %>% filter(is.na(value)) %>%
  group_by(location,item) %>%
  summarize(count = n()) %>%
  mutate(data_complete = case_when(
    count == max(food4$count) ~ "False"
  )) %>% filter(data_complete == "False") %>% select(-3)

food9 <- rbind(food9, food11)


food3 <- left_join(food3, food9, by = c("location","item"))


#add in 2019 data for simple situations where the 2019 month matches the latest month
test_for_2019 <- food3 %>%
  mutate(compare_date_2019 = make_date(2019, month(max_date),1)) %>%
  group_by(location, item) %>%
  #filter(!is.na(value)) %>%
  filter(date == compare_date_2019) %>%
  rename(compare_2019_value_adjusted = value_inflation_adjusted, compare_2019_value_raw = value) %>% select(-5,-11,-13)

test_for_2019_2 <- food3 %>%
  mutate(compare_date_2019 = make_date(2019, month(max_date),1)) %>%
  group_by(location,item) %>%
  filter(is.na(value)) %>%
  filter(date == compare_date_2019)

candidates_2019 <- food3 %>%
  filter(year(date) == 2019) %>%
  filter(!is.na(value))

nearest_2019 <- left_join(candidates_2019, test_for_2019, by = c("location","description", "state_abbreviation","state_spelled_out","region","item","measurement", "category_bin","max_date","latest_value_raw","latest_value_adjusted","min_date","oldest_value_raw","oldest_value_adjusted","p_change_oldest_newest_adjusted","data_complete"))
nearest_2019 <- nearest_2019 %>%
  mutate(diff_days = as.numeric(abs(date-compare_date_2019))) %>%
  mutate(ahead= !is.na(date) & date >= compare_date_2019) %>%
  group_by(location,item) %>%
  arrange(diff_days, desc(ahead), date) %>%
  slice_head(n = 1) %>%
  select(-24) %>%
  rename(compare_date_2019 = date) %>%
  mutate(compare_2019_value_raw = case_when(
    !is.na(compare_2019_value_raw) ~ compare_2019_value_raw,
    is.na(compare_2019_value_raw) ~ value
  )) %>% mutate(compare_2019_value_adjusted = case_when(
    !is.na(compare_2019_value_adjusted) ~ compare_2019_value_adjusted,
    is.na(compare_2019_value_adjusted) ~ value_inflation_adjusted
  )) %>% select(-c(10,11,12,13,))

food12 <- left_join(food3, nearest_2019, by = c("location","description", "state_abbreviation","state_spelled_out","region","item","measurement", "category_bin","max_date","latest_value_raw","latest_value_adjusted","min_date","oldest_value_raw","oldest_value_adjusted","p_change_oldest_newest_adjusted","data_complete"))


food3 <- food12 %>% 
  mutate(p_change_2019_newest_adjusted = round(((latest_value_adjusted - compare_2019_value_adjusted)/(compare_2019_value_adjusted))*100,1)) %>%
  select(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,22,23,24,27,21)



produce_bin <- food3 %>% 
  filter(category_bin == "Produce")

dry_goods_bin <- food3 %>% 
  filter(category_bin == "Dry Goods")

meat_bin <- food3 %>% 
  filter(category_bin == "Meat and Poultry")

dairy_bin <- food3 %>% 
  filter(category_bin == "Dairy and Eggs")

snack_bin <- food3 %>% 
  filter(category_bin == "Snacks and Desserts")

drinks_bin <- food3 %>% 
  filter(category_bin == "Drinks")

# CPI data

food_all <- food3 %>% 
  filter(item == "All Food At Home") %>% 
  mutate(p_change_oldest_newest_raw = ((latest_value_raw - oldest_value_raw)/oldest_value_raw)*100) %>% 
  group_by(location) %>%
  slice_tail(n = 1) %>% 
  select(-7,-8,-9,-21)

data_2019 <- food3 %>%
  filter(item == "All Food At Home") %>% 
  mutate(date_2019 = make_date(2019, month(max_date),1)) %>%
  group_by(location, item) %>%
  filter(!is.na(value)) %>%
  filter(date == date_2019) %>%
  rename(value_adjusted_2019 = value_inflation_adjusted, value_raw_2019 = value) %>% 
  mutate(p_change_2019_newest_adjusted = round(((latest_value_adjusted - value_adjusted_2019)/(value_adjusted_2019))*100,1)) %>% 
  mutate(p_change_2019_newest_raw = ((latest_value_raw - value_raw_2019)/value_raw_2019)*100) %>% 
  select(-5,-7,-8,-9,-11,-13,-18)

food_all1 <- left_join(food_all, data_2019, by = c("location","state_abbreviation","state_spelled_out","region","item","max_date","latest_value_raw","latest_value_adjusted","min_date","oldest_value_adjusted","p_change_oldest_newest_adjusted")) %>% 
  select(-2,-5,-8,-21) 


# median household income

current_year <- as.numeric(format(Sys.Date(), "%Y"))

get_most_recent_year <- function() {
  for (year in current_year:2015) {
    result <- tryCatch({
      get_acs(
        geography = "us",
        variables = "B19013_001E",
        geometry = FALSE,
        year = year,
        output = "wide"
      )
      return(year)
    }, error = function(e) NULL)
    
    if (!is.null(result)) return(year)
  }
  return(2021)
}

most_recent_year <- get_most_recent_year()

years <- c(2015, 2019, most_recent_year)

results <- list()

for (year in years) {
  income <- get_acs(
    geography = "us",
    variables = "B19013_001E",
    geometry = FALSE,
    year = year,
    output = "wide"
  )
  results[[year]] <- income
}

income_years <- do.call(rbind, results)

income_years <- income_years %>% select(-1,-4) %>% rename(region = NAME, median_household_income_raw = B19013_001E)

income_years$year <- years
income_years_flipped <- income_years %>% group_by(region, year) %>% summarize(income = sum(median_household_income_raw)) %>% pivot_wider(names_from = year, values_from = income) %>% clean_names()

names(income_years_flipped)[2:4] <- c("median_household_income_raw_2015", "median_household_income_raw_2019", "median_household_income_raw_recent")

#inflation change from 2015 to 2023 is 1.3, inflation change from 2019 to 2023 is 1.19

income_years_flipped$inflation_adjustment_2015 <- c(1.3)
income_years_flipped$inflation_adjustment_2019 <- c(1.19)

income_years_flipped <- income_years_flipped %>% mutate(median_household_income_adj_2015 = median_household_income_raw_2015*inflation_adjustment_2015) %>%
  mutate(median_household_income_adj_2019 = median_household_income_raw_2019*inflation_adjustment_2019) %>%
  mutate(p_change_2015_recent_income = round(((median_household_income_raw_recent-median_household_income_adj_2015)/median_household_income_adj_2015)*100,1)) %>%
  mutate(p_change_2019_recent_income = round(((median_household_income_raw_recent-median_household_income_adj_2019)/median_household_income_adj_2019)*100,1)) %>% 
  rename(location = region)

food_all2 <- left_join(food_all1, income_years_flipped, by = c("location")) 

# write csvs and jsons

write_json(produce_bin, "groceries/produce_data.json", pretty = TRUE, na = "null")
write_csv(produce_bin, "groceries/produce_data.csv", na = "null")

write_json(dry_goods_bin, "groceries/dry_goods_data.json", pretty = TRUE, na = "null")
write_csv(dry_goods_bin, "groceries/dry_goods_data.csv", na = "null")

write_json(meat_bin, "groceries/meat_data.json", pretty = TRUE, na = "null")
write_csv(meat_bin, "groceries/meat_data.csv", na = "null")

write_json(dairy_bin, "groceries/dairy_data.json", pretty = TRUE, na = "null")
write_csv(dairy_bin, "groceries/dairy_data.csv", na = "null")

write_json(snack_bin, "groceries/snack_data.json", pretty = TRUE, na = "null")
write_csv(snack_bin, "groceries/snack_data.csv", na = "null")

write_json(drinks_bin, "groceries/drinks_data.json", pretty = TRUE, na = "null")
write_csv(drinks_bin, "groceries/drinks_data.csv", na = "null")

write_json(food_all2, "groceries/food_cpi.json", pretty = TRUE, na = "null")
write_csv(food_all2, "groceries/food_cpi.csv", na = "null")
