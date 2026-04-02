#install.packages("blscrapeR")
library(blscrapeR)
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(blsR)
library(httr2)
library(readxl)
options(timeout=300)

#library(usethis)
#edit_r_environ()
#readRenviron("~/.Renviron")
Sys.getenv("BLS_KEY")

# Get current date information
current_date <- Sys.Date()
current_year <- year(current_date)
current_month <- month(current_date)

# Calculate dynamic date ranges, (this returns the year only)
start_10yr_back <- year(current_date %m-% months(123))  # 10 years and three months ago
start_5yr_back <- current_year - 5    # 5 years ago

# Calculate specific date thresholds (full date 10 years back from the current_date variable (sys.date(), etc)
date_10yr_back <- current_date %m-% years(10) %m-% months(3)   # Exact date 10 years and 3 months ago
date_5yr_back <- current_date %m-% years(5)     # Exact date 5 years ago
lubridate::day(date_5yr_back) <- 1 #change 5 years ago day to match the other columns so that comparisons later match


inflation_bls <- bls_api("CUSR0000SA0", startyear = start_10yr_back) #all items, all consumers price index CPI
inflation_current <- bls_api("CUSR0000SA0", startyear = current_year) %>% select(-c(latest)) #for some reason, the above isn't including 2025


inflation <- bind_rows(inflation_bls, inflation_current)

#rm(inflation)

inflation$date <- paste(inflation$year,inflation$period,sep="-")
inflation$date <- gsub("M","", inflation$date)
inflation$date <- paste(inflation$date,"01",sep = "-")

inflation$date <- as.Date(inflation$date, format = "%Y-%m-%d")

#rebase so the values aren't messed up

base_year <- start_10yr_back

rebase <- function(df, base_year) {
  base_val <- df %>%
    filter(year == base_year) %>%
    summarize(base = mean(value)) %>%
    pull(base)
  
  df %>%
    mutate(value = value / base_val * 100)
}


inflation_rebase <- rebase(inflation, start_10yr_back)


#replace everything before 2025 with CPI-U-RS

#bring in file from BLS-- regular download.file doesn't work so we need to make a temp file and then delete it.
#as of March 2026, BLS is blocking us from grabbing this file, so it needs to be manually downloaded once a year.

# url <- "https://www.bls.gov/cpi/research-series/r-cpi-u-rs-allitems.xlsx"
# 
# tmp <- tempfile(fileext = ".xlsx")
# 
# req <- request(url) |>
#   req_headers(
#     `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120 Safari/537.36",
#     `Accept`     = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,*/*",
#     `Accept-Language` = "en-US,en;q=0.9",
#     `Accept-Encoding` = "gzip, deflate, br",
#     `Connection`      = "keep-alive",
#     `Referer`    = "https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm",
#     `Sec-Fetch-Dest`  = "document",
#     `Sec-Fetch-Mode`  = "navigate",
#     `Sec-Fetch-Site`  = "same-origin"
#   )
# 
# resp <- req_perform(req)
# 
# writeBin(resp_body_raw(resp), tmp)

# use the manually downloaded file

tmp <- tempfile(fileext = ".xlsx")
download.file(
  "https://raw.githubusercontent.com/abcotvdata/price_tracker/main/inflation/r-cpi-u-rs-allitems.xlsx",
  destfile = tmp,
  mode = "wb"
)

#skip the first 5 rows of notes
inflation2 <- read_excel(tmp, skip = 5) %>% clean_names()

#rm(inflation2)

unlink(tmp)

#rearrange data so that months and years are both rows

inflation2 <- inflation2 %>% 
  pivot_longer(
    cols = -year,
    names_to = "month",
    values_to = "value",
    values_drop_na = FALSE
  ) %>% filter(month != "avg")

#create a date column to match other file
inflation2$date <- paste(inflation2$year, inflation2$month, "01", sep = "-")
inflation2$date <- as.Date(inflation2$date, format = "%Y-%b-%d")

#rebase to match CPI-U
inflation2_rebase <- rebase(inflation2, start_10yr_back)

#Set variable to first of the month exactly 10 years ago, this will be used in inflation 3 to get all data from exactly 10 years ago to the most current date
normalized_10yr <- date_10yr_back
  
lubridate::day(normalized_10yr) <- 1

#remove everything before 10 years ago and reorder data
inflation2_rebase <- inflation2_rebase %>%
  filter(date >= normalized_10yr) %>%
  select(c(date,value))

#filter initial inflation file to only be data not included in CPI-U-RS. This should still work when the 2025 data comes out
inflation3 <- inflation_rebase %>% select(c(date,value)) %>% filter(date > max(inflation2$date))

#replace NA for Oct 2025 with Sept 2025 (and any future NA values)
inflation3 <- inflation3 %>% arrange(date) %>% fill(value, .direction = "down")

#combine CPI-U-RS and CPI-U
inflation4 <- bind_rows(inflation2_rebase, inflation3)

#pull latest value
inflation_latest_idx <- which.max(inflation4$date)

latest_values <- inflation4$value[inflation_latest_idx]

inflation4$inflation_adjustment <- round((latest_values/inflation4$value),2)

inflation5 <- inflation4 %>% 
  select(date, inflation_adjustment)

#output_dir <- "/Users/aguia216/Desktop/CODING_PROJECTS/price-tracker16/src/lib/data"
#dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

#write_csv(inflation5, file.path(output_dir, "inflation_test.csv"), na = "null")

write_csv(inflation5, "inflation/inflation_adjustment.csv")
