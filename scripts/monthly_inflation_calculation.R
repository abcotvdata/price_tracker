#install.packages("blscrapeR")
library(blscrapeR)
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(blsR)
options(timeout=300)

#library(usethis)
#edit_r_environ()
#readRenviron("~/.Renviron")
Sys.getenv("BLS_KEY")

inflation <- bls_api("CUSR0000SA0", startyear = 2015) #all items, all consumers price index CPI
inflation_current <- bls_api("CUSR0000SA0", startyear = 2025) %>% select(-4) #for some reason, the above isn't including 2025
inflation <- rbind(inflation, inflation_current)

inflation$date <- paste(inflation$year,inflation$period,sep="-")
inflation$date <- gsub("M","", inflation$date)
inflation$date <- paste(inflation$date,"01",sep = "-")

inflation$date <- as.Date(inflation$date, format = "%Y-%m-%d")

inflation_latest_idx <- which.max(inflation$date)

latest_values <- inflation$value[inflation_latest_idx]

inflation$inflation_adjustment <- round((latest_values/inflation$value),2)

#pare down inflation file
inflation_small <- inflation %>% 
  select(7,8) %>% 
  arrange(date)

write_csv(inflation_small, "scripts/inflation_adjustment.csv")
