library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(httr2)
library(readxl)
library(writexl)
options(timeout=300)


#bring in file from BLS-- regular download.file doesn't work so we need to make a temp file and then delete it.

url <- "https://www.bls.gov/cpi/research-series/r-cpi-u-rs-allitems.xlsx"

tmp <- tempfile(fileext = ".xlsx")

req <- request(url) |>
  req_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120 Safari/537.36",
    `Accept`     = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,*/*",
    `Accept-Language` = "en-US,en;q=0.9",
    `Accept-Encoding` = "gzip, deflate, br",
    `Connection`      = "keep-alive",
    `Referer`    = "https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm",
    `Sec-Fetch-Dest`  = "document",
    `Sec-Fetch-Mode`  = "navigate",
    `Sec-Fetch-Site`  = "same-origin"
  )

resp <- req_perform(req)

writeBin(resp_body_raw(resp), tmp)

research_collection_data <- read_excel(tmp, skip = 5) %>% clean_names()

write_xlsx(research_collection_data, "inflation/r-cpi-u-rs-allitems.xlsx")
