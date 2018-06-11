## -----------------------------------------------------------------------------
## This script aims to clean Hanoi Air Quality data sets that collected by US Embassy
## 10/06/2018
## @anchu
## -----------------------------------------------------------------------------

library(readr)
library(dplyr)
library(purrr)
library(lubridate)

csv_files <- list.files("./data", pattern = "_YTD", full.names = TRUE)
dta <- map_dfr(csv_files, read_csv, col_types = cols(.default = col_character()))

## Missing values in variable `AQI`: -999 --> NA

dta <- dta %>%
    mutate(AQI = as.double(AQI),
           AQI = ifelse(AQI < 0, NA, AQI))

dta <- mutate(dta, Unit = "UG/M3")

## Parse date

dta <- mutate(dta, `Date (LT)` = ymd_hm(`Date (LT)`))


## Categorize AQI level

dta <- mutate(dta,
              `AQI Category` = case_when(
                  AQI <= 50 ~ "Good",
                  AQI >= 51 & AQI <= 100 ~ "Moderate",
                  AQI >= 101 & AQI <= 150 ~ "Unhealthy for Sensitive Groups",
                  AQI >= 151 & AQI <= 200 ~ "Unhealthy",
                  AQI >= 201 & AQI <= 300 ~ "Very Unhealthy",
                  AQI >= 301 & AQI <= 500 ~ "Hazardous",
                  is.na(AQI) ~ NA_character_
              ))

## Select necessary columns for analysis

dta <- select(dta,
              site = Site,
              parameter = Parameter,
              date = `Date (LT)`,
              year = Year,
              month = Month,
              day = Day,
              hour = Hour,
              aqi = AQI,
              unit = Unit,
              duration = Duration,
              qc_name = `QC Name`,
              aqi_categ = `AQI Category`)

## Export output

write_csv(dta, path = "./data/hanoi-air-quality.csv")
