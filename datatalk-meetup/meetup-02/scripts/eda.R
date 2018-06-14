

library(dplyr)
library(tidyr)
library(purrr)

air_quality <- readr::read_csv("./data/hanoi-air-quality.csv")

dim(air_quality)
## [1] 29928    12
names(air_quality)
count(air_quality, site)
count(air_quality, parameter)

## number of instances per year/month/day

count(air_quality, year)
count(air_quality, month)

## count instances by qc_name

count(air_quality, qc_name)

## 'Missing' happend mostly in 2015

count(air_quality, year, qc_name) %>%
    spread(qc_name, n)

## all groups of qc_name has missing values

filter(air_quality, qc_name == "Suspect") %>%
    count(is.na(aqi))

filter(air_quality, qc_name == "Valid") %>%
    count(is.na(aqi))

filter(air_quality, qc_name == "Missing") %>%
    count(is.na(aqi))

filter(air_quality, qc_name == "Invalid") %>%
    count(is.na(aqi))

## in this case, I choose to be strict about keeping only valid records

air_quality <- filter(air_quality, qc_name == "Valid" & !is.na(aqi))

## coverage of data reduced significantly

dim(air_quality)
## [1] 20724    12
range(air_quality$date)
## [1] "2015-12-09 15:00:00 UTC" "2018-06-01 00:00:00 UTC"
