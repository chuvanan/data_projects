

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)


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
air_quality$aqi <- as.double(air_quality$aqi)
## coverage of data reduced significantly

dim(air_quality)
## [1] 20724    12
range(air_quality$date)
## [1] "2015-12-09 15:00:00 UTC" "2018-06-01 00:00:00 UTC"


## -----------------------------------------------------------------------------
## Visual analysis

## bi-modal
ggplot(data = air_quality, aes(aqi)) +
    geom_histogram(color = "white", binwidth = 10)

ggplot(data = air_quality, aes(aqi)) +
    geom_histogram(color = "white", binwidth = 10) +
    facet_wrap( ~ year)

## monthly seasonality
ggplot(data = air_quality, aes(aqi)) +
    geom_histogram(color = "white", binwidth = 10) +
    facet_wrap( ~ month)

ggplot(data = air_quality, aes(month, aqi)) +
    geom_boxplot()

ggplot(data = air_quality, aes(aqi)) +
    geom_histogram(color = "white", binwidth = 10) +
    facet_wrap( ~ day)

ggplot(data = air_quality, aes(aqi)) +
    geom_histogram(color = "white", binwidth = 10) +
    facet_wrap( ~ hour)

## monthly seasonality
ggplot(data = air_quality, aes(aqi)) +
    geom_histogram(color = "white", binwidth = 10) +
    facet_grid(month ~ day)

ggplot(air_quality, aes(date, aqi)) +
    geom_line() +
    geom_smooth()

air_quality %>%
    mutate(date_ymd = format(date, "%y-%m-%d"),
           date_ymd = as.Date(date_ymd)) %>%
    group_by(date_ymd) %>%
    summarise(min_aqi = min(aqi),
              avg_aqi = mean(aqi),
              max_aqi = max(aqi)) %>%
    ungroup() %>%
    ggplot(aes(date_ymd, avg_aqi)) +
    geom_line()
