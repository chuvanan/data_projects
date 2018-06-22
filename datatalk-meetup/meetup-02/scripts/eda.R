

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

setwd("~/Documents/data_projects/datatalk-meetup/meetup-02/")
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


air_quality <- air_quality %>%
    mutate(date_ymd = format(date, "%y-%m-%d"),
           date_ymd = as.Date(date_ymd))

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

ggplot(data = air_quality, aes(month, aqi, group = day)) +
    geom_line()

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


ggplot(air_quality, aes(hour, aqi, group = year)) +
    geom_path()

air_quality %>%
    group_by(date_ymd) %>%
    summarise(min_aqi = min(aqi),
              max_aqi = max(aqi)) %>%
    ungroup() %>%
    ggplot(aes(date_ymd, ymin = min_aqi, ymax = max_aqi)) +
    geom_linerange(color = 'wheat2') +
    theme_minimal()

air_quality %>%
    mutate(dayth = lubridate::yday(date_ymd)) %>%
    pull(dayth) %>%
    head()

new_air <- air_quality %>%
    mutate(day_th = lubridate::yday(date_ymd)) %>%
    group_by(day_th) %>%
    summarise(max_aqi = max(aqi),
              min_aqi = min(aqi),
              avg_aqi = mean(aqi),
              se_aqi = sd(aqi) / sqrt(length(aqi))) %>%
    mutate(upper_aqi = avg_aqi + (2.101 * se_aqi),
           lower_aqi = avg_aqi - (2.101 * se_aqi)) %>%
    ungroup()

eom <- RTA::end_of_month(seq.Date(from = as.Date('2018-01-01'),
                                  by = '1 month', length.out = 12L))
eom <- lubridate::yday(eom)

p <- ggplot(new_air) +
    geom_hline(yintercept = c(50, 100, 150, 200, 300), color = 'gray70',
               size = 0.5, linetype = 3) +
    geom_vline(xintercept = eom, color = 'gray70',
               size = 0.5, linetype = 3) +
    geom_linerange(aes(day_th, ymin = min_aqi, ymax = max_aqi),
                   color = 'wheat2') +
    geom_linerange(aes(day_th, ymin = lower_aqi, ymax = upper_aqi),
                   color = 'gray1') +
    labs(x = NULL, y = "Air Quality Index (AQI)",
         title = 'The polluted city',
         subtitle = "Data shows Hanoi's daily outdoor air quality from Dec, 2015 to Jun, 2018\n
0-50: Good | 51-100: Moderate | 101-150: Unhealthy for Sensitive Groups | 151-200: Unhealthy | 201-300: Very Unhealthy | 300+: Hazardous",
caption = "Data source: U.S. Embassy Hanoi Air Quality Monitor") +
    scale_y_continuous(breaks = c(50, 100, 150, 200, 300)) +
    scale_x_continuous(breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                       labels = c("January", "February", "March", "April",
                                  "May", "June", "July", "August", "September",
                                  "October", "November", "December"),
                       expand = c(0, 0)) +
    theme_minimal(base_family = 'Roboto Slab') +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 30L),
          plot.subtitle = element_text(size = 15L, color = 'gray30'),
          axis.text.x = element_text(size = 15L, color = "black"),
          axis.text.y = element_text(size = 14L),
          axis.title.y = element_text(size = 15, color = "gray30", vjust = 4),
          plot.caption = element_text(size = 14L, vjust = -4, color = "gray30"),
          plot.margin = unit(c(1.2, 1, 1, 1), "cm"))
p + geom_linerange(aes(x = 181, ymin = 250, ymax = 350),
                   size = 2, color = "wheat2") +
    geom_linerange(aes(x = 181, ymin = 280, ymax = 320),
                   size = 2, color = "gray30") +
    geom_linerange(aes(x = 185, ymin = 280, ymax = 320),
                   size = 0.35, color = "gray30") +
    annotate("text", 194, 350, label = "Record High",
             size = 5, color = "gray30") +
    annotate("text", 194, 250, label = "Record Low",
             size = 5, color = "gray30") +
    annotate("text", 199, 300, label = "Normal Range",
             size = 5, color = "gray30")

dev.size("in")
## [1] 15.38542  6.48294

dev.size("cm")
## [1] 39.07896 16.46667

dev.size("px")
## [1] 1477  624
