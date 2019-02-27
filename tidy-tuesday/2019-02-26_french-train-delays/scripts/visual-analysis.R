

## -----------------------------------------------------------------------------
## Load packages and import data

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

read_csv <- readr::read_csv
full_train <- read_csv("../data/full_trains.csv")
## hint: readr::spec() for full column specification

## -----------------------------------------------------------------------------
## Visual analysis


## What's the most source of delay?
source_of_delay <- full_train %>%
    select(starts_with("delay")) %>%
    gather(source_delay, pct) %>%
    mutate(source_delay = forcats::fct_reorder(source_delay, pct, median)) %>%
    ggplot(aes(source_delay, pct)) +
    geom_boxplot() +
    scale_y_continuous(label = scales::percent_format()) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1))

ggsave(filename = "../figures/source_of_delay.pdf", source_of_delay)

## are there difference between service?


source_of_delay_by_service <- full_train %>%
    select(service, starts_with("delay")) %>%
    gather(source_delay, pct, delay_cause_external_cause:delay_cause_travelers) %>%
    mutate(source_delay = forcats::fct_reorder(source_delay, pct, median)) %>%
    ggplot(aes(source_delay, pct)) +
    geom_boxplot() +
    scale_y_continuous(label = scales::percent_format()) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1)) +
    facet_wrap( ~ service)

## ==> traffic management appears to be the major cause of delays in internation
## service

ggsave(filename = "../figures/source_of_delay_by_service.pdf", source_of_delay_by_service)

## what's the percentage of train departing late? Trend? Difference between
## stations?

full_train %>%
    mutate(pct_depature_late = num_late_at_departure / total_num_trips) %>%
    ggplot(aes(pct_depature_late)) +
    geom_histogram(color = "white") +
    scale_x_continuous(label = scales::percent_format()) +
    facet_wrap( ~ service) +
    theme_minimal(base_size = 20)

full_train %>%
    mutate(pct_depature_late = num_late_at_departure / total_num_trips) %>%
    arrange(desc(pct_depature_late)) %>%
    select(departure_station, arrival_station, pct_depature_late)

## remove comment columns
full_train <- full_train %>%
    select(-contains("comment"))

## https://today.rtl.lu/news/luxembourg/1306023.html
## punctuality rate?

full_train <- full_train %>%
    mutate(pct_arriving_late = num_arriving_late / total_num_trips,
           ymonth = if_else(nchar(month) < 2,
                            as.numeric(paste0(year, "0", month)),
                            as.numeric(paste0(year, month))))

pct_arriving_late <- full_train %>%
    mutate(arrival_station_group = forcats::fct_lump(arrival_station, 5)) %>%
    ggplot(aes(pct_arriving_late)) +
    geom_histogram(color = "white") +
    facet_wrap( ~ arrival_station_group) +
    theme(text = element_text(size = 15))

ggsave(filename = "../figures/pct_arriving_late.pdf", pct_arriving_late)
