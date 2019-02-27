

## -----------------------------------------------------------------------------
## Load packages and import data

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

read_csv <- readr::read_csv
full_train <- read_csv("../data/full_trains.csv")


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
