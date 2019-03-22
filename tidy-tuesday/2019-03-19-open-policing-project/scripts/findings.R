

library(dplyr)
library(purrr)
read_csv <- readr::read_csv

policing <- read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")

## write.csv(policing,
##           file = "../data/agg-open-policing-data.csv",
##           row.names = FALSE)

## -----------------------------------------------------------------------------
## EDA

dim(policing)
head(policing)
tail(policing)
glimpse(policing)


## check duplicates
anyDuplicated(policing)

## check NA
map(policing, ~ sum(is.na(.x)) * 100 / length(.x)) %>%
    keep( ~ .x > 0)

## how many unique counties?
count(policing, location, sort = TRUE)

## how many states?
count(policing, state, sort = TRUE)     # only 17
