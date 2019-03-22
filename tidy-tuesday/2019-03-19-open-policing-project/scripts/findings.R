

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

count(policing, driver_race)
## only study three major groups of races: Black, Hispanic, White

## distribution of stop_per_year
par(cex = 1.3)
hist(log10(policing$stops_per_year), breaks = 30,
     xlab = "Stops Per Year (Log10)", main = "SPY For All Counties",
     col = "steelblue", border = "gray")
box()

par(cex = 1.3)
hist(log10(policing$stop_rate), breaks = 30,
     col = "steelblue", border = "gray",
     xlab = "Stop Rate (Log10)",
     main = "Stop Rate for All Counties")
box()

quantile(policing$stop_rate, probs = seq(0, 1, 0.01), na.rm = TRUE)
## why stop_rate > 1?
