

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
