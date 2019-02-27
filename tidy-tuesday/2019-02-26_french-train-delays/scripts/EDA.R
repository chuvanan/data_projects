

## -----------------------------------------------------------------------------
## Load packages and import data

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(corrplot)

read_csv <- readr::read_csv
full_train <- read_csv("../data/full_trains.csv")

dim(full_train)
## 5462   27
names(full_train)
glimpse(full_train)

## helper functions

count_na <- function(x) {
    sum(is.na(x)) * 100 / length(x)
}

gghist <- function(dta, col) {
    col_q <- enquo(col)
    ggplot(dta, aes(!!col_q)) +
        geom_histogram(color = "white")
}

ggscatter <- function(dta, col_x, col_y) {
    col_x_q <- enquo(col_x)
    col_y_q <- enquo(col_y)
    ggplot(dta, aes(!!col_x_q, !!col_y_q)) +
        geom_point(size = 2)
}


## -----------------------------------------------------------------------------
## Initial data exploration

## any missing values?
map_dbl(full_train, count_na) %>%
    keep( ~ .x > 0) %>%
    sort()

## what's service?
full_train %>%
    count(service)
## service           n
## <chr>         <int>
## 1 NA             1430
## 2 International   432
## 3 National       3600

full_train %>%
    filter(is.na(service)) %>%
    count(year, month)
## every year-month period has 130 missing records of 'service'

full_train %>%
    filter(is.na(service)) %>%
    count(departure_station, arrival_station)

n_distinct(full_train$departure_station) # 59 unique station
n_distinct(full_train$arrival_station) # 59 unique station

departure_station_late <- full_train %>%
    filter(is.na(service)) %>%
    count(departure_station, sort = TRUE) %>%
    mutate(departure_station = forcats::fct_reorder(departure_station, n)) %>%
    ggplot(aes(departure_station, n)) +
    geom_col() +
    coord_flip()

arrival_station_late <- full_train %>%
    filter(is.na(service)) %>%
    count(arrival_station, sort = TRUE) %>%
    mutate(arrival_station = forcats::fct_reorder(arrival_station, n)) %>%
    ggplot(aes(arrival_station, n)) +
    geom_col() +
    coord_flip()

gridExtra::grid.arrange(departure_station_late,
                        arrival_station_late,
                        ncol = 2)

## any duplicated rows?
anyDuplicated(full_train)

## remove comments (I don't know French)
full_train <- full_train %>% select(-contains("comment"))

## check the distribution of numeric variables
gghist(full_train, journey_time_avg)
gghist(full_train, total_num_trips)
gghist(full_train, num_of_canceled_trains) # skewed, long tail

gghist(full_train, num_late_at_departure)

quantile(full_train$num_late_at_departure,
         probs = seq(0, 1, 0.1))

gghist(full_train, num_arriving_late)

quantile(full_train$num_arriving_late,
         na.rm = TRUE,
         probs = seq(0, 1, 0.1))

gghist(full_train, avg_delay_late_at_departure)

quantile(full_train$avg_delay_all_arriving,
         probs = seq(0, 1, 0.1))

quantile(full_train$avg_delay_all_departing,
         probs = seq(0, 1, 0.1))

ggscatter(full_train, num_greater_15_min_late, num_greater_30_min_late)
ggscatter(full_train, num_greater_15_min_late, num_greater_60_min_late)
ggscatter(full_train, num_greater_30_min_late, num_greater_60_min_late)

gghist(full_train, num_greater_15_min_late)
gghist(full_train, num_greater_30_min_late)
gghist(full_train, num_greater_60_min_late)


## GGally::ggpairs(
##             full_train[, !names(full_train) %in% c("year", "month", "service",
##                                                    "departure_station", "arrival_station")]
##         )

## ==> not really informative

## let's try correlation plot

cor(full_train[, !names(full_train) %in% c("year", "month", "service",
                                           "departure_station", "arrival_station")],
    use = "complete.obs") %>%
    corrplot::corrplot(method = "shade",
                       order = "FPC",
                       type = "upper")

cor(full_train[, !names(full_train) %in% c("year", "month", "service",
                                           "departure_station", "arrival_station")],
    use = "complete.obs") %>%
    corrplot::corrplot(method = "shade",
                       order = "AOE",
                       type = "upper")
