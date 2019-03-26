## 2019-03-26
## @ancv

## Load packages ------------------------------

library(dplyr)
library(purrr)
library(ggplot2)

## Load data ------------------------------

dtaviz <- readr::read_csv("dvs_challenge_1_membership_time_space.csv")

names(dtaviz)
dim(dtaviz)
head(dtaviz)
summary(dtaviz)
map(dtaviz, class)

dtaviz$date <- as.Date(dtaviz$date, "%m/%d/%Y")

bigger_font <- function() theme(text = element_text(size = 17))

## Visual Analysis ------------------------------

## when do people sign up?
ggplot(dtaviz, aes(factor(hour))) +
    geom_bar() +
    bigger_font()

signup_by_hour <- dtaviz %>%
    group_by(hour) %>%
    summarise(n_signup = n()) %>%
    mutate(pct_signup = n_signup / sum(n_signup),
           in_working_hour = hour >= 8 & hour <= 18,
           hour = as.factor(hour))

ggplot(signup_by_hour, aes(hour, pct_signup, fill = in_working_hour)) +
    geom_col() +
    scale_y_continuous(name = NULL, label = scales::percent) +
    scale_x_discrete(name = "Hour in day") +
    labs(title = "When did people sign up DataViz Society?") +
    scale_fill_manual(name = "In Working Hour",
                      values = c("steelblue", "red4")) +
    theme(legend.position = "top") +
    bigger_font()
