

## -----------------------------------------------------------------------------
## Load packages and import data

library(readr)
library(dplyr)
library(ggplot2)

by_tag_year <- read_csv("./data/by_tag_year.csv")

names(by_tag_year)
dim(by_tag_year)
head(by_tag_year)

## -----------------------------------------------------------------------------
## Data exploration


## Add fraction column
by_tag_year_fraction <- by_tag_year %>%
    mutate(fraction = number / year_total)


## Has R been growing or shrinking?

r_over_time <- by_tag_year_fraction %>%
    filter(tag == "r")

## Visualizing change over time
ggplot(r_over_time, aes(year, fraction)) +
    geom_line()


## How about dplyr and ggplot2
selected_tags <- c("r", "dplyr", "ggplot2")
selected_tags_over_time <- by_tag_year_fraction %>%
    filter(tag %in% selected_tags)

ggplot(selected_tags_over_time, aes(year, fraction, color = tag)) +
    geom_line()

## What are the most asked-about tags?
sorted_tags <- by_tag_year_fraction %>%
    group_by(tag) %>%
    summarise(tag_total = sum(number)) %>%
    ungroup() %>%
    arrange(desc(tag_total))

## Get the six largest tags
highest_tags <- head(sorted_tags$tag)

by_tag_subset <- by_tag_year_fraction %>%
    filter(tag %in% highest_tags)

ggplot(by_tag_subset, aes(year, fraction, color = tag)) +
    geom_line()

## Some more tags
my_tags <- c("android", "ios", "windows-phone")
by_tag_subset <- by_tag_year_fraction %>%
    filter(tag %in% my_tags)

ggplot(by_tag_subset, aes(year, fraction, color = tag)) +
    geom_line()
