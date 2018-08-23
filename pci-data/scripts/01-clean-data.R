


## -----------------------------------------------------------------------------
## Load packages

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
read_csv <- readr::read_csv


## -----------------------------------------------------------------------------
## Get Vietnam GDP Growth Data from WorldBank

gdp <- read_csv("./data/API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_10058058.csv", skip = 4)
gdp$X63 <- NULL

names(gdp) <- c("country_name", "country_code", "indct_name", "indct_code",
                paste0("y", 1960:2017))
gdp_vietnam <- gdp[gdp$country_code == "VNM", ]
gdp_vietnam <- Filter(function(x) !is.na(x), gdp_vietnam)

## keep data from 2016 to ben consistent with PCI data set
gdp_vietnam <- gdp_vietnam %>%
    gather(year, growth, -c(country_name:indct_code)) %>%
    mutate(year = as.integer(gsub("y", "", year))) %>%
    filter(year >= 2006)

## write.csv(gdp_vietnam, "./data/vietnam-gdp-growth-rate.csv", row.names = FALSE)
