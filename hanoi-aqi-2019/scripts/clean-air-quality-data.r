

## Load packages and import data -----------------------------------------------

require(readr)
require(dplyr)
require(tidyr)

aqi_ytd_2015 <- read_csv("./data-raw/Hanoi_PM2.5_2015_YTD.csv")
aqi_ytd_2016 <- read_csv("./data-raw/Hanoi_PM2.5_2016_YTD.csv")
aqi_ytd_2017 <- read_csv("./data-raw/Hanoi_PM2.5_2017_YTD.csv")
aqi_ytd_2018 <- read_csv("./data-raw/Hanoi_PM2.5_2018_YTD.csv")
aqi_ytd_2019 <- read_csv("./data-raw/Hanoi_PM2.5_2019_YTD.csv")

dim(aqi_ytd_2015)
dim(aqi_ytd_2016)
dim(aqi_ytd_2017)
dim(aqi_ytd_2018)
dim(aqi_ytd_2019)

## Combine data sets into one --------------------------------------------------

## check that all files have idential column names
all.equal(names(aqi_ytd_2015), names(aqi_ytd_2016))
all.equal(names(aqi_ytd_2015), names(aqi_ytd_2017))
all.equal(names(aqi_ytd_2015), names(aqi_ytd_2018))
all.equal(names(aqi_ytd_2015), names(aqi_ytd_2019))

## change colnames of all data frames
aqi_colnames <- c("site", "parameter", "date", "year", "month", "day", "hour",
                  "nowcast_conc", "aqi", "aqi_categ", "raw_conc", "conc_unit",
                  "duration", "qc_name")

names(aqi_ytd_2015) <- aqi_colnames
names(aqi_ytd_2016) <- aqi_colnames
names(aqi_ytd_2017) <- aqi_colnames
names(aqi_ytd_2018) <- aqi_colnames
names(aqi_ytd_2019) <- aqi_colnames

## check that all data frames have idential column data type
all.equal(sapply(aqi_ytd_2015, class), sapply(aqi_ytd_2016, class))
all.equal(sapply(aqi_ytd_2015, class), sapply(aqi_ytd_2017, class))
all.equal(sapply(aqi_ytd_2015, class), sapply(aqi_ytd_2018, class))
all.equal(sapply(aqi_ytd_2015, class), sapply(aqi_ytd_2019, class))

## combine them all
aqi <- bind_rows(aqi_ytd_2015, aqi_ytd_2016, aqi_ytd_2017,
                 aqi_ytd_2018, aqi_ytd_2019)
