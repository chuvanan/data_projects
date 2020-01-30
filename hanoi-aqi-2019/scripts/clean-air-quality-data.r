

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

## Helper functions ------------------------------------------------------------

categorize_aqi <- function(aqi) {
    if (aqi <= 50) {
        out <- "Good"
    } else if (aqi >= 51 & aqi <= 100) {
        out <- "Moderate"
    } else if (aqi >= 101 & aqi <= 150) {
        out <- "Unhealthy for Sensitive Groups"
    } else if (aqi >= 151 & aqi <= 200) {
        out <- "Unhealthy"
    } else if (aqi >= 201 & aqi <= 300) {
        out <- "Very Unhealthy"
    } else if (aqi >= 301) {
        out <- "Hazardous"
    }
    return(out)
}


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

## EDA -------------------------------------------------------------------------

aqi$site <- NULL
aqi$parameter <- NULL
aqi$nowcast_conc[aqi$nowcast_conc == -999] <- NA
aqi$aqi[aqi$aqi == -999] <- NA
aqi$conc_unit <- NULL
aqi$duration <- NULL
aqi$aqi_categ[aqi$aqi_categ == "N/A"] <- NA
aqi$raw_conc[aqi$raw_conc == -999] <- NA

## remove missing records
aqi <- aqi[!is.na(aqi$aqi), ]

## parse dates
aqi$date <- as.POSIXct(aqi$date, format = "%Y-%m-%d %I:%M %p")
aqi$date <- as.Date(aqi$date)
aqi$weekdays <- weekdays(aqi$date)

prop.table(table(aqi$weekdays, useNA = "always")) * 100

## confirm AQI levels are accurately classified
aqi_levels <- sapply(aqi$aqi, categorize_aqi)
table(aqi_levels == aqi$aqi_categ)

aqi$aqi_categ <- factor(aqi$aqi_categ,
                        levels = c("Good", "Moderate", "Unhealthy for Sensitive Groups",
                                   "Unhealthy", "Very Unhealthy", "Hazardous"),
                        ordered = TRUE)
table(aqi$aqi_categ, useNA = "ifany") * 100 / NROW(aqi)

## Export ----------------------------------------------------------------------

## saveRDS(aqi, "./data/hanoi-air-quality.rds")
