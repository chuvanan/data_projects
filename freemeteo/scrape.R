## Scraping data from freemeteo.vn
## 18/04/2017
## anchu@rta.vn


library(rvest)
library(xml2)
library(dplyr)

years <- rep(2000:2017, each = 12)
months <- rep(1:12, times = length(2000:2017))
urls <- mapply(paste0,
               rep("https://freemeteo.vn/weather/hanoi/history/monthly-history/?gid=1581130&station=11376&month=", times = 216),
               months,
               rep("&year=", times = 216),
               years,
               rep("&language=english&country=vietnam", times = 216))
names(urls) <- NULL

## urls <- urls[!grepl("month=(1|2|3|4|5|6|7|8|9|10|11)\\&year=1958", urls)]
## urls <- urls[!grepl("month=(5|6|7|8|9|10|11|12)\\&year=2017", urls)]

extract_weather_table <- function(x) {
    out <- xml2::read_html(x)
    out <- rvest::html_table(out)
    out[[6]]
}

weather_hist <- lapply(urls, function(x) try(extract_weather_table(x)))
# saveRDS(weather_hist, "~/Dropbox/freemeteo/weather_hist.rds")

weather_hist <- readRDS("~/Dropbox/freemeteo/weather_hist.rds")

lapply(weather_hist, dim)

## weather_hist <- lapply(weather_hist, function(x) Filter(function(y) ncol(y) == 10, x))
## weather_hist <- lapply(weather_hist, function(x) x[["NULL"]])
weather_hist <- Reduce(rbind, weather_hist)
weather_hist[] <- lapply(weather_hist, as.character)

names(weather_hist) <- c("date", "min_temp", "max_temp", "max_steady_wind", "max_wind_gust",
                         "precipitation", "snow_depth", "pressure", "icon", "description")

head(weather_hist)

## reformat dates
weather_hist$date <- as.Date(weather_hist$date, "%m/%d/%Y")

## clean temperature
weather_hist$min_temp[weather_hist$min_temp == "N/A"] <- NA
weather_hist$min_temp <- gsub("°C", "", weather_hist$min_temp)
weather_hist$min_temp <- as.integer(weather_hist$min_temp)

weather_hist$max_temp[weather_hist$max_temp == "N/A"] <- NA
weather_hist$max_temp <- gsub("°C", "", weather_hist$max_temp)
weather_hist$max_temp <- as.integer(weather_hist$max_temp)

## clean wind
weather_hist$max_steady_wind[weather_hist$max_steady_wind == "N/A"] <- NA
weather_hist$max_steady_wind <- gsub(" Km/h", "", weather_hist$max_steady_wind)
weather_hist$max_steady_wind <- as.integer(weather_hist$max_steady_wind)

weather_hist$max_wind_gust[weather_hist$max_wind_gust == "N/A"] <- NA
weather_hist$max_wind_gust <- gsub(" Km/h", "", weather_hist$max_wind_gust)
weather_hist$max_wind_gust <- as.integer(weather_hist$max_wind_gust)

## clean precipitation
weather_hist$precipitation[weather_hist$precipitation == "N/A"] <- NA
weather_hist$precipitation <- gsub("mm", "", weather_hist$precipitation)
weather_hist$precipitation <- as.numeric(weather_hist$precipitation)

## clean snow depth
weather_hist$snow_depth <- NA

weather_hist$pressure[weather_hist$pressure == "N/A"] <- NA
weather_hist$pressure <- gsub("mb", "", weather_hist$pressure)
weather_hist$pressure <- as.numeric(weather_hist$pressure)

weather_hist$icon <- NULL

weather_hist$description[weather_hist$description == ""] <- NA
weather_hist$description[weather_hist$description == "weather events not reported"] <- NA

# write.csv(weather_hist, "~/Dropbox/freemeteo/hanoi_weather_history.csv", row.names = F)
# write.csv(weather_hist, "~/Dropbox/R-workshop/slides/data_visualisation/hanoi_weather_history.csv", row.names = F)
## -----------------------------------------------------------------------------
## 21/05/2017
## crawling for data from 14/04/2017

years <- 2017
months <- 4:5
urls <- mapply(paste0,
               rep("http://freemeteo.vn/weather/hanoi/history/monthly-history/?gid=1581130&station=11376&month=", times = 2),
                       months,
                       rep("&year=", times = 2),
                       years,
                       rep("&language=english&country=vietnam", times = 2))
        names(urls) <- NULL

        weather_hist <- lapply(urls, XML::readHTMLTable)

        weather_hist <- lapply(weather_hist, function(x) Filter(function(y) ncol(y) == 10, x))
        weather_hist <- lapply(weather_hist, function(x) x[["NULL"]])
        weather_hist <- Reduce(rbind, weather_hist)
        weather_hist[] <- lapply(weather_hist, as.character)

        names(weather_hist) <- c("date", "min_temp", "max_temp", "max_steady_wind", "max_wind_gust",
                                 "precipitation", "snow_depth", "pressure", "icon", "description")

        ## reformat dates
        weather_hist$date <- as.Date(weather_hist$date, "%m/%d/%Y")

        ## clean temperature
        weather_hist$min_temp[weather_hist$min_temp == "N/A"] <- NA
        weather_hist$min_temp <- gsub("°C", "", weather_hist$min_temp)
        weather_hist$min_temp <- as.integer(weather_hist$min_temp)

        weather_hist$max_temp[weather_hist$max_temp == "N/A"] <- NA
        weather_hist$max_temp <- gsub("°C", "", weather_hist$max_temp)
        weather_hist$max_temp <- as.integer(weather_hist$max_temp)

        ## clean wind
        weather_hist$max_steady_wind[weather_hist$max_steady_wind == "N/A"] <- NA
        weather_hist$max_steady_wind <- gsub(" Km/h", "", weather_hist$max_steady_wind)
        weather_hist$max_steady_wind <- as.integer(weather_hist$max_steady_wind)

        weather_hist$max_wind_gust[weather_hist$max_wind_gust == "N/A"] <- NA
        weather_hist$max_wind_gust <- gsub(" Km/h", "", weather_hist$max_wind_gust)
        weather_hist$max_wind_gust <- as.integer(weather_hist$max_wind_gust)

        ## clean precipitation
        weather_hist$precipitation[weather_hist$precipitation == "N/A"] <- NA
                 weather_hist$precipitation <- gsub("mm", "", weather_hist$precipitation)
                 weather_hist$precipitation <- as.numeric(weather_hist$precipitation)

                 ## clean snow depth
                 weather_hist$snow_depth <- NA

weather_hist$pressure[weather_hist$pressure == "N/A"] <- NA
weather_hist$pressure <- gsub("mb", "", weather_hist$pressure)
weather_hist$pressure <- as.numeric(weather_hist$pressure)

weather_hist$icon <- NULL

weather_hist$description[weather_hist$description == ""] <- NA
weather_hist$description[weather_hist$description == "weather events not reported"] <- NA

head(weather_hist)
tail(weather_hist)

str(weather_hist)

temp <- read.csv("~/Dropbox/freemeteo/hanoi_weather_history.csv",
                 stringsAsFactors = F)

## weather_hist <- subset(weather_hist, date >= as.Date("2017-04-14"))

identical(names(temp), names(weather_hist))
temp[] <- lapply(temp, as.character)
weather_hist[] <- lapply(weather_hist, as.character)
temp <- rbind(temp, weather_hist)

## write.csv(temp, "~/Dropbox/freemeteo/hanoi_weather_history.csv", row.names = F)
