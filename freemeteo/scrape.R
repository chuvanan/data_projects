## Scraping data from freemeteo.vn
## 18/04/2017
## anchu@rta.vn

build_url_query <- function(m, y) {
    base_url <- "https://freemeteo.vn/weather/hanoi/history/monthly-history/?gid=1581130&station=11376&month="
    url_query <- paste0(base_url, m, "&year=", y, "&language=english&country=vietnam")
    url_query
}

pull_weather_table <- function(x) {
    out <- xml2::read_html(x)
    out <- rvest::html_table(out)
    out[[6L]]
}

clean_weather_table <- function(x) {

    out <- x
    names(out) <- c("date", "min_temp", "max_temp", "max_steady_wind", "max_wind_gust",
                    "precipitation", "snow_depth", "pressure", "icon", "description")

    ## reformat dates
    out$date <- as.Date(out$date, "%m/%d/%Y")

    ## clean temperature
    out$min_temp[out$min_temp == "N/A"] <- NA
    out$min_temp <- gsub("°C", "", out$min_temp)
    out$min_temp <- as.integer(out$min_temp)

    out$max_temp[out$max_temp == "N/A"] <- NA
    out$max_temp <- gsub("°C", "", out$max_temp)
    out$max_temp <- as.integer(out$max_temp)

    ## clean wind
    out$max_steady_wind[out$max_steady_wind == "N/A"] <- NA
    out$max_steady_wind <- gsub(" Km/h", "", out$max_steady_wind)
    out$max_steady_wind <- as.integer(out$max_steady_wind)

    out$max_wind_gust[out$max_wind_gust == "N/A"] <- NA
    out$max_wind_gust <- gsub(" Km/h", "", out$max_wind_gust)
    out$max_wind_gust <- as.integer(out$max_wind_gust)

    ## clean precipitation
    out$precipitation[out$precipitation == "N/A"] <- NA
    out$precipitation <- gsub("mm", "", out$precipitation)
    out$precipitation <- as.numeric(out$precipitation)

    ## clean snow depth
    out$snow_depth <- NA

    out$pressure[out$pressure == "N/A"] <- NA
    out$pressure <- gsub("mb", "", out$pressure)
    out$pressure <- as.numeric(out$pressure)

    out$icon <- NULL

    out$description[out$description == ""] <- NA
    out$description[out$description == "weather events not reported"] <- NA

    return(out)
}

## Example:
page_test <- build_url_query(m = 12, y = 2017)
page_table <- pull_weather_table(page_test)
page_result <- clean_weather_table(page_table)
