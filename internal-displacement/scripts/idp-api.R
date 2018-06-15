
library(dplyr)
library(purrr)

## helper function

get_displacement_data <- function(country) {

    ci_key <- 'HDX00AKEYJUl17'
    base_url <- 'https://api.idmcdb.org/api/displacement_data?'
    api <- paste0(base_url, 'iso3=', country, '&ci=', ci_key)

    res <- httr::GET(api)
    httr::stop_for_status(res)

    dta <- httr::content(res)$result
    dta <- do.call('rbind', dta)
    as.data.frame(dta, stringsAsFactors = FALSE)
}

## pull data

cc_iso3 <- readr::read_csv('~/country-code-iso3.csv')

cc_iso3 <- as.list(cc_iso3$cc_iso3)

possibly_get_displacement_data <- possibly(get_displacement_data, otherwise = NULL)

ip_data <- map(cc_iso3, possibly_get_displacement_data)

ip_data <- discard(ip_data, is.null)

str(ip_data)

test <- do.call(bind_rows, ip_data)

count(test, iso)
