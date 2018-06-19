

set_ci <- function(ci = NULL) {
    stopifnot(!is.null(ci))
    options(idmc.url = 'https://api.idmcdb.org/api/')
    options(idmc.ci = ci)
}


## Returns an array of countries in the database
get_countries <- function(sort = c('name', 'iso3')) {

    ## build API query
    base_url <- getOption('idmc.url')
    ci <- getOption('idmc.ci')
    sort <- match.arg(sort)
    api <- paste0(base_url, 'countries?sort=', sort, '&ci=', ci)

    ## GET
    res <- httr::GET(api)
    httr::stop_for_status(res)

    dta <- httr::content(res, as = 'parsed')$result
    dta <- do.call('rbind', dta)
    dta <- as.data.frame(dta, stringsAsFactors = FALSE)
    dta[] <- lapply(dta, unlist)
    dta

}



get_country_overview <- function(iso3) {
    ## Returns a country displacement information and includes population and
    ## refugee numbers


}

get_confict_data <- function(iso3, year, range, limit) {
    ## Returns all available conflict displacement data

    base_url <- 'https://api.idmcdb.org/api/'


}


get_disaster_data <- function(iso3, year, range, limit) {
    ## Returns all available disaster displacement data by event and includes hazard type information

    base_url <- 'https://api.idmcdb.org/api/'

}

get_aggregated_disaster_data <- function(iso3, year, range, limit) {
    ## Returns all available disaster displacement data by country aggregating
    ## all the events

    base_url <- 'https://api.idmcdb.org/api/'


}

## Returns all available displacement data for both disaster and conflict by
## year and country
get_displacement_data <- function(iso3, year, range, limit) {

    if (missing(iso3)) iso3 <- ''

    if (missing(range)) range <- ''

    if (missing(year)) {
        year <- ''
    } else {

    }

    if (missing(limit)) limit <- ''

    ## build API query
    base_url <- getOption('idmc.url')
    ci <- getOption('idmc.ci')
    api <- paste0(base_url, 'displacement_data?iso3=', iso3, '&year=', year,
                  '&range=', range, '&limit=', limit, '&ci=', ci)

    ## GET
    res <- httr::GET(api)
    httr::stop_for_status(res)

    dta <- httr::content(res)$result
    dta <- do.call('rbind', dta)
    dta <- as.data.frame(dta, stringsAsFactors = FALSE)
    dta[] <- lapply(dta, unlist)
    dta

}


get_strata_data <- function(iso3) {
    ## Returns the strata data for a country by type of displacement and fact
    ## type

    base_url <- 'https://api.idmcdb.org/api/'

}

get_figure_analysis <- function(iso3) {
    ## Returns the figure analysis for a country

    base_url <- 'https://api.idmcdb.org/api/'

}

get_confidence_assessment <- function(iso3) {
    ## Returns the confidence assessment data for a country by type of
    ## displacement and fact type

    base_url <- 'https://api.idmcdb.org/api/'

}

get_disaster_events <- function(iso3) {
    ## Returns events associated with an iso code

    base_url <- 'https://api.idmcdb.org/api/'

}
