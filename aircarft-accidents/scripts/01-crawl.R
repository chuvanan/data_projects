## 2019-03-13
## @ancv

library(parallel)
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)

## data sources
baaa_site <- "https://www.baaa-acro.com/crash-archives?created=&created_1=&field_crash_region_target_id=All&field_crash_country_target_id&field_crash_registration_target_id&field_crash_aircraft_target_id&field_crash_operator_target_id&field_crash_cause_target_id=All&field_crash_zone_target_id&field_crash_site_type_target_id=All&field_crash_phase_type_target_id=All&field_crash_flight_type_target_id=All&field_crash_survivors_value=All&field_crash_city_target_id&page="
first_page <- 0
last_page <- 259
baaa_sites <- paste0(baaa_site, first_page:last_page)

## helper function
get_accident_table <- function(x) {
    html <- xml2::read_html(x)
    rvest::html_table(html)[[1]]
}

get_accident_table_possibly <- purrr::possibly(get_accident_table,
                                               otherwise = NULL)
baaa_data <- parLapply(cl, baaa_sites, get_accident_table_possibly)

## check whether the list has NULL
any(sapply(baaa_data, is.null))

sapply(baaa_data, nrow)
sapply(baaa_data, ncol)                 # all have 8 columns

## bind them all
baaa_df <- do.call("rbind", baaa_data)
names(baaa_df) <- c("Image", "Date", "Operator", "AC_Type",
                    "Location", "Fatalities", "Registration", "Details")
baaa_df$Image <- NULL
baaa_df$Details <- NULL

## export
write.csv(baaa_df,
          file = "../data/baaa-aircraft-accidents.csv",
          row.names = FALSE)
