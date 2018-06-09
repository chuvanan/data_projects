
library(dplyr)
library(purrr)

thewall <- readRDS("thewall_usa.rds")

get_field <- function(regx = NULL, pos = NULL, ...) {
    function(x) {
        fun <- function(s) {
            s <- s[grepl(regx, s, ...)]
            s <- if (length(s) == 0L) NA else s
            s
        }
        fun <- if (is.null(pos)) fun else function(v) v[[pos]]
        res <- lapply(x, fun)
        unlist(res)
    }
}

get_name <- get_field(pos = 1L)
get_service_unit <- get_field(regx = "(Regular|Reserve|Service|Temporary|National Guard)$")
get_birth_date <- get_field(regx = "^Born on")
get_length_of_service <- get_field(regx = "Length of service")
get_service_start_date <- get_field(regx = "tour began")
get_casualty_date <- get_field(regx = "Casualty was on")
get_body_recovered <- get_field(regx = "(Body).*(recovered)")
get_casualty_location <- get_field(regx = "^In.*(VIETNAM|LAOS|CAMBODIA)")
get_casualty_type <- get_field(regx = "^(non-hostile)|(hostile)", ignore.case = TRUE)
get_panel_line <- get_field(regx = "^Panel")
get_birth_place <- get_field(regx = "^From")
get_rank_rate <- get_field(regx = "^([1-9]+(st|nd|rd|th))")
get_background <- get_field(regx = "year old")

sample(thewall, 10)

get_name(thewall)
get_service_unit(thewall)
get_background(thewall)
get_birth_date(thewall)
get_length_of_service(thewall)
get_service_start_date(thewall)
get_casualty_date(thewall)
get_body_recovered(thewall)
get_casualty_location(thewall)
get_casualty_type(thewall)
## get_casualty_detail(thewall)
get_panel_line(thewall)
get_birth_place(thewall)
get_rank_rate(thewall)
