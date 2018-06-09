
library(dplyr)
library(purrr)

thewall <- readRDS("thewall_usa.rds")

## full name
## army unit
## background
## origin
## length of service
## service start date
## casualty date
## casualty place
## casualty type
## casualty detail
## body recovered
## panel line

get_name <- function(x) {
    purrr::map_chr(x, 1L)
}

get_army_unit <- function(x) {
    fun <- function(s) {
        s <- s[grepl("(Regular)$|(Service)$|(Reserve)$", s)]
        s <- if (length(s) == 0L) NA else s
        s
    }
    purrr::map_chr(x, fun)
}

get_background <- function(x) {
    fun <- function(s) {
        s <- s[grepl("year old", s)]
        s <- if (length(s) == 0L) NA else s
        s
    }
    purrr::map_chr(x, fun)
}

get_dob <- function(x) {
    fun <- function(s) {
        s <- s[grepl("^Born on", s)]
        s <- if (length(s) == 0L) NA else s
        s
    }
    purrr::map_chr(x, fun)
}

get_length_of_service <- function(x) {
    fun <- function(s) {
        s <- s[grepl("Length of service", s)]
        s <- if (length(s) == 0L) NA else s
        s
    }
    purrr::map_chr(x, fun)
}

get_service_start_date <- function(x) {
    fun <- function(s) {
        s <- s[grepl("tour began", s)]
        s <- if (length(s) == 0L) NA else s
        s
    }
    purrr::map_chr(x, fun)
}

get_casualty_date <- function(x) {
    fun <- function(s) {
        s <- s[grepl("Casualty was on", s)]
        s[grepl("Length of service", s)]
        s
    }
    purrr::map_chr(x, fun)
}

get_body_recovered <- function(x) {
    fun <- function(s) {
        s <- s[grepl("(Body).*(recovered)", s)]
        s <- if (length(s) == 0L) NA else s
        s
    }
    purrr::map_chr(x, fun)
}

get_casualty_location <- function(x) {
    fun <- function(s) {
        s <- s[grepl("^In.*(VIETNAM|LAOS|CAMBODIA)", s)]
        s <- if (length(s) == 0L) NA else s
        s
    }
    purrr::map_chr(x, fun)
}

get_casualty_type <- function(x) {
    fun <- function(s) {
        s <- s[grepl("^(non-hostile)|(hostile)", s, ignore.case = TRUE)]
        s <- if (length(s) == 0L) NA else s
        s
    }
    purrr::map_chr(x, fun)
}

get_casualty_detail <- function(x) {
    fun <- function() NULL
}

get_panel_line <- function(x) {
    fun <- function(s) {
        s <- s[grepl("^Panel", s)]
        s <- if (length(s) == 0L) NA else s
        s
    }
    purrr::map_chr(x, fun)
}

get_birth_place <- function(x) {
    fun <- function(s) {
        s <- s[grepl("^From", s, ignore.case = TRUE)]
        s <- if (length(s) == 0L) NA else s
        s
    }
    purrr::map_chr(x, fun)
}

get_division <- function(x) {
    fun <- function(s) {
        s <- s[grepl("^([1-9]+(st|nd|rd|th))", s)]
        s <- if (length(s) == 0L) NA else s
        s
    }
    purrr::map_chr(x, fun)
}


sample(thewall, 10)

get_name(thewall)
get_army_unit(thewall)
get_background(thewall)
get_dob(thewall)
get_length_of_service(thewall)
get_service_start_date(thewall)
get_casualty_date(thewall)
get_body_recovered(thewall)
get_casualty_location(thewall)
get_casualty_type(thewall)
## get_casualty_detail(thewall)
get_panel_line(thewall)
get_birth_place(thewall)
get_division(thewall)
