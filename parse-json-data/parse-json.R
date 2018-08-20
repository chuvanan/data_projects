

## required packages:
## install.packages("stringi")
## install.packages("stringr")
## install.packages("jsonlite")
## install.packages("dplyr")

parse_malformed_json <- function(x) {

    clean <- function(x) {
        out <- stringi::stri_replace_all_fixed(x, "\t", "")
        out <- stringi::stri_replace_all_fixed(out, "\\r", "")
        out <- stringi::stri_replace_all_fixed(out, "\\n", "")
        out
    }

    con <- file(x, "r")
    json_text <- readLines(con, encoding = "UTF-8")
    close(con)
    json_text <- clean(json_text)
    breaks <- which(stringr::str_detect(json_text, "_id"))

    start_breaks <- breaks - 1
    end_breaks <- c(breaks[2:length(breaks)] - 2, length(json_text))
    stopifnot(length(start_breaks) == length(end_breaks))

    n_loop <- length(start_breaks)
    out <- vector("list", length = n_loop)

    for (i in seq_len(n_loop)) {
        out[[i]] <- tryCatch(
            jsonlite::fromJSON(json_text[start_breaks[i]:end_breaks[i]]),
            error = function(e) {
                NULL
            }
        )
    }
    out
}


bind_dtfs <- function(x) {

    all_cols <- Reduce(union, lapply(x, names))

    out <- lapply(x, function(y) {
        ## fill in non-overlapping columns with NA
        y[setdiff(all_cols, names(y))] <- NA

        ## re-order columns
        y[all_cols]
    })

    do.call('rbind', out)
}


output <- parse_malformed_json("~/Downloads/hcm.json")
output <- Filter(function(x) length(x) > 0, output)

dta <- bind_dtfs(output)
dta <- as.data.frame(dta, stringsAsFactors = FALSE)

dta$`_id` <- unlist(dta$`_id`)
dta$domain <- unlist(dta$domain)

dta$other_properties_1 <- sapply(dta$other_properties, function(x) {
    if (any(names(x) == "Hướng nhà")) {
        return(x[["Hướng nhà"]])
    } else {
        return(NA)
    }
})

dta$other_properties_2 <- sapply(dta$other_properties, function(x) {
    if (any(names(x) == "Hướng ban công")) {
        return(x[["Hướng ban công"]])
    } else {
        return(NA)
    }
})
dta$other_properties <- NULL

dta$contact_address <- unlist(dta$contact_address)
dta$district <- unlist(dta$district)
dta$title <- unlist(dta$title)
dta$geo_lat <- unlist(dta$geo_lat)
dta$content <- unlist(dta$content)
dta$other_contact_info <- NA
dta$geo_long <- unlist(dta$geo_long)
dta$type <- unlist(dta$type)
dta$image <- unlist(dta$image)

dta$submission_date <- unlist(dta$submission_date)
dta$timestampISODate <- unlist(dta$timestampISODate)
dta$end_date <- unlist(dta$end_date)
dta$timestamp <- unlist(dta$timestamp)
dta$price <- unlist(dta$price)
dta$contact_email <- unlist(dta$contact_email)
dta$address <- unlist(dta$address)
dta$street_width <- unlist(dta$street_width)
dta$contact_mobile_phone <- unlist(dta$contact_mobile_phone)
dta$url <- unlist(dta$url)
dta$contact_phone <- unlist(dta$contact_phone)
dta$identity_number <- unlist(dta$identity_number)
dta$contact <- unlist(dta$contact)
dta$surface_width <- unlist(dta$surface_width)
dta$number_of_floors <- unlist(dta$number_of_floors)
dta$surface_size <- unlist(dta$surface_size)
dta$number_of_rooms <- unlist(dta$number_of_rooms)
dta$number_of_toilets <- unlist(dta$number_of_toilets)
dta$furniture <- unlist(dta$furniture)
dta$project_gabarit <- unlist(dta$project_gabarit)
dta$project_investor <- unlist(dta$project_investor)
dta$of_project <- unlist(dta$of_project)
