## https://www.regular-expressions.info/rlanguage.html

load("../data/jobs-data-demo.RData")

str(jobs_data)

if (anyDuplicated(jobs_data)) {
    jobs_data <- jobs_data[!duplicated(jobs_data), ]
}

## clean job details

get_character_position <- function(pattern, text) {
    start <- regexpr(pattern = pattern, text = text)
    len <- attr(start, "match.length")
    attr(start, "match.length") <- NULL
    list(start = start, len = len)
}

get_text_index <- function(txt) {
    org_idx <- get_character_position(pattern = "Organisation Name: ", text = txt)
    loc_idx <- get_character_position(pattern = " Location: ", text = txt)
    apd_idx <- get_character_position(pattern = " Application Deadline: ", text = txt)
    loc_idx$start[loc_idx$start < 0] <- apd_idx$start[loc_idx$start < 0] + 1
    list(org_idx = org_idx, loc_idx = loc_idx, apd_idx = apd_idx)
}

get_organization_name <- function(txt) {
    txt_idx <- get_text_index(txt)
    out <- substring(txt, first = (txt_idx$org_idx$start + txt_idx$org_idx$len),
                     last = txt_idx$loc_idx$start - 1)
    trimws(out)
}

get_location <- function(txt) {
    txt_idx <- get_text_index(txt)
    out <- substring(txt, first = (txt_idx$loc_idx$start + txt_idx$loc_idx$len),
                     last = txt_idx$apd_idx$start - 1)
    trimws(out)
}

get_application_deadline <- function(txt) {
    txt_idx <- get_text_index(txt)
    out <- substring(txt, first = (txt_idx$apd_idx$start + txt_idx$apd_idx$len))
    trimws(out)
}

jobs_data$org_name <- get_organization_name(jobs_data$job_details)
jobs_data$location <- get_location(jobs_data$job_details)
jobs_data$deadline <- get_application_deadline(jobs_data$job_details)
