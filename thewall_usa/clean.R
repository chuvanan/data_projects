

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

get_all_fields <- function(dta) {
    name <- get_name(dta)
    service_unit <- get_service_unit(dta)
    background <- get_background(dta)
    birth_date <- get_birth_date(dta)
    length_of_service <- get_length_of_service(dta)
    service_start_date <- get_service_start_date(dta)
    casualty_date <- get_casualty_date(dta)
    body_recovered <- get_body_recovered(dta)
    casualty_location <- get_casualty_location(dta)
    casualty_type <- res <- get_casualty_type(dta)
    panel_line <- get_panel_line(dta)
    birth_place <- get_birth_place(dta)
    rank_rate <- get_rank_rate(dta)

    ## get_casualty_detail is defined on the fly
    pos <- mapply(function(x, y) which(x == y) + 1, dta, casualty_type)
    get_casualty_detail <- lapply(pos, function(x) get_field(pos = x))
    casualty_detail <- lapply(seq_along(dta), function(i) get_casualty_detail[[i]](dta[i]))
    casualty_detail <- unlist(casualty_detail)

    ## combine all fileds
    out <- data.frame(
        name = name,
        service_unit = service_unit,
        background = background,
        birth_date = birth_date,
        length_of_service = length_of_service,
        service_start_date = service_start_date,
        casualty_date = casualty_date,
        body_recovered = body_recovered,
        casualty_location = casualty_location,
        casualty_type = casualty_type,
        panel_line = panel_line,
        birth_place = birth_place,
        rank_rate = rank_rate,
        casualty_detail = casualty_detail,
        stringsAsFactors = FALSE)
    out
}

res <- get_all_fields(thewall)
## write.csv(res, "thewall_usa.csv", row.names = FALSE)
