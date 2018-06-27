
require(purrr)

normalize_colnames <- function(x) {
    raw_names <- names(x)
    nice_names <- raw_names %>%
        map_chr(function(x) gsub("-", " ", x)) %>%
        map_chr(RTA::capwords) %>%
        map_chr(function(x) gsub(" ", "", x))
    names(x) <- nice_names
    x
}
