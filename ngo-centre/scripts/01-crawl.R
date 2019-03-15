

ngocentre <- "https://www.ngocentre.org.vn/jobs?page="
first_page <- 0
last_page <- 1581
ngocentre_pages <- paste0(ngocentre, first_page:last_page)

get_title <- function(x) {
    out <- rvest::html_nodes(x, xpath = "//h2[@class='title']")
    out <- rvest::html_text(out)
    out <- out[!out %in% c("Browse Jobs", "Job Archive")]
    out <- vapply(out, function(.x) gsub("\n", "", .x), "", USE.NAMES = FALSE)
    out
}

get_job_details <- function(x) {
    out <- rvest::html_nodes(x, xpath = "//fieldset[@class='fieldgroup group-jobs']")
    out <- rvest::html_text(out)
    out <- vapply(out, function(.x) gsub("\n", " ", .x), "", USE.NAMES = FALSE)
    out <- vapply(out, function(.x) gsub("\\s{1,}", " ", .x), "", USE.NAMES = FALSE)
    out <- vapply(out, function(.x) gsub(" ", "", .x), "", USE.NAMES = FALSE)
    out <- trimws(out)
    out
}

crawl_job_listings <- function(site) {
    html <- xml2::read_html(site)
    data.frame(
        title = get_title(html),
        job_details = get_job_details(html),
        stringsAsFactors = FALSE
    )
}

crawl_job_listings_possibly <- purrr::possibly(crawl_job_listings,
                                               otherwise = NULL)

## demo
crawl_job_listings_possibly(ngocentre_pages[1])
