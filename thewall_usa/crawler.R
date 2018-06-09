

library(rvest)
library(purrr)
library(stringr)

n_pages <- 58223
base_url <- "http://thewall-usa.com/info.asp?recid="
urls <- paste0(base_url, 1:n_pages)

crawler <- function(.url) {

  # read html
  page <- read_html(.url)
  block <- page %>% html_nodes(xpath = "//td[@class='BlackText']")

  # keep nodesets that contains information
  block <- block[c(1, 3)]

  veteran_name <- block[1] %>%
    html_text() %>%
    str_replace_all("[\r\n\t]", "") %>%
    str_trim(side = "both")

  veteran_profile <- block[[2]] %>%
    as.character() %>%
    str_split("<br>") %>%
    flatten_chr() %>%
    map_chr(str_replace_all, pattern = "[\r\n\t]|(<!--|-->)|(<.*>)", replacement = "") %>%
    map_chr(str_trim, side = "both") %>%
    keep(~ .x != "")

  c(veteran_name, veteran_profile)

}

vnm_memorial <- vector("list", length = 3)

for (i in seq_along(vnm_memorial)) {
  vnm_memorial[[i]] <- crawler(urls[i])
  Sys.sleep(2)
}


fun <- function() {
    print("hello")
}
