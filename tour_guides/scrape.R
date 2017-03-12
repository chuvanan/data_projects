

library(rvest)
library(stringr)


baseurl <- "http://2016.huongdanvien.vn/index.php/guide/cat/05/"
n_page <- 370
pages <- paste0(baseurl, 1:370)

download_hdvdl_profile <- function(urls) {
  require(rvest)
  require(stringr)
  out <- character(0)
  for (i in urls) {
    site <- read_html(i)
    site <- html_nodes(site, xpath = "//div[@class='guide-info']")
    site <- html_text(site)
    site <- str_replace_all(site, "[\r\n\t]", "")
    site <- trimws(site, which = "both")
    out <- c(out, site)
  }
  out
}

ds_hdvl <- download_hdvdl_profile(pages)


