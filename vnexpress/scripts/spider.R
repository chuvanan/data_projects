## A short script to pull data from vnexpress.net
## @anchu
## 22/06/2018


library(xml2)
library(purrr)
library(stringr)
source("helper-functions.R")

## https://vnexpress.net/robots.txt
## Allow: /

## extract daily sitemap-news urls

vnex_sitemap <- "https://vnexpress.net/sitemap/1000000/sitemap.xml"

sm_links <- get_sm_link(vnex_sitemap)

## extract daily headlines, publication dates, images, etc.

dta <- map(sm_links, possibly_parse_sm_link)
dta <- discard(dta, is.null)

## found two corrupted links:
## "https://vnexpress.net/sitemap/1000000/sitemap-news.xml?y=2014&m=11&d=27"
## "https://vnexpress.net/sitemap/1000000/sitemap-news.xml?y=2014&m=11&d=11"

fix_links <- c("https://vnexpress.net/sitemap/1000000/sitemap-news.xml?y=2014&m=11&d=27",
               "https://vnexpress.net/sitemap/1000000/sitemap-news.xml?y=2014&m=11&d=11")

dta_fix <- map(fix_links, function(x) {
    con <- curl::curl(x)
    xml <- readLines(con)
    close(con)

    url_loc <- get_url_loc(xml)
    url_loc <- str_replace_all(url_loc, "<url>", "")
    change_freq <- get_change_freq(xml)
    priority <- get_priority(xml)
    image_loc <- get_image_loc(xml)
    image_title <- get_image_title(xml)
    news_name <- get_news_name(xml)
    news_language <- get_news_language(xml)
    news_publication_date <- get_news_publication_date(xml)
    news_title <- get_news_title(xml)
    news_keywords <- get_news_keywords(xml)
    tibble::data_frame(url_loc, change_freq, image_loc, image_title,
                       news_name, news_language, news_publication_date,
                       news_title, news_keywords)
})

full_dta <- append(dta, dta_fix)
full_dta <- dplyr::bind_rows(full_dta)

## export csv
write.csv(full_dta, "./data/vnex-headlines.csv", row.names = FALSE)
saveRDS(full_dta, "~/Documents/data_projects/vnexpress/data/vnex-headlines.RDS")
