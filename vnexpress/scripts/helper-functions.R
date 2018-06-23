
require(xml2)
require(purrr)
require(stringr)

get_sm_link <- function(x) {
    sm_xml <- x %>%
        read_xml() %>%
        xml_contents()

    sm_nodes <- sm_xml %>%
        map(as.character) %>%
        map(str_split, "\n") %>%
        map(flatten_chr)

    sm_nodes %>%
        map(function(x) keep(x, str_detect(x, "loc"))) %>%
        map_chr(function(x) str_replace_all(x, "\\s|</?loc>", "")) %>%
        keep(function(x) str_detect(x, "sitemap-news")) %>%
        map_chr(function(x) str_replace_all(x, "amp;", ""))
}

get_url_loc <- function(x) {
    out <- keep(x, str_detect(x, "<loc>"))
    str_replace_all(out, "^\\s+|</?loc>", "")
}

get_change_freq <- function(x) {
    out <- keep(x, str_detect(x, "changefreq"))
    str_replace_all(out, "^\\s+|</?changefreq>", "")
}

get_priority <- function(x) {
    out <- keep(x, str_detect(x, "priority"))
    str_replace_all(out, "^\\s+|</?priority>", "")
}

get_image_loc <- function(x) {
    out <- keep(x, str_detect(x, "image:loc"))
    str_replace_all(out, "^\\s+|</?image:loc>", "")
}

get_image_title <- function(x) {
    out <- keep(x, str_detect(x, "image:title"))
    out <- str_replace_all(out, "^\\s+|</?image:title>", "")
    str_replace_all(out, "[<!>\\[\\]]|CDATA", "")
}

get_news_name <- function(x) {
    out <- keep(x, str_detect(x, "news:name"))
    str_replace_all(out, "^\\s+|</?news:name>", "")
}

get_news_language <- function(x) {
    out <- keep(x, str_detect(x, "news:language"))
    str_replace_all(out, "^\\s+|</?news:language>", "")
}

get_news_publication_date <- function(x) {
    out <- keep(x, str_detect(x, "news:publication_date"))
    str_replace_all(out, "^\\s+|</?news:publication_date>", "")
}

get_news_title <- function(x) {
    out <- keep(x, str_detect(x, "news:title"))
    out <- str_replace_all(out, "^\\s+|</?news:title>", "")
    str_replace_all(out, "[<!>\\[\\]]|CDATA", "")
}

get_news_keywords <- function(x) {
    out <- keep(x, str_detect(x, "news:keywords"))
    out <- str_replace_all(out, "^\\s+|</?news:keywords>", "")
    str_replace_all(out, "[<!>\\[\\]]|CDATA", "")
}

get_all_nodes <- function(x) {
    url_loc <- map_chr(x, get_url_loc)
    change_freq <- map_chr(x, get_change_freq)
    priority <- map_chr(x, get_priority)
    image_loc <- map_chr(x, get_image_loc)
    image_title <- map_chr(x, get_image_title)
    news_name <- map_chr(x, get_news_name)
    news_language <- map_chr(x, get_news_language)
    news_publication_date <- map_chr(x, get_news_publication_date)
    news_title <- map_chr(x, get_news_title)
    news_keywords <- map_chr(x, get_news_keywords)
    tibble::data_frame(url_loc, change_freq, image_loc, image_title,
                       news_name, news_language, news_publication_date,
                       news_title, news_keywords)
}

parse_sm_link <- function(x) {
    page_xml <- x %>%
        read_xml() %>%
        xml_contents()
    page_nodes <- page_xml %>%
        map(as.character) %>%
        map(str_split, "\n") %>%
        map(flatten_chr)
    get_all_nodes(page_nodes)
}

possibly_parse_sm_link <- possibly(parse_sm_link, NULL)
