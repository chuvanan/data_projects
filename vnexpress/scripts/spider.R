##



library(xml2)
library(purrr)
library(stringr)

## -----------------------------------------------------------------------------
## extract daily sitemap-news urls

vnex_sitemap <- "https://vnexpress.net/sitemap/1000000/sitemap.xml"

sm_xml <- vnex_sitemap %>%
    read_xml() %>%
    xml_contents()

sm_node <- sm_xml %>%
    map(as.character) %>%
    map(str_split, "\n") %>%
    map(flatten_chr)

sm_link <- sm_node %>%
    map(function(x) keep(x, str_detect(x, "loc"))) %>%
    map_chr(function(x) str_replace_all(x, "\\s|</?loc>", "")) %>%
    keep(function(x) str_detect(x, "sitemap-news"))

## -----------------------------------------------------------------------------
## extract daily headlines, publication dates, images, etc.
