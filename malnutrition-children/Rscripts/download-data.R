## A simple script to download malnutrition data from www.mard.gov.vn
## @anchu

library(XML)

source.url <- "http://fsiu.mard.gov.vn/data/dinhduong.htm"

html <- htmlTreeParse(source.url, useInternalNodes = TRUE)

links <- unlist(xpathApply(html, "//a/@href"))

links <- paste0("http://fsiu.mard.gov.vn/data/", links)

path <- "~/Documents/malnutrition-children/data/"

for (i in seq_along(links)) {

    download.file(links[i], destfile = paste0(path, basename(links[i])))

}
