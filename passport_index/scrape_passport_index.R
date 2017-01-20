

library(rvest)
library(stringr)

pp_url <- "https://www.passportindex.org/byIndividualRank.php?ccode=vn"

passport <- read_html(pp_url)

countries <- passport %>%
  html_nodes(xpath = "//div[@class='col-lg-3 col-md-3 col-sm-3 col-xs-8 name_country']") %>%
  html_text()

scores <- passport %>%
  html_nodes(xpath = "//div//progress") %>%
  html_attr(name = "value")

vfs <- scores[seq(1, 398, 2)]
voa <- scores[seq(2, 398, 2)]

hdi <- passport %>%
  html_nodes(xpath = "//span[@class='progress_pop']") %>%
  html_text() %>%
  str_extract(pattern = "0\\.[0-9]+")
  
passport_idx <- data.frame(
  countries = countries,
  vfs = as.integer(vfs),
  voa = as.integer(voa),
  hdi = as.double(hdi),
  stringsAsFactors = FALSE
)

write.csv(passport_idx, "~/Documents/data_projects/passport_index/passport_index.csv",
          row.names = FALSE)
