
## Meetup.com meetups listing "R Project for Statistical Computing"
## Feb 26, 2017


library(rvest)
library(dplyr)
library(ggmap)

## crawl html + data -----------------------------------------------------------
url <- "https://www.meetup.com/topics/r-project-for-statistical-computing/all/"

page <- read_html(url)

groups <- page %>%
  html_nodes(xpath = "//span[@class='text--bold display--block']") %>%
  html_text()

group_info <- page %>%
  html_nodes(xpath = "//span[@class='text--secondary text--small chunk']") %>%
  html_text()

## clean data ------------------------------------------------------------------
group_info <- gsub("(\n|\t)+", "", group_info)

n_members <- sapply(strsplit(group_info, split = "|", fixed = T), function(x) x[1])
n_members <- gsub(",", "", n_members)
n_members <- stringr::str_extract(n_members, "[[:digit:]]+")
n_members <- as.numeric(n_members)

locations <- sapply(strsplit(group_info, split = "|", fixed = T), function(x) x[2])
cities <- sapply(strsplit(locations, split = ","), function(x) x[1])
states <- sapply(strsplit(locations, split = ","), function(x) x[2])
cities <- trimws(cities, which = "both")
states <- trimws(states, which = "both")

## finalise data ---------------------------------------------------------------

meetups <- data.frame(groups, n_members, locations, cities, states,
                      stringsAsFactors = F)
## meetups <- mutate_geocode(meetups, location = locations) # long run

## export data -----------------------------------------------------------------

write.csv(meetups, "~/Dropbox/Rmeetups.csv", row.names = F)

