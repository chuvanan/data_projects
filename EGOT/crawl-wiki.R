

library(tidyr)
library(dplyr)
library(rvest)
library(purrr)

wiki_url <- "https://en.wikipedia.org/wiki/List_of_people_who_have_won_Academy,_Emmy,_Grammy,_and_Tony_Awards"
wiki_page <- read_html(wiki_url)

wiki_table <- wiki_page %>%
    html_table(fill = TRUE) %>%
    .[[1]]

## better column names
names(wiki_table) <- c("name", "year_span", "emmy", "grammy", "oscar", "tony",
                       "completion_year", "age", "categoty")

## fix subscript
wiki_table[] <- map_at(wiki_table,
                       c("emmy", "grammy", "oscar", "tony", "completion_year"),
                       .f = ~ as.numeric(substr(.x, 1, 4)))
wiki_table$age <- gsub("\\syears[0-9]?", "", wiki_table$age)
wiki_table$age <- as.numeric(wiki_table$age)

wiki_table %>%
    select(winner = name, emmy, grammy, oscar, tony) %>%
    gather(prize, year, -winner) %>%
    readr::write_csv("EGOT-winners.csv")
