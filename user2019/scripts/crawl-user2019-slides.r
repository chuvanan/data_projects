## @ancva
## 07/2019


## Load packages ---------------------------------------------------------------

conflictRules("purrr", mask.ok = "pluck") # for R-3.6.x
require(rvest)
require(purrr)

## Main functions --------------------------------------------------------------

get_talk_schedule <- function(page) {

    ## select nodes that have schedule information
    schedule <- html_nodes(page, xpath = "//tr[@class='filtered']") %>%
        html_text()

    ## clean texts
    schedule <- map(schedule, ~ strsplit(.x, split = "\n")) %>%
        map( ~ unlist(.x)) %>%
        map( ~ trimws(.x)) %>%
        map( ~ keep(.x, function(x) x != ""))

    ## turn them into a data frame
    schedule <- as.data.frame(do.call("rbind", schedule), stringsAsFactors = FALSE)
    names(schedule) <- c("time", "session", "speaker", "title", "chair", "room")

    schedule
}

get_talk_slide <- function(page) {

    ## extract all children from the schedule information nodes
    slide <- html_nodes(page, xpath = "//table[@id='schedule20190710']") %>%
        html_nodes(xpath = "//td[@style='text-align: center']/child::a") %>%
        html_attr("href")


    ## make static files as full link address
    slide <- ifelse(startsWith(slide, "/static"),
                    yes = paste0("http://user2019.r-project.org", slide),
                    no = slide)

    slide
}

download_talk_slide <- function(path, talk_schedule, talk_slide) {

    ## sanity check before downloading files
    is_dir <- dir.exists(path)
    if (!is_dir) stop("`path` does not exist.", call. = FALSE)

    ## keep .pdf or .zip files only
    talks <- cbind(talk_schedule, talk_slide, stringsAsFactors = FALSE)
    talks <- talks[grepl(pattern = "\\.(pdf|zip)$", talks$talk_slide), ]

    n <- nrow(talks)
    for (i in seq_len(n)) {

        file_ext <- ifelse(grepl("\\.pdf$", talks$talk_slide[i]), ".pdf", ".zip")
        save_as <- paste0(talks$session[i], "-", talks$title[i], file_ext)

        download.file(url = talks$talk_slide[i], destfile = file.path(path, save_as))
        Sys.sleep(1)                    # be polite
    }

}

## Pull slides to local computer -----------------------------------------------

user2019_homepage <- "http://user2019.r-project.org/talk_schedule/"
page <- read_html(x = user2019_homepage)

talk_schedule <- get_talk_schedule(page)
talk_slide <- get_talk_slide(page)

## test
dir.create("~/R/demo-download/")
download_talk_slide(path = "~/R/demo-download/",
                    talk_schedule[3:4, ],
                    talk_slide[3:4])
