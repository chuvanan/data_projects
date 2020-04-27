


require(rvest)


read_springer_publication = function() {
    springer_search_result = "SearchResults.csv"
    stopifnot("`SearchResults.csv` not found." = file.exists(springer_search_result))
    springer_books = read.csv(springer_search_result)
    names(springer_books) = gsub(pattern = r"(\.)", replacement = "_", tolower(names(springer_books)))
    springer_books
}



##' @param pubs [data.frame] A table of Springer's free publications containing download links
##' @param save_to [character] Where to save books to be downloaded
scrap_springer_book = function(pubs, save_to) {

    stopifnot("`pubs` is not a data.frame" = is.data.frame(pubs))
    stopifnot("`save_to` is not a directory" = dir.exists(save_to))

    BASE_URL = "https://link.springer.com"
    N = NROW(pubs)

    ## create names for files to be downloaded
    pubs$save_as = paste0(pubs$item_title, " - ", pubs$publication_year, ".pdf")

    for (i in seq_len(N)) {
        page = read_html(pubs$url[i])
        atags = xml_find_all(page, "//a")
        hrefs = xml_attr(atags, "href")
        pdf = Filter(function(s) grepl(pattern = ".pdf$", s), hrefs)[1L]
        link = paste0(BASE_URL, pdf)
        tryCatch(
            download.file(url = link, destfile = file.path(save_to, pubs$save_as[i])),
            error = function(e) {
                message(sprintf("[DEBUG] Error while downloading file: %s", link))
                message(e)
            }
        )
        Sys.sleep(2L) # Be polite
    }
}

springer_book_list = read_springer_publication()
scrap_springer_book(springer_book_list[6:7, ], save_to = "~/Downloads/demo")
