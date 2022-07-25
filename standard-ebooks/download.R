


require(rvest)
require(purrr)
require(stringr)
require(glue)

base_url = "https://standardebooks.org"
fiction_url = "https://standardebooks.org/ebooks?page={page}&tags[]=fiction"
fiction_folder = "~/Downloads/epub/standard-ebooks-project/fiction/"


get_filename = function(x) {
    lst = str_split(x, pattern = "/")[[1]]
    return(lst[length(lst)])
}

build_subject_url = function(url, n_pages) {
    if (length(n_pages) == 1L) {
        subject_urls = glue(url, page = seq_len(n_pages))
    } else {
        subject_urls = glue(url, page = n_pages)
    }
    return(subject_urls)
}

build_page_url = function(url) {
    pages = url |>
        read_html() |>
        html_elements(xpath = "//a[@property='schema:url']") |>
        html_attr(name = "href") |>
        keep(.p = function(x) grepl(pattern = "^/ebook", x)) |>
        unique()
    pages = str_c(base_url, pages)
    return(pages)
}


download_azw = function(url, save_to) {
    azw = url |>
        read_html() |>
        html_elements(xpath = "//a[@class='amazon']") |>
        html_attr(name = "href")
    azw = str_c(base_url, azw)
    save_as = str_c(save_to, get_filename(azw))
    try(download.file(url = azw, destfile = save_as))
}

pull_ebooks = function(subject, n_pages, save_to) {
    subject_urls = build_subject_url(url = subject, n_pages = n_pages)
    N = length(subject_urls)

    for (i in seq_len(N)) {
        subject_links = build_page_url(subject_urls[i])
        n_links = length(subject_links)
        for (k in seq_len(n_links)) {
            download_azw(url = subject_links[k], save_to = save_to)
            Sys.sleep(1L)
        }
    }
}

pull_ebooks(subject = fiction_url, n_pages = 2:10, save_to = fiction_folder)




