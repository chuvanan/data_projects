
library(rvest)


base_url <- "https://www.flightradar24.com/data/airports"

airports <- read_html(base_url)

airports <- html_table(airports, fill = TRUE)

airports <- airports[[1]]

airports <- airports[, c(3, 4)]

names(airports) <- c("country", "n")

airports$n <- gsub("[ a-z]", "", airports$n)

airports <- airports[nchar(airports$country) > 1, ]

airports$n <- as.integer(airports$n)

airports <- airports[order(airports$n), ]

dev.size("px")

png("airports.png", width = 600, height = 2000, res = 200)

par(mar = c(2, 5.5, 2, 1))
barplot(height = tail(airports$n, 30),
        names.arg = tail(airports$country, 30),
        horiz = TRUE,
        las = 1,
        cex.names = 0.65,
        border = FALSE)
grid(NULL, NA, col = "ivory4", lwd = 1.2)

dev.off()
