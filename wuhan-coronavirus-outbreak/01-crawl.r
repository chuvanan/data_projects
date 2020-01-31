

require(rvest) # for reading html page

wiki_page <- "https://en.wikipedia.org/wiki/2019%E2%80%9320_Wuhan_coronavirus_outbreak"
page_parsed <- read_html(wiki_page)
outbreak <- html_table(page_parsed, fill = TRUE)[[4]]

## look at data
head(outbreak)
tail(outbreak)

## rename columns
names(outbreak) <- c("country", "confirmed_cases", "deaths", "reference")

## remove rows
outbreak <- outbreak[!grepl(pattern = "territories|As of", outbreak$country), ]

## clean data
outbreak$confirmed_cases <- gsub(pattern = "\\[[0-9]+\\]|,", replacement = "", outbreak$confirmed_cases)
outbreak$confirmed_cases <- as.numeric(outbreak$confirmed_cases)
outbreak$deaths <- as.numeric(outbreak$deaths)
outbreak$country[outbreak$country == "China (mainland)"] <- "China"
outbreak <- outbreak[order(outbreak$confirmed_cases), ]

## compare countries
png(filename = "./wuhan-virus-outbreak.png", 7, 7, units = "in", res = 300)
par(mar = c(3, 9.5, 2, 2), family = "Nunito")
barplot(outbreak$confirmed_cases, names.arg = outbreak$country, horiz = TRUE, col = "goldenrod", border = "goldenrod", las = 1,
        main = "Wuhan CoronaVirus Outbreak by Territory")
barplot(height = c(rep(NA, NROW(outbreak) - 1), max(outbreak$deaths)), col = "red", add = TRUE, horiz = TRUE, border = "red", axes = FALSE)
abline(v = seq(2e3, 8e3, 2e3), col = "white", lwd = 2)
legend("center", legend = c("confirmed cases", "deaths"), col = c("goldenrod", "red"), pch = 15, bty = "n")
dev.off()
