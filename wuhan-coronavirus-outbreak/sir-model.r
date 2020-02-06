

## References:
## * https://blog.ephorie.de/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov
## * https://www.maa.org/press/periodicals/loci/joma/the-sir-model-for-spread-of-disease-the-differential-equation-model

## Data source:
## * https://en.wikipedia.org/wiki/Timeline_of_the_2019%E2%80%9320_Wuhan_coronavirus_outbreak#Case_statistics


## -----------------------------------------------------------------------------
## Scrap case statistics
## -----------------------------------------------------------------------------


require(rvest)

page <- read_html("https://en.wikipedia.org/wiki/Timeline_of_the_2019%E2%80%9320_Wuhan_coronavirus_outbreak#Case_statistics")
wuhan_stats <-  html_table(page, fill = TRUE)[[3]]

wuhan_stats <- wuhan_stats[, c(1, 2, 3, 6, 7)]
names(wuhan_stats) <- c("date", "suspected", "confirmed", "deaths", "recovered")

wuhan_stats <- wuhan_stats[!grepl("^Notes", wuhan_stats$date), ]
wuhan_stats <- wuhan_stats[!grepl("^Date", wuhan_stats$date), ]

wuhan_stats$date <- as.Date(wuhan_stats$date)
stats_cols <- setdiff(names(wuhan_stats), "date")
wuhan_stats[, stats_cols] <- lapply(wuhan_stats[, stats_cols], function(x) replace(x, x == "", NA))
wuhan_stats[, stats_cols] <- lapply(wuhan_stats[, stats_cols], function(x) as.numeric(gsub(",", "", x)))


## 2019-nCoV outbreak

par(mfrow = c(1, 2), family = "Roboto Condensed")
plot(wuhan_stats$date[wuhan_stats$date >= as.Date("2020-01-16")],
     wuhan_stats$confirmed[wuhan_stats$date >= as.Date("2020-01-16")],
     type = "b", , xlab = "date", ylab = "infected", col = "red4", pch = 19)
plot(wuhan_stats$date[wuhan_stats$date >= as.Date("2020-01-16")],
     wuhan_stats$confirmed[wuhan_stats$date >= as.Date("2020-01-16")],
     log = "y", xlab = "date", ylab = "infected", col = "red4", pch = 19)
abline(lm(log10(confirmed) ~ date, data = wuhan_stats))
title("Confirmed cases 2019-nCoV in mainland China", outer = TRUE, line = -2)

## -----------------------------------------------------------------------------
## SIR modeling
## -----------------------------------------------------------------------------
