
library(XML)

beer_url <- "http://craftcans.com/db.php?search=all&sort=beerid&ord=desc&view=text"

craft_beers <- readHTMLTable(beer_url)

craft_beers <- craft_beers[[11]]

craft_beers[] <- lapply(craft_beers, as.character)

names(craft_beers) <- tolower(names(craft_beers))

craft_beers$size <- gsub(" [ozOZ]\\.?", "", craft_beers$size)
craft_beers$size <- gsub(' "Silo Can"|Alumi-Tek®|Slimline|unce|[zZ]\\.?', '', craft_beers$size)
craft_beers$size <- gsub("12 & 16", "16", craft_beers$size)
craft_beers$size <- as.numeric(craft_beers$size)

craft_beers$abv <- gsub("???", NA, craft_beers$abv, fixed=TRUE)
craft_beers$abv <- gsub("%", "", craft_beers$abv, fixed=TRUE)
craft_beers$abv <- as.numeric(craft_beers$abv)

craft_beers$ibus <- gsub("N/A", NA, craft_beers$ibus)
craft_beers$ibus <- as.numeric(craft_beers$ibus)

write.csv(craft_beers, "~/Dropbox/craft_beers.csv", row.names = FALSE)
