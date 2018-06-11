## -----------------------------------------------------------------------------
## code: explore-data.R
## description: explore top open source projects on github
## author: anchu@rta.vn
## date: Sat Jul 15 11:06:26 2017
## -----------------------------------------------------------------------------


library(wordcloud)

github.repos <- read.csv("~/Documents/top-starred-opensource-projects/data/TopStaredRepositories.csv",
                         stringsAsFactors = FALSE)


## -----------------------------------------------------------------------------
## data wrangling
## -----------------------------------------------------------------------------


## top list

head(github.repos, 10)

## bottom list
tail(github.repos, 10)

## which are the most popular language?

pop.lang <- table(github.repos$Language[github.repos$Language != ""]) # don't count empty string
pop.lang <- sort(pop.lang)

par(bg = "gray98", cex.axis = 0.8, mar = c(3.5, 7, 2, 1), family = "Lato")
barplot(pop.lang, horiz = TRUE, las = 1, border = FALSE,
        col = adjustcolor("forestgreen", 0.8))
mtext("number of repos", side = 1, line = 2)
mtext("Most Popular Languages Used", line = 0, font = 2)
grid(NULL, NA, col = "gray98", lwd = 1.5, lty = 1)


## which are the most popular tags?

pop.tags <- strsplit(github.repos$Tags, ",")
pop.tags <- unlist(pop.tags)
pop.tags <- sort(table(pop.tags))

par(family = "Lato")
wordcloud(names(pop.tags), pop.tags, scale = c(8, 0.5), max.words = 100,
          random.order = FALSE)


## distribution of stars

stars.count <- gsub("k", "", github.repos$Number.of.Stars)
stars.count <- as.double(stars.count) * 1000

par(family = "Lato", bg = "gray98", cex.axis = 0.8)
hist(log(stars.count), breaks = 50, border = "gray98", col = "steelblue",
     xlim = c(8, 13))
