

library(readr)
source("helper-functions.R")

wc_matches <- read_csv("../data/WorldCupMatches.csv")
wc_players <- read_csv("../data/WorldCupPlayers.csv")
wc <- read_csv("../data/WorldCups.csv")


## prettify column names

wc_matches <- normalize_colnames(wc_matches)
wc_players <- normalize_colnames(wc_players)
wc <- normalize_colnames(wc)

## sanity check
