

library(ggplot2)
library(dplyr)

egot <- readr::read_csv("EGOT-winners.csv")

egot %>%
    mutate(winner = forcats::fct_reorder(winner, year, max),
           winner = forcats::fct_rev(winner)) %>%
    ggplot(aes(year, winner, color = prize, group = winner)) +
    geom_point()
