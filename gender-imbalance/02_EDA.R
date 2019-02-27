

library(tidyr)
library(dplyr)
library(ggplot2)

gender_stats <- read.csv("gender-imbalance-vietnam.csv",
                         stringsAsFactors = FALSE)

bigger_font_theme <- theme_minimal() +
    theme(text = element_text(size = 15))

## the national trend over years?

national_trend <- gender_stats %>%
    filter(region == "CẢ NƯỚC") %>%
    gather(year, pct, y2005:y2017) %>%
    mutate(year = as.integer(gsub("y", "", year)))

national_trend_plot <- national_trend %>%
    ggplot(aes(year, pct)) +
    geom_step(color = "darkgreen") +
    geom_point(size = 3, color = "red4") +
    scale_x_continuous(breaks = c(2005, 2007:2017)) +
    scale_y_continuous(breaks = unique(national_trend$pct)) +
    labs(x = NULL, y = NULL) +
    bigger_font_theme +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "figures/national_trend.pdf", national_trend_plot)
