

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

## ggsave(filename = "figures/national_trend.pdf", national_trend_plot)
## ==> changes in national numbers seem to be small and steady.
## How did they come up with those numbers?

regions <- c("CẢ NƯỚC", "Đồng bằng sông Hồng", "Trung du và miền núi phía Bắc",
             "Bắc Trung Bộ và duyên hải miền Trung", "Tây Nguyên",
             "Đông Nam Bộ", "Đồng bằng sông Cửu Long")

## average of all provinces?
gender_stats %>%
    filter(!province %in% regions) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE) # Nope

## median of all provinces?
gender_stats %>%
    filter(!province %in% regions) %>%
    summarise_if(is.numeric, median, na.rm = TRUE) # Nope

## average of all regions
gender_stats %>%
    filter(province %in% regions[regions != "CẢ NƯỚC"]) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE) # Nope

## median of all regions
gender_stats %>%
    filter(province %in% regions[regions != "CẢ NƯỚC"]) %>%
    summarise_if(is.numeric, median, na.rm = TRUE) # Nope

## ==> there must be a weighted formula to calculate the national statistics?
