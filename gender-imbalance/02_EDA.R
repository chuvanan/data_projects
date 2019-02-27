

library(tidyr)
library(dplyr)                          # >= 0.8.0
library(purrr)
library(ggplot2)
library(ggalt)                          # for geom_xspline()

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

## Which provinces have shown incresing trend? Are there any difference betwwen
## regions?

province_only <- gender_stats %>%
    filter(!province %in% regions)

## demo for Hanoi only
hanoi <- province_only %>%
    filter(province == "Hà Nội") %>%
    gather(year, bal, y2005:y2017) %>%
    mutate(year = as.numeric(gsub("y", "", year)))

hanoi %>%
    ggplot(aes(year, bal)) +
    geom_point() +
    geom_smooth(se = FALSE, method = "lm")

lm(bal ~ year, hanoi)$coefficients[2]   # slope of the fitted line
## negative: downtrend
## positive: uptrend

province_model <- province_only %>%
    gather(year, bal, y2005:y2017) %>%
    mutate(year = as.numeric(gsub("y", "", year))) %>%
    group_by(province) %>%
    nest() %>%
    mutate(model = map(data, ~ lm(bal ~ year, data = .x))) %>%
    mutate(slope = map_dbl(model, ~ .x[["coefficients"]][2]))

## number of provinces have positive trend
sum(province_model$slope > 0)           # 43
province_model$province[province_model$slope > 0]

## number of provinces have negative trend
sum(province_model$slope < 0)           # 21
province_model$province[province_model$slope < 0]

## How to visualize the trend of all provinces?

province_model %>%
    select(province, data, slope) %>%
    unnest() %>%
    mutate(slope_sign = if_else(slope > 0, "up", "down")) %>%
    ggplot(aes(year, bal, group = province, color = slope_sign)) +
    geom_xspline(alpha = 0.5, spline_shape = 1) +
    scale_x_continuous(breaks = c(2005, 2007:2017)) +
    scale_color_viridis_d(guide = FALSE, option = "inferno") +
    facet_wrap( ~ slope_sign) +
    bigger_font_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
