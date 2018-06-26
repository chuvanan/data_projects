
library(dplyr)
library(ggplot2)
library(gridExtra)

setwd("~/Documents/data_projects/datatalk-meetup/meetup-02/")
air_quality <- readr::read_csv("./data/hanoi-air-quality.csv")

air_quality <- filter(air_quality, qc_name == "Valid" & !is.na(aqi))
air_quality$aqi <- as.double(air_quality$aqi)

air_quality <- air_quality %>%
    mutate(date_ymd = format(date, "%y-%m-%d"),
           date_ymd = as.Date(date_ymd))

new_air <- air_quality %>%
    mutate(day_th = lubridate::yday(date_ymd)) %>%
    group_by(day_th) %>%
    summarise(max_aqi = max(aqi),
              min_aqi = min(aqi),
              avg_aqi = mean(aqi),
              se_aqi = sd(aqi) / sqrt(length(aqi)),
              upper_hinge = fivenum(aqi)[4],
              lower_hinge = fivenum(aqi)[2]) %>%
    mutate(upper_aqi = avg_aqi + (2.101 * se_aqi),
           lower_aqi = avg_aqi - (2.101 * se_aqi)) %>%
    ungroup()

eom <- RTA::end_of_month(seq.Date(from = as.Date('2018-01-01'),
                                  by = '1 month', length.out = 12L))
eom <- lubridate::yday(eom)


## table legend
aqi_table <- data_frame(
    AQI = c("300+", "201-300", "151-200", "101-150", "51-100", "0-50"),
    `Pollution Level` = c("Hazardous", "Very unhealthy", "Unhealthy",
                          "Unhealthy for sensitive groups", "Moderate",
                          "Good")
)
aqi_table <- tableGrob(aqi_table, rows = NULL,
                       theme = ttheme_minimal(base_family = "Roboto Slab",
                                              base_colour = "gray30",
                                              base_size = 13L))

p <- ggplot(new_air) +
    geom_hline(yintercept = c(50, 100, 150, 200, 300), color = 'gray70',
               size = 0.5, linetype = 3) +
    geom_vline(xintercept = eom, color = 'gray70',
               size = 0.5, linetype = 3) +
    geom_linerange(aes(day_th, ymin = min_aqi, ymax = max_aqi),
                   color = 'wheat2') +
    geom_linerange(aes(day_th, ymin = lower_aqi, ymax = upper_aqi),
                       color = 'gray1') +
        labs(x = NULL, y = "Air Quality Index (AQI)",
             title = 'The polluted city',
             subtitle = "The graph shows Hanoi's daily outdoor air quality from Dec, 2015 to Jun, 2018",
             caption = "Data source: U.S. Embassy Hanoi Air Quality Monitor") +
        scale_y_continuous(breaks = c(50, 100, 150, 200, 300)) +
        scale_x_continuous(breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                           labels = c("January", "February", "March", "April",
                                      "May", "June", "July", "August", "September",
                                      "October", "November", "December"),
                           expand = c(0, 0)) +
        theme_minimal(base_family = 'Roboto Slab') +
        theme(panel.grid = element_blank(),
              plot.title = element_text(size = 30L),
              plot.subtitle = element_text(size = 15L, color = 'gray30'),
              axis.text.x = element_text(size = 15L, color = "black"),
              axis.text.y = element_text(size = 13),
              axis.title.y = element_text(size = 13, color = "gray30", vjust = 4),
              plot.caption = element_text(size = 14L, vjust = -4, color = "gray30"),
              plot.margin = unit(c(1.2, 1, 1, 1), "cm"))
p + geom_linerange(aes(x = 100, ymin = 250, ymax = 350),
                   size = 2, color = "wheat2") +
    geom_linerange(aes(x = 100, ymin = 280, ymax = 320),
                   size = 2, color = "gray30") +
    geom_linerange(aes(x = 104, ymin = 280, ymax = 320),
                   size = 0.35, color = "gray30") +
    annotate("text", 112, 350, label = "Record High",
             size = 4.5, color = "gray30") +
    annotate("text", 112, 250, label = "Record Low",
             size = 4.5, color = "gray30") +
    annotate("text", 117, 300, label = "Normal Range",
             size = 4.5, color = "gray30") +
    annotate("segment", x = 102.5, xend = 104, y = 280, yend = 280, color = "gray30", size = 0.35) +
    annotate("segment", x = 102.5, xend = 104, y = 320, yend = 320, color = "gray30", size = 0.35) +
    annotation_custom(aqi_table, xmin = 200, xmax = 230, ymin = 260, ymax = 340)

dev.size("in")
## [1] 15.38542  6.48294

dev.size("cm")
## [1] 39.07896 16.46667

dev.size("px")
## [1] 1477  624
