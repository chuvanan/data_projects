

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
              se_aqi = sd(aqi) / sqrt(length(aqi))) %>%
    mutate(upper_aqi = avg_aqi + (2.101 * se_aqi),
           lower_aqi = avg_aqi - (2.101 * se_aqi)) %>%
    ungroup()

eom <- RTA::end_of_month(seq.Date(from = as.Date('2018-01-01'),
                                  by = '1 month', length.out = 12L))
eom <- lubridate::yday(eom)

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
         subtitle = "Data shows Hanoi's daily outdoor air quality from Dec, 2015 to Jun, 2018\n
0-50: Good | 51-100: Moderate | 101-150: Unhealthy for Sensitive Groups | 151-200: Unhealthy | 201-300: Very Unhealthy | 300+: Hazardous",
caption = "Data source: U.S. Embassy Hanoi Air Quality Monitor") +
    scale_y_continuous(breaks = c(50, 100, 150, 200, 300)) +
    scale_x_continuous(breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                       labels = c("January", "February", "March", "April",
                                  "May", "June", "July", "August", "September",
                                  "October", "November", "December"),
                       expand = c(0, 0)) +
    theme_minimal(base_family = 'Lato') +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 30L),
          plot.subtitle = element_text(size = 15L, color = 'gray30'),
          axis.text.x = element_text(size = 15L, color = "black"),
          axis.text.y = element_text(size = 14L),
          axis.title.y = element_text(size = 15, color = "gray30", vjust = 4),
          plot.caption = element_text(size = 14L, vjust = -4, color = "gray30"),
          plot.margin = unit(c(1.2, 1, 1, 1), "cm"))
p + geom_linerange(aes(x = 181, ymin = 250, ymax = 350),
                   size = 2, color = "wheat2") +
    geom_linerange(aes(x = 181, ymin = 280, ymax = 320),
                   size = 2, color = "gray30") +
    geom_linerange(aes(x = 185, ymin = 280, ymax = 320),
                   size = 0.35, color = "gray30") +
    annotate("text", 194, 350, label = "Record High",
             size = 5, color = "gray30") +
    annotate("text", 194, 250, label = "Record Low",
             size = 5, color = "gray30") +
    annotate("text", 199, 300, label = "Normal Range",
             size = 5, color = "gray30")

dev.size("in")
## [1] 15.38542  6.48294

dev.size("cm")
## [1] 39.07896 16.46667

dev.size("px")
## [1] 1477  624
