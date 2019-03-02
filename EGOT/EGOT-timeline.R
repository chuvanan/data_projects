

library(ggplot2)
library(tidyr)
library(dplyr)
library(ggrepel)
library(gridExtra)
library(hrbrthemes)

egot_raw <- readr::read_csv("EGOT-winners.csv")

egot <- egot_raw %>%
    select(winner = name, emmy, grammy, oscar, tony) %>%
    gather(prize, year, -winner)

egot <- egot %>%
    mutate(winner = forcats::fct_reorder(winner, year, max),
           winner = forcats::fct_rev(winner))

prize_colors <- c("#e74329", "#88abff", "#ba8c00", "#d648c9")

bigger_font_theme <- function() {
    theme_minimal() +
        theme(text = element_text(size = 20),
              legend.position = "top",
              legend.title = element_blank(),
              plot.title = element_text(hjust = 0.5),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())
}

egot_timeline <- ggplot(aes(year, winner,
                            fill = prize,
                            group = winner),
                        data = egot) +
    geom_line(color = "gray30", size = 0.5) +
    geom_point(size = 4, shape = 21, color = "white") +
    labs(title = "TOWARDS THE EGOT",
         x = NULL, y = NULL) +
    scale_fill_manual(values = prize_colors) +
    scale_x_continuous(breaks = seq(1930, 2010, 10)) +
    bigger_font_theme()

## ggsave(filename = "EGOT-timeline.pdf", egot_timeline)

emmy <- egot %>%
    filter(prize == "emmy") %>%
    mutate(winner = forcats::fct_reorder(winner, year),
           winner = forcats::fct_rev(winner)) %>%
    ggplot(aes(year, winner, label = winner)) +
    geom_point() +
    geom_text_repel() +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(breaks = seq(1930, 2010, 10)) +
    theme_minimal() +
    theme(axis.text.y = element_blank())

grammy <- egot %>%
    filter(prize == "grammy") %>%
    mutate(winner = forcats::fct_reorder(winner, year),
           winner = forcats::fct_rev(winner)) %>%
    ggplot(aes(year, winner, label = winner)) +
    geom_point() +
    geom_text_repel() +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(breaks = seq(1930, 2010, 10)) +
    theme_minimal() +
    theme(axis.text.y = element_blank())

oscar <- egot %>%
    filter(prize == "oscar") %>%
    mutate(winner = forcats::fct_reorder(winner, year),
           winner = forcats::fct_rev(winner)) %>%
    ggplot(aes(year, winner, label = winner)) +
    geom_point() +
    geom_text_repel() +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(breaks = seq(1930, 2010, 10)) +
    theme_minimal() +
    theme(axis.text.y = element_blank())

tony <- egot %>%
    filter(prize == "tony") %>%
    mutate(winner = forcats::fct_reorder(winner, year),
           winner = forcats::fct_rev(winner)) %>%
    ggplot(aes(year, winner, label = winner)) +
    geom_point() +
    geom_text_repel() +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(breaks = seq(1930, 2010, 10)) +
    theme_minimal() +
    theme(axis.text.y = element_blank())

grid.arrange(emmy, grammy, oscar, tony)

egot_winners <- egot_raw %>%
    ggplot(aes(year_span, age, label = name)) +
    geom_smooth(se = FALSE, method = "lm", color = "lightblue") +
    geom_point() +
    geom_text_repel(size = 4, color = "white") +
    scale_color_ipsum() +
    labs(x = "years taken to win all four awards",
         y = "age at the time of completion",
         title = "EGOT Winners") +
    theme_modern_rc()

ggsave(filename = "EGOT-winners.pdf", egot_winners)
