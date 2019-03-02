

library(ggplot2)
library(dplyr)

egot <- readr::read_csv("EGOT-winners.csv")

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
