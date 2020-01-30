

require(ggplot2)
require(ggrepel)

theme_set(
    theme_bw(base_size = 16, base_family = "Lato") +
    theme(panel.grid = element_blank(),
          panel.border = element_blank())
)

service_score <- read.csv("service-score.csv", stringsAsFactors = FALSE)

ggplot(service_score, aes(reorder(region, avg_score), score)) +
    geom_line(color = "gray80") +
    geom_point(shape = 21, color = "white", fill = "gray70", size = 3) +
    geom_point(aes(region, avg_score), fill = "orange", color = "gray80", shape = 23, size = 4) +
    geom_text_repel(aes(label = cocode), size = 4, nudge_x = -0.06) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme()
## ggsave("./service-ranking.png")
