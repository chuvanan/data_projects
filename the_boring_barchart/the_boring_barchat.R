

## https://vnexpress.net/infographics/oto-xe-may/10-oto-ban-chay-nhat-viet-nam-thang-truoc-tet-3711252.html

library(tidyr)
library(dplyr)
library(ggplot2)

dta <- read.csv("~/Documents/data_projects/the_boring_barchart/cars_sales.csv",
                stringsAsFactors = FALSE)

top10 <- dta %>%
    group_by(model) %>%
    summarise(total_sales = sum(sales)) %>%
    arrange(desc(total_sales)) %>%
    pull(model)

dta <- dta %>%
    mutate(region = factor(region,
                           levels = c("North", "Central", "South"),
                           ordered = TRUE),
           model = factor(model,
                          levels = rev(top10),
                          ordered = TRUE))

dta %>%
    ggplot(aes(model, sales, color = region)) +
    geom_point(size = 3) +
    coord_flip() +
    ## facet_wrap( ~ region) +
    labs(x = NULL, y = NULL,
         title = "Best-selling cars in Vietnam, Jan 2018",
         caption = "Data source: Vnexpress") +
    scale_color_manual(values = c("#CC3300", "#DAD0C6", "#0680CD")) +
    theme_minimal() +
    theme(text = element_text(family = "Lato", size = 13),
          legend.position = "top",
          legend.justification = c(1, 0),
          legend.title = element_blank(),
          plot.background = element_rect(fill = "gray97"),
          plot.caption = element_text(size = 10, face = "italic", color = "gray30"))

dev.size()
## 11.177083  5.433618

dta %>%
    filter(region != "Central") %>%
    spread(region, sales) %>%
    ggplot(aes(North, South)) +
    geom_text(aes(label = model)) +
    scale_x_continuous(expand = c(0, 0))
