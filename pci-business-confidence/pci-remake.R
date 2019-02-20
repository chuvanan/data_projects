

library(ggplot2)
library(ggrepel)
library(tidyr)
library(dplyr)

dta <- read.csv("pci-bus-conf.csv", stringsAsFactors = FALSE)

dta %>%
    spread(bus_indicator, value) %>%
    rename(ind1 = `DN se mo rong SXKD`,
           ind2 = `Dong gop cua TN vao GDP`) %>%
    ggplot(aes(ind1, ind2)) +
    geom_point(size = 3, color = "red4") +
    geom_smooth(method = "lm", se = FALSE, color = "gray90") +
    geom_text_repel(aes(label = year), size = 5) +
    labs(x = "Ty le DN mo rong SXKD",
         y = "Toc do tang truong dong gop cuar KVTN vao GDP") +
    theme_minimal(base_size = 14)

ggsave(filename = "pci-remake.pdf")
