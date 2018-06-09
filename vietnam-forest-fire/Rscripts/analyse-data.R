

setwd("~/Documents/vietnam-forest-fire/data/")

forest.fire <- read.csv("vietnam-forest-fire.csv", stringsAsFactors = FALSE)

provinces <- unique(forest.fire$province)

par(mfrow = c(8, 8),
    mar = c(2, 2, 2, 1),
    oma = c(2, 2, 2, 1),
    bg = "gray98",
    family = "Lato",
    col.axis = "gray30")

for (p in provinces) {
    prov <- forest.fire[forest.fire$province == p, ]
    prov <- prov[order(prov$year), ]
    barplot(prov$fire_damage,
            names.arg = prov$year,
            border = "gray98",
            col = "red4",
            yaxt = "n")
    grid(NA, NULL)
    axis(2, tick = F)
    mtext(p, cex = 0.7, adj = 0)
}
