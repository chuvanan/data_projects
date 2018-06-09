


setwd("~/Documents/forest-stats/data/")

forest.land <- read.csv("forest-coverage.csv", stringsAsFactors = FALSE)

## keep individual countries only

forest.land <- forest.land[forest.land$Region != "", ]

## -----------------------------------------------------------------------------
## changes in forest coverage splited by regions

regions <- unique(forest.land$Region)

split.regions <- split(forest.land, forest.land$Region)

par(mfrow = c(3, 3),
    mar = c(2, 2, 3, 2),
    oma = c(2, 2, 2, 1),
    bg = "gray98",
    family = "Lato",
    col.axis = "gray30")

for (r in regions) {
    region <- split.regions[[r]]
    countries <- unique(region$CountryCode)
    plot.new()
    plot.window(xlim = c(1990, 2015),
                ylim = c(0, 100))
    grid()
    axis(1, tick = FALSE, line = -0.5)
    axis(2, col.ticks = "gray30", col = "gray98")
    for (i in countries) {
        country <- region[region$CountryCode == i, ]
        country <- country[order(country$Year), ]
        my.col <- if (i == "VNM") "forestgreen" else adjustcolor("gray", alpha.f = 0.8)
        my.lwd <- if (i == "VNM") 2.5 else 1.2
        lines(country$Year, country$Perct, col = my.col, lwd = my.lwd)
    }
    mtext(r, cex = 0.9)
}


## dev.size("in")
## 7.479167 9.101050

## dev.size("px")
## 718 876
