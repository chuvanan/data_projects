

library(plotrix)
library(viridis)
library(reshape2)


hair_style <- read.csv("~/Dropbox/facial_hair_styles/Facial Hair Styles - Sheet1.csv",
                       stringsAsFactors = F)

names(hair_style) <- c("year", "cleanshaven", "sideburns", "muttonchops", "moustache_with_sideburns",
                       "moustache_with_muttonchops", "moustache", "goatee", "full_beard_chinstrap")

hair_style$total <- apply(hair_style[, -1], 1, sum)

style_cols <- setdiff(names(hair_style), c("year", "total"))
hair_style[, style_cols] <- lapply(hair_style[, style_cols], function(x) round(x * 100/hair_style$total, 1))
hair_style$total <- NULL
legends <- gsub("_", " ", style_cols)

## cairo_pdf("~/Dropbox/facial_hair_styles/hair_styles.pdf", 11.708333, 6.368657)

## par(bg = "gray98", family = "Lato", mar = c(3, 3, 4, 13))
## stackpoly(hair_style$year, hair_style[, -1], stack = T, staxx = F, axis4 = F,
##           border = "gray", col = viridis(8, alpha = 0.7))
## mtext("A Century of Facial Hair Styles", line = 2, adj = 0 , font = 2, cex = 1.2)
## mtext("University graduate facial hair styles (measured by percentage), 1898-2008",
##       line = 0.5, adj = 0, font = 3)
## legend("right", legend = legends, col = viridis(8, alpha = 0.7), pch = 15,
##        inset = c(-0.305, 0), xpd = T, bty = "n")

## dev.off()


library(lattice)


demo <- melt(hair_style, id.vars = "year")

names(demo)

xyplot(value ~ year | variable, data = demo, type = "l", lwd = 2,
       main = "Facial Hair Styles", family = "Lato")

xyplot(moustache ~ year, data = hair_style, type = "l", lwd = 2, col = "red4",
       main = "Facial Hair Styles", family = "Lato")
