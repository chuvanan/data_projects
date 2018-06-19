

## -----------------------------------------------------------------------------

library(reshape2)
library(plotrix)

sea_levels <- read.csv("~/Dropbox/Public/muc_nuoc_bien.csv",
                       sep = ";", skip = 1,
                       stringsAsFactors = FALSE)

names(sea_levels) <- c("year", "location", paste0("m", 1:12))

for (i in seq_along(sea_levels$year)) {
  if (is.na(sea_levels$year[i])) {
    sea_levels$year[i] <- sea_levels$year[i-1]
  }
}

sea_levels <- subset(sea_levels, location != "")
rownames(sea_levels) <- NULL

sea_levels <- melt(sea_levels,
                   id.vars = c("location", "year"),
                   variable.name = "month",
                   value.name = "unit_cm")
sea_levels$month <- gsub("m", "", sea_levels$month)
sea_levels$month <- as.integer(sea_levels$month)

## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------

png("sea-level.png", width = 1000, height = 600, res = 100)

par(mar = c(2.5, 3.5, 2.5, 1),
    mfrow = c(4, 4),
    las = 1, bty = "n",
    tck = -0.03, xpd  = TRUE)

for (i in unique(sea_levels$location)) {
  out <- subset(sea_levels, location == i)
  plot(out$month, out$unit_cm, type = "n", axes = FALSE,
       xlab = "", ylab = "", ylim = c(min(out$unit_cm), max(out$unit_cm)))
  points(unit_cm ~ month, data = subset(out, year == 2015),
         type = "o", lwd = 1.3, col = "red")
  points(unit_cm ~ month, data = subset(out, year == 2014),
         type = "o", lwd = 1.3, col = "blue")
  mtext(side = 3, text = i, line = 0.5, adj = 0, font = 2, cex = 0.9)
  axis(side = 1,
       at = 1:12,
       labels = c(1, NA, 3, NA, 5, NA, 7, NA, 9, NA, 11, NA),
       tck = -0.01,
       cex.axis = 0.95,
       col.axis = "gray10")
  axis(side = 2,
       tck = -0.01,
       cex.axis = 0.95,
       col.axis = "gray10")
  if (i == "Cô Tô") {
    legend("topleft",
           legend = c("2015", "2014"),
           col = c("red", "blue"),
           pch = 1, bty="n", cex = 1)
  }
}


dev.off()

