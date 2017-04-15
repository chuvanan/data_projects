


dta <- read.csv("~/Dropbox/intro_visualization/demo/sakura.csv", stringsAsFactors = F)
dta$common.date <- as.Date(dta$common.date)


y_locs <- as.Date(c("2000-03-27", "2000-04-01", "2000-04-10", "2000-04-20", "2000-05-01", "2000-05-05"))
y_labs <- c("27th", "1st", "10th", "20th", "1st", "5th")
cherry_col <- rgb(205, 96, 144, max = 255, alpha = 180) # hotpink3
loess_col <- rgb(139, 129, 76, max = 255, alpha = 200) # lightgoldenrod4


## make plot window
plot.new()
plot.window(xlim = range(dta$AD),
            ylim = range(dta$common.date, na.rm = T))

## add axes
par(bg = "gray98")
plot.new()
plot.window(xlim = range(dta$AD),
            ylim = range(dta$common.date, na.rm = T))
axis(1)
axis(2)

## make date legible
par(bg = "gray98")
plot.new()
plot.window(xlim = range(dta$AD),
            ylim = range(dta$common.date, na.rm = T))
axis(1)
axis(2, at = y_locs, lab = y_labs, xpd = T, las = 1)


## add cherry blossom
par(bg = "gray98")
plot.new()
plot.window(xlim = range(dta$AD),
            ylim = range(dta$common.date, na.rm = T))
axis(1)
axis(2, at = y_locs, lab = y_labs, xpd = T, las = 1)
points(dta$AD, dta$common.date, pch = 21)

## add cherry blossom
par(bg = "gray98")
plot.new()
plot.window(xlim = range(dta$AD),
            ylim = range(dta$common.date, na.rm = T))
axis(1)
axis(2, at = y_locs, lab = y_labs, xpd = T, las = 1)
points(dta$AD, dta$common.date, pch = 21)
lines(loess.smooth(dta$AD, dta$common.date, span = 1/10), lwd = 2)

## make margin larger and add title
par(bg = "gray98", mar = c(4, 7, 4, 2))
plot.new()
plot.window(xlim = range(dta$AD),
            ylim = range(dta$common.date, na.rm = T))
axis(1)
axis(2, at = y_locs, lab = y_labs, xpd = T, las = 1)
points(dta$AD, dta$common.date, pch = 21)
lines(loess.smooth(dta$AD, dta$common.date, span = 1/10), lwd = 2)
mtext("Cherry bomb", adj = 0, line = 2.5, font = 2, cex = 1.1)
mtext("Date of cherry-blossom peak-bloom in Kyoto, Japan, 800AD-2016", adj = 0, line = 1)
mtext("Year", side = 1, line = 2)



## make segments 
par(bg = "gray98", mar = c(4, 7, 4, 2))
plot.new()
plot.window(xlim = range(dta$AD),
            ylim = range(dta$common.date, na.rm = T))
axis(1)
axis(2, at = y_locs, lab = y_labs, xpd = T, las = 1)
points(dta$AD, dta$common.date, pch = 21)
lines(loess.smooth(dta$AD, dta$common.date, span = 1/10), lwd = 2)
mtext("Cherry bomb", adj = 0, line = 2.5, font = 2, cex = 1.1)
mtext("Date of cherry-blossom peak-bloom in Kyoto, Japan, 800AD-2016", adj = 0, line = 1)
mtext("Year", side = 1, line = 2)
segments(600, y_locs[1], 600, y_locs[2] - 0.2, xpd = T, lwd = 20, lend = 1, col = "gray93")
segments(600, y_locs[2] + 0.2, 600, y_locs[5] - 0.2, xpd = T, lwd = 20, lend = 1, col = "gray93")
segments(600, y_locs[5] + 0.2, 600, y_locs[6], xpd = T, lwd = 20, lend = 1, col = "gray93")


## add text

## make segments 
par(bg = "gray98", mar = c(4, 7, 4, 2))
plot.new()
plot.window(xlim = range(dta$AD),
            ylim = range(dta$common.date, na.rm = T))
axis(1)
axis(2, at = y_locs, lab = y_labs, xpd = T, las = 1)
points(dta$AD, dta$common.date, pch = 21)
lines(loess.smooth(dta$AD, dta$common.date, span = 1/10), lwd = 2)
mtext("Cherry bomb", adj = 0, line = 2.5, font = 2, cex = 1.1)
mtext("Date of cherry-blossom peak-bloom in Kyoto, Japan, 800AD-2016", adj = 0, line = 1)
mtext("Year", side = 1, line = 2)
segments(600, y_locs[1], 600, y_locs[2] - 0.2, xpd = T, lwd = 20, lend = 1, col = "gray93")
segments(600, y_locs[2] + 0.2, 600, y_locs[5] - 0.2, xpd = T, lwd = 20, lend = 1, col = "gray93")
segments(600, y_locs[5] + 0.2, 600, y_locs[6], xpd = T, lwd = 20, lend = 1, col = "gray93")
text(600, y_locs[1] + 2.3, labels = "March", srt = 90, xpd = T, cex = 0.9, col = "gray30")
text(600, y_locs[2] + 15, labels = "April", srt = 90, xpd = T, cex = 0.9, col = "gray30")
text(600, y_locs[5] + 2.2, labels = "May", srt = 90, xpd = T, cex = 0.9, col = "gray30")


## add legend
par(bg = "gray98", mar = c(4, 7, 4, 2))
plot.new()
plot.window(xlim = range(dta$AD),
            ylim = range(dta$common.date, na.rm = T))
axis(1)
axis(2, at = y_locs, lab = y_labs, xpd = T, las = 1)
points(dta$AD, dta$common.date, pch = 21)
lines(loess.smooth(dta$AD, dta$common.date, span = 1/10), lwd = 2)
mtext("Cherry bomb", adj = 0, line = 2.5, font = 2, cex = 1.1)
mtext("Date of cherry-blossom peak-bloom in Kyoto, Japan, 800AD-2016", adj = 0, line = 1)
mtext("Year", side = 1, line = 2)
segments(600, y_locs[1], 600, y_locs[2] - 0.2, xpd = T, lwd = 20, lend = 1, col = "gray93")
segments(600, y_locs[2] + 0.2, 600, y_locs[5] - 0.2, xpd = T, lwd = 20, lend = 1, col = "gray93")
segments(600, y_locs[5] + 0.2, 600, y_locs[6], xpd = T, lwd = 20, lend = 1, col = "gray93")
text(600, y_locs[1] + 2.3, labels = "March", srt = 90, xpd = T, cex = 0.9, col = "gray30")
text(600, y_locs[2] + 15, labels = "April", srt = 90, xpd = T, cex = 0.9, col = "gray30")
text(600, y_locs[5] + 2.2, labels = "May", srt = 90, xpd = T, cex = 0.9, col = "gray30")
legend(1800, y_locs[6], legend = "Trend", lty = 1, xpd = T, lwd = 3, text.col = "gray20")


## finalize colors, fonts, spacing
par(bg = "gray98", yaxs = "i", family = "Lato", mar = c(4, 7, 4, 2))
plot.new()
plot.window(range(dta$AD), c(min(dta$common.date, na.rm = T), max(dta$common.date, na.rm = T) + 1))
axis(1, col.ticks = "gray20", col = "gray98", cex.axis = 0.9, col.axis = "gray20")
axis(2, at = y_locs, lab = y_labs, xpd = T, las = 1, tick = F, cex.axis = 0.9, col.axis = "gray30", line = -0.5)
abline(h = y_locs, col = "gray85")
abline(h = y_locs[1])
points(dta$AD, dta$common.date, bg = cherry_col, pch = 21, col = "white", xpd = T)
lines(loess.smooth(dta$AD, dta$common.date, span = 1/10), lwd = 3, col = loess_col)
mtext("Cherry bomb", adj = 0, line = 2.5, font = 2, cex = 1.1)
mtext("Date of cherry-blossom peak-bloom in Kyoto, Japan, 800AD-2016", adj = 0, line = 1)
mtext("Year", side = 1, line = 2, col = "gray20", font = 3)
segments(600, y_locs[1], 600, y_locs[2] - 0.2, xpd = T, lwd = 20, col = "gray93", lend = 1)
segments(600, y_locs[2] + 0.2, 600, y_locs[5] - 0.2, xpd = T, lwd = 20, col = "gray93", lend = 1)
segments(600, y_locs[5] + 0.2, 600, y_locs[6], xpd = T, lwd = 20, col = "gray93", lend = 1)
text(600, y_locs[1] + 2.3, labels = "March", srt = 90, xpd = T, cex = 0.9, col = "gray30")
text(600, y_locs[2] + 15, labels = "April", srt = 90, xpd = T, cex = 0.9, col = "gray30")
text(600, y_locs[5] + 2.2, labels = "May", srt = 90, xpd = T, cex = 0.9, col = "gray30")
mtext("Date of cherry-blossom peak-bloom", side = 2, line = 5, col = "gray20", font = 3)
legend(1800, y_locs[6], legend = "Trend", lty = 1, bty = "n", xpd = T, lwd = 3,
       col = loess_col, text.col = "gray20")


