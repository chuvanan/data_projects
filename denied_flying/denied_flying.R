

denied <- read.csv("~/Dropbox/denied_flying/denied.csv", stringsAsFactors = F)

denied <- local({
  count_year <- with(denied, aggregate(airline, list(airline = airline), length))
  denied <- merge(denied, count_year, by = "airline", all.x = T)
  denied <- subset(denied, x >= 3)
})

airlines <- unique(denied$airline)
my_colors <- c("#5e2f23", "#78dba1", "#da536a", "#bfd871", "#a04951",
               "#5a8b40", "#c26537", "#72bbba", "#c19d40", "#405037", "#c4a88e")
y_2014 <- round(log1p(denied$involuntary_db_per_10k[denied$year == 2014]), 2)
y_2016 <- round(log1p(denied$involuntary_db_per_10k[denied$year == 2016]), 2)
y_axislab <- denied$airline[denied$year == 2016]


cairo_pdf("~/Dropbox/denied_flying/denied_flying.pdf", 7.156250, 9.381562)

par(family = "Lato", bg = "gray98", col.axis = "gray30", mar = c(4, 3, 4, 9))
plot.new()
plot.window(range(denied$year), range(log1p(denied$involuntary_db_per_10k)))
abline(v = 2014:2016, lty = "dotted", col = "lightgray")
axis(1, at = 2014:2016, tick = F)
axis(2, at = unique(y_2014), tick = F, las = 1, line = -1.5, xpd = T)
axis(4, at = y_2016, tick = F, las = 1, line = -1.5, xpd = T)
axis(4, at = y_2016, labels = y_axislab, tick = F, las = 1, line = 0.5, xpd = T)
for (i in 1:11) {
  out <- subset(denied, airline == airlines[i])
  out <- out[order(out$year), ]
  lines(out$year, log1p(out$involuntary_db_per_10k), col = my_colors[i], lwd = 3)
}
mtext("Involuntary Disembark Rate Per 10K Passengers", line = 2, adj = 0, cex = 1.3, font = 2)
mtext("Y-axis log scale; Only included airlines with 3-year span data", line = 0.5, adj = 0)
mtext("Source: U.S. DoT Air Travel Consumer Reports", side = 1, line = 2.5, adj = 1, cex = 0.9, col = "gray30")

dev.off()

## dev.size("in")
