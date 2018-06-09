

releases <- read.csv("~/Documents/rta-app-releases/data/rtwork-releases.csv",
                     stringsAsFactors = FALSE)

releases$dates <- as.Date(releases$dates)

releases <- releases[order(releases$dates), ]

y.pos <- seq(1, nrow(releases), by = 10)
y.lab <- releases$builds[y.pos]

x.pos <- releases$dates[y.pos]
x.lab <- format(x.pos, "%d/%m/%y")

png("~/Documents/rta-app-releases/figures/rtwork-release-cycle.png", 1076, 673, res = 100)

par(mar = c(5, 5.5, 2, 1),
    bg = "gray98",
    family = "Lato",
    cex.axis = 0.8)
plot(releases$dates, 1:nrow(releases), type = "s", col = "gray", axes = FALSE, ann = FALSE)
points(releases$dates, 1:nrow(releases), pch = 19, cex = 0.5, col = "forestgreen")
axis(1, at = x.pos, labels = x.lab, tick = FALSE, las = 2)
axis(2, at = y.pos, labels = y.lab, tick = FALSE, las = 1)
abline(h = y.pos, col = "lightgray", lty = "dotted")
abline(v = x.pos, col = "lightgray", lty = "dotted")
mtext("rtWork release cycle is getting shorter and more regular", adj = 0, font = 2)
mtext("builds", side = 2, line = 4, cex = 0.9)
arrows(as.Date("2017-02-09"), 22, as.Date("2017-04-07"), 22,
       length = 0.05, code = 3, col = "gray30")
text(as.Date("2017-03-15"), 26, labels = "Time it takes to level\nup ~10 versions",
     cex = 0.8, col = "gray30")
abline(v = as.Date("2016-09-13"), col = "red4")
text(as.Date("2016-07-20"), 65, labels = "Ship cycle\nback on schedule",
     cex = 0.8, col = "gray30")
arrows(as.Date("2016-08-20"), 65, as.Date("2016-09-13"), 65,
       length = 0.05, col = "gray30")
abline(v = as.Date("2016-05-30"), col = "red4")
text(as.Date("2016-04-20"), 50, labels = "rtSurvey\nlaunched",
     cex = 0.8, col = "gray30")
arrows(as.Date("2016-05-10"), 50, as.Date("2016-05-30"), 50,
       length = 0.05, col = "gray30")
rect(as.Date("2015-12-08"), 1, as.Date("2015-12-28"), 102,
     col = adjustcolor("gray80", alpha.f = 0.25), border = FALSE)
text(as.Date("2015-12-20"), 30, labels = "12 releases\nin\n20 days???",
     cex = 0.8, col = "gray30")

dev.off()

## dev.size("in")
## 11.208333  6.992017

## dev.size("px")
## 1076  673
