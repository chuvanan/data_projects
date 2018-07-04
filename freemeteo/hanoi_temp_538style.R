
library(animation)

hanoi <- readLines("~/Dropbox/freemeteo/VSHANOI.txt")

hanoi <- trimws(hanoi, "both")
hanoi <- strsplit(hanoi, split = " ")
hanoi <- lapply(hanoi, function(x) Filter(function(y) y != "", x))

dta <- data.frame(
  month = vapply(hanoi, function(x) x[1], character(1)),
  day = vapply(hanoi, function(x) x[2], character(1)),
  year = vapply(hanoi, function(x) x[3], character(1)),
  temp = vapply(hanoi, function(x) x[4], character(1)),
  stringsAsFactors = F
)

dta[] <- lapply(dta, as.numeric)
dta <- subset(dta, temp > 0)
dta$temp_celsius <- (dta$temp - 32) * 5/9

## -----------------------------------------------------------------------------

## x_labs <- seq(40, 100, 2)
## x_labs[!x_labs %in% c(40, 50, 60, 70, 80, 90, 100)] <- ""

## cairo_pdf("~/Dropbox/freemeteo/hanoi_temp_538style.pdf", 9.989583, 10.721785)

## par(mfrow = c(12, 1), mar = c(2, 1, 2, 1), bg = "gray98", yaxs = "i", family = "Lato")
## for (i in 1:12) {
##   out <- dta[dta$month == i, ]
##   temp_median <- round(median(out$temp), 0)
##   h <- hist(out$temp, freq = F, breaks = seq(40, 100, 0.5), xlim = c(40, 100),
##             ann = F, axes = F, col = "gray", border = "white")
##   mtext(month.name[i], adj = 0, cex = 0.9, col = "gray20")
##   abline(v = temp_median, col = "midnightblue", lwd = 2)
##   text(temp_median, max(h$density) + 0.025, label = temp_median, xpd = T, col = "midnightblue", cex = 1.1)
##   if (i == 12) axis(1, at = seq(40, 100, 2), labels = x_labs, col = "gray98", col.ticks = "gray", col.axis = "gray20")
##   abline(h = 0, col = "gray", lwd = 1.2)
## }

## dev.off()

## -----------------------------------------------------------------------------


## y_range <- local({
##   dta_split <- split(dta, dta$month)
##   dta_hist <- lapply(dta_split, function(x) hist(x$temp_celsius))
##   dta_hist <- lapply(dta_hist, function(x) x[["density"]])
##   range(unlist(dta_hist))
## })

## my_color <- rgb(136,117,67, max= 255, alpha = 150)

## cairo_pdf("~/Dropbox/freemeteo/hanoi_temp_celsius.pdf", 9.989583, 10.721785)

## par(mfrow = c(12, 1), mar = c(0, 0, 2.5, 0), bg = "gray98", yaxs = "i", family = "Lato", oma = c(2.5, 1.5, 1, 1))
## for (i in 1:12) {
##   out <- dta[dta$month == i, ]
##   temp_median <- round(median(out$temp_celsius), 0)
##   h <- hist(out$temp_celsius,
##             freq = F,
##             breaks = seq(5, 40, 0.25), xlim = c(5, 40), ylim = c(0, max(y_range) + 0.1),
##             ann = F, axes = F, col = my_color, border = "gray98", xpd = T)
##   mtext(month.name[i], adj = 0, cex = 0.9, col = "gray20")
##   ## abline(v = temp_median, col = "midnightblue", lwd = 2)
##   segments(temp_median, 0, temp_median, max(h$density), lwd = 2, col = "midnightblue", lend = 1)
##   text(temp_median, max(h$density) + 0.06, label = temp_median, xpd = T, col = "midnightblue", cex = 1.1)
##   if (i == 12) axis(1, col = "gray98", col.ticks = my_color, col.axis = "gray20", lwd = 0.5, cex.axis = 1.1)
##   if (i == 1) legend(33, 0.5, legend = "median temprature (Celsius)", lty = 1, bty = "n",
##                      lwd = 2, col = "midnightblue", text.col = "gray20", cex = 1.2, xpd = T)
##   ## abline(h = 0, col = my_color)
## }
## mtext("Temprature Variation in Hanoi, 1995-2016", oute = T, line = -0.5, adj = 1, font = 2)

## dev.off()

## -----------------------------------------------------------------------------


source("~/Dropbox/Rsnippets/col2rgb.R")

temp_by_month <- with(dta, aggregate(temp_celsius, list(year = year, month = month), mean))
temp_by_month <- subset(temp_by_month, year != 2017)

my_color <- col_to_rgb("gray", alpha = 200)
my_lwd <- 1

cairo_pdf("~/Dropbox/freemeteo/monthly_average_temperature_hanoi.pdf", 6.989583, 6.992017)

par(mar = c(3, 4, 3, 2), cex.axis = 0.9, col.axis = "gray30", family = "Lato", xaxs = "i")
plot.new()
plot.window(c(1, 12), range(temp_by_month$x))
grid(NA, NULL, lty = 1, lwd = 0.4)
axis(1, at = 1:12, labels = month.abb, tick = F)
axis(2, las = 1, tick = F)
## abline(v = 1:12, col = "lightgray", lty = "dotted")
for (i in 1995:2016) {
  out <- subset(temp_by_month, year == i)
  out <- out[order(out$month), ]
  if (i == 2016) {
    my_color <- "red4"
    my_lwd <- 2
  } else {
    my_color <- col_to_rgb("gray", alpha = 200)
    my_lwd <- 1
  }
  xspline(out$month, out$x, shape = 1, border = my_color, lwd = my_lwd)
}
mtext("Average monthly temperatures in Hanoi, 1995-2016", adj = 0, font = 2, line = 1, cex = 1.1)
mtext("Celsius", side = 2, line = 2.5, col = "gray30")
legend("topright", lty = 1, col = "red4", lwd = 2, legend = "2016", bty = "n", text.col = "gray30")

dev.off()
