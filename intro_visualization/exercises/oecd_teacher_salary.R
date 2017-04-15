## Critique by redesign
## Case 3
## http://vnexpress.net/infographics/giao-duc/luong-khoi-diem-cua-giao-vien-trung-hoc-co-so-tren-the-gioi-3548729.html



## -----------------------------------------------------------------------------
## IMPORT DATA
## -----------------------------------------------------------------------------

dta <- read.csv("oecd_teacher_salary.csv", stringsAsFactors = F)
dta <- dta[order(dta$starting), ]

## generate colors for points and labels
dta$col <- "#21908CFF"
dta$col[dta$country == "OECD Average"] <- "red4"
dta$col_lab <- "gray30"
dta$col_lab[dta$country == "OECD Average"] <- "red4"

x_range <- range(c(dta$starting, dta$top))
y_range <- c(1, nrow(dta))
axis_labs <- sprintf("%s$", format(seq(0, 140000, 20000), big.mark = ","))

## -----------------------------------------------------------------------------
## VISUALIZE DATA
## -----------------------------------------------------------------------------


## create plotting windows
plot.new()
plot.window(xlim = x_range, ylim = y_range)
axis(1, tick = F)

## add segments
plot.new()
plot.window(xlim = x_range, ylim = y_range)
axis(1, tick = F)
segments(dta$starting, 1:36, dta$top, 1:36)

## add points
plot.new()
plot.window(xlim = x_range, ylim = y_range)
axis(1, tick = F)
segments(dta$starting, 1:36, dta$top, 1:36)
points(dta$starting, 1:36, pch = 19)
points(dta$top, 1:36, pch = 19)


## add country labels
plot.new()
plot.window(xlim = x_range, ylim = y_range)
axis(1, tick = F)
segments(dta$starting, 1:36, dta$top, 1:36)
points(dta$starting, 1:36, pch = 19)
points(dta$top, 1:36, pch = 19)
text(dta$starting - 3000, 1:36, labels = dta$country, adj = 1, xpd = T)


## make margin large
par(mar = c(3, 5, 5, 3))
plot.new()
plot.window(xlim = x_range, ylim = y_range)
axis(1, tick = F)
segments(dta$starting, 1:36, dta$top, 1:36)
points(dta$starting, 1:36, pch = 19)
points(dta$top, 1:36, pch = 19)
text(dta$starting - 3000, 1:36, labels = dta$country, adj = 1, xpd = T)

## add legend
par(mar = c(3, 5, 5, 3))
plot.new()
plot.window(xlim = x_range, ylim = y_range)
axis(1, tick = F)
segments(dta$starting, 1:36, dta$top, 1:36)
points(dta$starting, 1:36, pch = 19)
points(dta$top, 1:36, pch = 19)
text(dta$starting - 3000, 1:36, labels = dta$country, adj = 1, xpd = T)
text(79920, 38, labels = "lương khởi điểm", xpd = T)
text(138920, 38, labels = "lương tối đa", xpd = T)
arrows(79920, 37.5, 79920, 36.5, length = 0.05, xpd = T)
arrows(138920, 37.5, 138920, 36.5, length = 0.05, xpd = T)


## add context
par(mar = c(3, 5, 5, 3))
plot.new()
plot.window(xlim = x_range, ylim = y_range)
axis(1, tick = F)
segments(dta$starting, 1:36, dta$top, 1:36)
points(dta$starting, 1:36, pch = 19)
points(dta$top, 1:36, pch = 19)
text(dta$starting - 3000, 1:36, labels = dta$country, adj = 1, xpd = T)
text(79920, 38, labels = "lương khởi điểm", xpd = T)
text(138920, 38, labels = "lương tối đa", xpd = T)
arrows(79920, 37.5, 79920, 36.5, length = 0.05, xpd = T)
arrows(138920, 37.5, 138920, 36.5, length = 0.05, xpd = T)
mtext(side = 3, text = "Cách biệt về lương giáo viên các quốc gia OECD", adj = 0, line = 3.5)
mtext(side = 3, text = "So sánh lương khởi điểm và lương tối đa của giáo viên trung học cơ sở, 2013", adj = 0, line = 2)
mtext(side = 1, text = "Nguồn: OECD", adj = 1, font = 3, line = 1.7)


## refine colors, spacing, font size
par(mar = c(3, 5, 5, 3))
plot.new()
plot.window(xlim = x_range, ylim = y_range)
axis(1, lwd = 0, cex.axis = 0.85, col.axis = "gray40", line = -1,
     at = seq(0, 140000, 20000), labels = axis_labs)
grid(NULL, NA)
segments(dta$starting, 1:36, dta$top, 1:36, col = dta$col, lty = "dotted")
points(dta$starting, 1:36, pch = 19, col = dta$col, cex = 0.9)
points(dta$top, 1:36, pch = 19, col = dta$col, cex = 0.9)
text(dta$starting - 3000, 1:36, labels = dta$country, adj = 1, cex = 0.85, xpd = T, col = dta$col_lab)
text(79920, 38, labels = "lương khởi điểm", cex = 0.9, xpd = T, col = "gray40")
text(138920, 38, labels = "lương tối đa", cex = 0.9, xpd = T, col = "gray40")
arrows(79920, 37.5, 79920, 36.5, length = 0.05, xpd = T, lwd = 0.7, col = "gray40")
arrows(138920, 37.5, 138920, 36.5, length = 0.05, xpd = T, lwd = 0.7, col = "gray40")
mtext(side = 3, text = "Cách biệt về lương giáo viên các quốc gia OECD", adj = 0, cex = 1.4,
      line = 3.5, font = 2)
mtext(side = 3, text = "So sánh lương khởi điểm và lương tối đa của giáo viên trung học cơ sở, 2013",
      adj = 0, cex = 0.95, line = 2, font = 3)
mtext(side = 1, text = "Nguồn: OECD", adj = 1,
      cex = 0.9, col = "gray40", font = 3, line = 1.7)


## -----------------------------------------------------------------------------
## EXPORT
## -----------------------------------------------------------------------------

cairo_pdf("oecd_teacher_salary.pdf", 7.095895, 7.492618)

par(mar = c(3, 5, 5, 3))
plot.new()
plot.window(xlim = x_range, ylim = y_range)
axis(1, lwd = 0, cex.axis = 0.85, col.axis = "gray40", line = -1,
     at = seq(0, 140000, 20000), labels = axis_labs)
grid(NULL, NA)
segments(dta$starting, 1:36, dta$top, 1:36, col = dta$col, lty = "dotted")
points(dta$starting, 1:36, pch = 19, col = dta$col, cex = 0.9)
points(dta$top, 1:36, pch = 19, col = dta$col, cex = 0.9)
text(dta$starting - 3000, 1:36, labels = dta$country, adj = 1, cex = 0.85, xpd = T, col = dta$col_lab)
text(79920, 38, labels = "lương khởi điểm", cex = 0.9, xpd = T, col = "gray40")
text(138920, 38, labels = "lương tối đa", cex = 0.9, xpd = T, col = "gray40")
arrows(79920, 37.5, 79920, 36.5, length = 0.05, xpd = T, lwd = 0.7, col = "gray40")
arrows(138920, 37.5, 138920, 36.5, length = 0.05, xpd = T, lwd = 0.7, col = "gray40")
mtext(side = 3, text = "Cách biệt về lương giáo viên các quốc gia OECD", adj = 0, cex = 1.4,
      line = 3.5, font = 2)
mtext(side = 3, text = "So sánh lương khởi điểm và lương tối đa của giáo viên trung học cơ sở, 2013",
      adj = 0, cex = 0.95, line = 2, font = 3)
mtext(side = 1, text = "Nguồn: OECD", adj = 1,
      cex = 0.9, col = "gray40", font = 3, line = 1.7)

dev.off()
