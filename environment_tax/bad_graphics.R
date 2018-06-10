


env_tax <- data.frame(
  year = 2012:2016,
  thu = c(11.2, 11.5, 11.9, 27, 41.9),
  chi = c(9, 9.7, 9.9, 11.4, 12.3)
)

cairo_pdf("~/Dropbox/taxgraph.pdf", width = 6.406250, height = 6.618001)

## set up figure region
par(bg = "gray98", family = "Lato Light",
    mar = c(5, 3, 5, 2))
xrange <- range(env_tax$year)
yrange <- range(c(env_tax$chi, env_tax$thu))

## plotting
plot.new()
plot.window(xlim = xrange,
            ylim = yrange)
grid(NULL, NULL)
axis(side = 1, lwd = 0, col = "gray98", col.axis = "gray40",
     cex.axis = 0.8)
axis(side = 2, lwd = 0, col = "gray98", col.axis = "gray40",
     cex.axis = 0.8, las = 1)
points(env_tax$year, env_tax$thu, type = "b", col = "cadetblue", lwd = 0.9, pch = 19, cex = 1.2)
points(env_tax$year, env_tax$chi, type = "b", col = "cadetblue", lwd = 0.9, pch = 1, cex = 1.2)
segments(2014, 10.5, 2014, 11.3, col = "red4")
arrows(2015, 12, 2015, 26, col = "red4", length = 0.08, code = 3)
arrows(2016, 13, 2016, 41, col = "red4", length = 0.08, code = 3)
text(2014.05, 11, labels = "2", cex = 0.9, adj = 0, col = "red4", font = 2)
text(2015.05, 18.9, labels = "15.6", cex = 0.9, adj = 0, col = "red4", font = 2)
text(2016.05, 27.1, labels = "29.6", cex = 0.9, adj = 0, xpd = TRUE, col = "red4", font = 2)

## title and labels
mtext(side = 3, line = 3, text = "Thu và Chi Thuế Bảo Vệ Môi Trường",
      adj = 0, font = 2, cex = 1.2)
mtext(side = 3, line = 1.5, text = "Đơn vị: nghìn tỷ đồng",
      font = 3, adj = 0)
mtext(side = 1, line = 3, text = "Nguồn dữ liệu: Vnexpress/Infographics. Đồ họa: @anchu",
      col = "gray40", font = 3, adj = 1, cex = 0.8)

## legend
arrows(2013.1, 22.5, 2013.1, 24.5, col = "red4", length = 0.08, code = 3)
text(2013.2, 23.5, labels = "Chênh lệch thu chi", cex = 0.9, adj = 0, col = "red4", font = 2)
legend(2013, 29, legend = c("Tổng thu", "Tổng chi"),
       pch = c(19, 1),
       col = "cadetblue",
       bty = "n", lwd = 1.3, cex = 0.9)

dev.off()
