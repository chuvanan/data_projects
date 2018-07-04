

out <- data.frame(
  year = c(2012:2017),
  ex = c(24.5, 29.7, 33.3, 35.7, 37.9, 43.7),
  im = c(24.8, 29.2, 32.3, 37.5, 37.1, 45.6)
)

out$year_lb <- paste0("Q1/'", 12:17)
out$net <- out$ex - out$im
out$net_color <- "cadetblue"
out$net_color[out$net < 0] <- "red4"

cairo_pdf("~/Dropbox/xuat_nhap_khau/ex_im.pdf", width=9.052083, height=5.153106)

par(bg = "gray98", family = "Lato", mfrow = c(1, 2), col.axis = "gray30")
## plot 1
plot.new()
plot.window(xlim = c(2012, 2017),
            ylim = range(c(out$ex, out$im)))
axis(1, at = out$year[c(1, 3, 5)], labels = out$year_lb[c(1, 3, 5)], tick = F, cex.axis = 0.8)
axis(1, at = out$year[c(2, 4, 6)], labels = out$year_lb[c(2, 4, 6)], tick = F, cex.axis = 0.8, line = 1)
axis(2, tick = F, las = 1, cex.axis = 0.9)
grid()
points(out$year, out$ex, type = "o", col = "blue", pch = 19)
points(out$year, out$im, type = "o", col = "red", pch = 19)
mtext("Exports and Imports of Vietnam", line = 2, adj = 0, cex = 1.2)
legend(2014, 43, legend = c("Export", "Import"), pch = 19, col = c("blue", "red"), bty = "n", lty = 1, cex = 0.9)
text(2012.3, 45, "(billion USD)", xpd = T, cex = 0.9, col = "gray30")
## plot 2
bp <- barplot(out$net, names.arg = F, col = out$net_color, border = F, las = 1, yaxt = "n")
axis(2, tick = F, las = 1, col = "gray98", lty = 1, cex.axis = 0.9)
grid(NA, NULL)
axis(1, at = bp[c(1, 3, 5)], labels = out$year_lb[c(1, 3, 5)], tick = F, cex.axis = 0.8)
axis(1, at = bp[c(2, 4, 6)], labels = out$year_lb[c(2, 4, 6)], tick = F, cex.axis = 0.8, line = 1)
mtext("Net exports of Vietnam", adj = 0, line = 2, cex = 1.2)
text(0.6, 1, "(billion USD)", xpd = T, cex = 0.9, col = "gray30")
legend(bp[1], -1, legend = c("Trade surplus", "Trade deficit"), col = c("cadetblue", "red4"), pch = 15, cex = 0.9, bty = "n")

dev.off()
