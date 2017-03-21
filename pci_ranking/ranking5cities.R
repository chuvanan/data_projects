
library(viridis)
library(reshape2)

pci <- data.frame(
  year = 2011:2015,
  hanoi = c(36, 51, 33, 26, 24),
  haiphong = c(45, 50, 15, 34, 28),
  danang = c(5, 12, 1, 1, 1),
  saigon = c(20, 13, 10, 4, 6),
  cantho = c(16, 14, 6, 15, 14)
)

x_label <- Reduce(c, pci[pci$year == 2015, 2:6])
point_label <- pci[pci$year %in% c(2011, 2015), ]
point_label <- melt(point_label, id.vars = "year", variable.name = "province", value.name = "rank")
my_colors <- viridis(5)


cairo_pdf("pci_ranking.pdf", width = 6.989583, height = 6.992017)
par(bg = "gray98", mar = c(5, 2, 5, 5), family = "Lato Light")
plot.new()
plot.window(xlim = c(2011, 2015),
            ylim = c(63, 1))
axis(1, lwd = 0)
axis(4, at = x_label, labels = c("Hà Nội", "Hải Phòng", "Đà Nẵng", "Sài Gòn", "Cần Thơ"),
     las = 1, lwd = 0, hadj = 0.4)
grid(NULL, NA)
points(pci$year, pci$hanoi, type = "b", col = my_colors[1], pch = 19)
points(pci$year, pci$haiphong, type = "b", col = my_colors[2], pch = 19)
points(pci$year, pci$danang, type = "b", col = my_colors[3], pch = 19)
points(pci$year, pci$saigon, type = "b", col = my_colors[4], pch = 19)
points(pci$year, pci$cantho, type = "b", col = my_colors[5], pch = 19)
text(point_label$year, point_label$rank - 2, labels = point_label$rank, xpd = T, cex = 0.8)
mtext("Thứ hạng PCI của 5 thành phố trực thuộc TW", adj = 0, font = 2, cex = 1.3, line = 3)
mtext("So với 63 tỉnh thành", adj = 0, font = 4, cex = 1, line = 1.5)
mtext("Nguồn: VCCI | @anchu", side = 1, font = 3, line = 3, adj = 1)
dev.off()

## dev.size("in") # 6.989583 6.992017
