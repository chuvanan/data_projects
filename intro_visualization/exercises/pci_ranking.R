## Critique by redesign
## Case 2
## http://infographics.vn//nhin-lai-xep-hang-pci-cua-5-thanh-pho-truc-thuoc-trung-uong/5573.vna


library(viridis) # for colors
library(reshape2)


## import data
pci <- read.csv("pci_ranking.csv", stringsAsFactors = F)

x_label <- Reduce(c, pci[pci$year == 2015, 2:6])
point_label <- pci[pci$year %in% c(2011, 2015), ]
point_label <- melt(point_label, id.vars = "year", variable.name = "province", value.name = "rank")
my_colors <- viridis(5)

## -----------------------------------------------------------------------------
## VISUALIZE DATA
## -----------------------------------------------------------------------------

## first try
plot(pci$year, pci$hanoi, type = "b")

## inverse yaxis
plot(pci$year, pci$hanoi, type = "b", ylim = c(63, 1))

## add other province
plot(pci$year, pci$hanoi, type = "b", ylim = c(63, 1))
lines(pci$year, pci$haiphong, type = "b")
lines(pci$year, pci$danang, type = "b")
lines(pci$year, pci$saigon, type = "b")
lines(pci$year, pci$cantho, type = "b")

## add grid
plot(pci$year, pci$hanoi, type = "b", ylim = c(63, 1))
grid(NULL, NA)
lines(pci$year, pci$haiphong, type = "b")
lines(pci$year, pci$danang, type = "b")
lines(pci$year, pci$saigon, type = "b")
lines(pci$year, pci$cantho, type = "b")

## remove labels
plot(pci$year, pci$hanoi, type = "b", ylim = c(63, 1), ann = F, axes = F)
grid(NULL, NA)
lines(pci$year, pci$haiphong, type = "b")
lines(pci$year, pci$danang, type = "b")
lines(pci$year, pci$saigon, type = "b")
lines(pci$year, pci$cantho, type = "b")

## remake labels
plot(pci$year, pci$hanoi, type = "b", ylim = c(63, 1), ann = F, axes = F)
grid(NULL, NA)
lines(pci$year, pci$haiphong, type = "b")
lines(pci$year, pci$danang, type = "b")
lines(pci$year, pci$saigon, type = "b")
lines(pci$year, pci$cantho, type = "b")
axis(1, tick = F)
axis(4, at = x_label, labels = c("Hà Nội", "Hải Phòng", "Đà Nẵng", "Sài Gòn", "Cần Thơ"),
     las = 1, tick = F)

## make margin larger
par(mar = c(4, 2, 7, 5))
plot(pci$year, pci$hanoi, type = "b", ylim = c(63, 1), ann = F, axes = F)
grid(NULL, NA)
lines(pci$year, pci$haiphong, type = "b")
lines(pci$year, pci$danang, type = "b")
lines(pci$year, pci$saigon, type = "b")
lines(pci$year, pci$cantho, type = "b")
axis(1, tick = F)
axis(4, at = x_label, labels = c("Hà Nội", "Hải Phòng", "Đà Nẵng", "Sài Gòn", "Cần Thơ"),
     las = 1, tick = F)

## add data labels
par(mar = c(4, 2, 7, 5))
plot(pci$year, pci$hanoi, type = "b", ylim = c(63, 1), ann = F, axes = F)
grid(NULL, NA)
lines(pci$year, pci$haiphong, type = "b")
lines(pci$year, pci$danang, type = "b")
lines(pci$year, pci$saigon, type = "b")
lines(pci$year, pci$cantho, type = "b")
axis(1, tick = F)
axis(4, at = x_label, labels = c("Hà Nội", "Hải Phòng", "Đà Nẵng", "Sài Gòn", "Cần Thơ"),
     las = 1, tick = F)
text(point_label$year, point_label$rank - 2, labels = point_label$rank, xpd = T)


## add context
par(mar = c(4, 2, 7, 5))
plot(pci$year, pci$hanoi, type = "b", ylim = c(63, 1), ann = F, axes = F)
grid(NULL, NA)
lines(pci$year, pci$haiphong, type = "b")
lines(pci$year, pci$danang, type = "b")
lines(pci$year, pci$saigon, type = "b")
lines(pci$year, pci$cantho, type = "b")
axis(1, tick = F)
axis(4, at = x_label, labels = c("Hà Nội", "Hải Phòng", "Đà Nẵng", "Sài Gòn", "Cần Thơ"),
     las = 1, tick = F)
text(point_label$year, point_label$rank - 2, labels = point_label$rank, xpd = T)
mtext("Thứ hạng PCI của 5 thành phố trực thuộc TW", adj = 0, line = 5) # title
mtext("Chỉ số PCI đo lường chất lượng môi trường kinh doanh, điều hành kinh tế\nvà cải cách hành chính của chính quyền 63 tỉnh/thành phố",
      adj = 0, line = 2)                # subtitle
mtext("Nguồn: VCCI", side = 1, line = 2.5, adj = 1) # source


## add colors
par(mar = c(4, 2, 7, 5))
plot(pci$year, pci$hanoi, type = "b", ylim = c(63, 1), ann = F, axes = F)
grid(NULL, NA)
lines(pci$year, pci$haiphong, type = "b", col = my_colors[2])
lines(pci$year, pci$danang, type = "b", col = my_colors[3])
lines(pci$year, pci$saigon, type = "b", col = my_colors[4])
lines(pci$year, pci$cantho, type = "b", col = my_colors[5])
axis(1, tick = F)
axis(4, at = x_label, labels = c("Hà Nội", "Hải Phòng", "Đà Nẵng", "Sài Gòn", "Cần Thơ"),
     las = 1, tick = F)
text(point_label$year, point_label$rank - 2, labels = point_label$rank, xpd = T)
mtext("Thứ hạng PCI của 5 thành phố trực thuộc TW", adj = 0, line = 5) # title
mtext("Chỉ số PCI đo lường chất lượng môi trường kinh doanh, điều hành kinh tế\nvà cải cách hành chính của chính quyền 63 tỉnh/thành phố",
      adj = 0, line = 2)                # subtitle
mtext("Nguồn: VCCI", side = 1, line = 2.5, adj = 1) # source

## final adjustment of line width and spacing
par(mar = c(4, 2, 7, 5))
plot(pci$year, pci$hanoi, type = "b", ylim = c(63, 1), ann = F, axes = F, pch = 19, lwd = 2, col = my_colors[1])
grid(NULL, NA)
lines(pci$year, pci$haiphong, type = "b", col = my_colors[2], pch = 19, lwd = 2)
lines(pci$year, pci$danang, type = "b", col = my_colors[3], pch = 19, lwd = 2)
lines(pci$year, pci$saigon, type = "b", col = my_colors[4], pch = 19, lwd = 2)
lines(pci$year, pci$cantho, type = "b", col = my_colors[5], pch = 19, lwd = 2)
axis(1, lwd = 0, col.axis = "gray30", line = -0.5)
axis(4, at = x_label, labels = c("Hà Nội", "Hải Phòng", "Đà Nẵng", "Sài Gòn", "Cần Thơ"),
     las = 1, lwd = 0, adj = 0, line = -1, col.axis = "gray20")
text(point_label$year, point_label$rank - 2, labels = point_label$rank, xpd = T, cex = 0.8, col = "gray30")
mtext("Thứ hạng PCI của 5 thành phố trực thuộc TW", adj = 0, font = 2, cex = 1.3, line = 5)
mtext("Chỉ số PCI đo lường chất lượng môi trường kinh doanh, điều hành kinh tế\nvà cải cách hành chính của chính quyền 63 tỉnh/thành phố",
      adj = 0, font = 3, line = 2, col = "gray20")
mtext("Nguồn: VCCI", side = 1, font = 3, line = 2.5, adj = 1, col = "gray30")


## -----------------------------------------------------------------------------
## EXPORT
## -----------------------------------------------------------------------------

cairo_pdf("~/Dropbox/intro_visualization/figures/pci_ranking.pdf", width = 6.989583, height = 6.992017)

par(mar = c(4, 2, 7, 5))
plot(pci$year, pci$hanoi, type = "b", ylim = c(63, 1), ann = F, axes = F, pch = 19, lwd = 2, col = my_colors[1])
grid(NULL, NA)
lines(pci$year, pci$haiphong, type = "b", col = my_colors[2], pch = 19, lwd = 2)
lines(pci$year, pci$danang, type = "b", col = my_colors[3], pch = 19, lwd = 2)
lines(pci$year, pci$saigon, type = "b", col = my_colors[4], pch = 19, lwd = 2)
lines(pci$year, pci$cantho, type = "b", col = my_colors[5], pch = 19, lwd = 2)
axis(1, lwd = 0, col.axis = "gray30", line = -0.5)
axis(4, at = x_label, labels = c("Hà Nội", "Hải Phòng", "Đà Nẵng", "Sài Gòn", "Cần Thơ"),
     las = 1, lwd = 0, adj = 0, line = -1, col.axis = "gray20")
text(point_label$year, point_label$rank - 2, labels = point_label$rank, xpd = T, cex = 0.8, col = "gray30")
mtext("Thứ hạng PCI của 5 thành phố trực thuộc TW", adj = 0, font = 2, cex = 1.3, line = 5)
mtext("Chỉ số PCI đo lường chất lượng môi trường kinh doanh, điều hành kinh tế\nvà cải cách hành chính của chính quyền 63 tỉnh/thành phố",
      adj = 0, font = 3, line = 2, col = "gray20")
mtext("Nguồn: VCCI", side = 1, font = 3, line = 2.5, adj = 1, col = "gray30")

dev.off()
