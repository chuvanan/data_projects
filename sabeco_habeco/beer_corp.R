## source: http://news.zing.vn/so-phan-trai-nguoc-cua-hai-anh-em-nganh-bia-post717959.html


sabeco <- data.frame(
  year = 2008:2016,
  profit = c(919, 1840, 3302, 2756, 3362, 3274, 3530, 4433, 5707)
)

habeco <- data.frame(
  year = 2008:2016,
  profit = c(342, 671, 866, 1022, 1187, 1224, 1392, 1171, 1002)
)


png("~/Documents/data_projects/sabeco_habeco/beer.png",
    width = 524, height = 437, res = 100)

par(mar= c(3, 4.5, 2, 1),
    bg = "gray95")
plot.new()
plot.window(xlim = c(2008, 2016),
            ylim = range(c(sabeco$profit, habeco$profit)))
grid(NULL, NULL)
axis(side = 1, lwd = 0, col.axis = "gray20", cex.axis = 0.8, line = -0.5,
     at = seq(2008, 2016, by = 2),
     labels = c("2008", "'10", "'12", "'14", "'16"))
axis(side = 2, lwd = 0, col.axis = "gray20", cex.axis = 0.8, las = 1,
     hadj = 0.7,
     at = seq(1000, 5000, by = 1000),
     labels = c("1,000", "2,000", "3,000", "4,000", "5,000"))
points(sabeco$year, sabeco$profit,
       type = "o",
       col = "forestgreen",
       pch = 19)
points(habeco$year, habeco$profit,
       type = "o",
       col = "red2",
       pch = 19)
mtext(side = 2, text = "Lợi nhuận thuần (tỷ đồng)",
      line = 3.2, cex = 0.85, col = "gray20")
mtext(side = 3, text = "Bia xanh, bia đỏ",
      line = 0.8, adj = 0, font = 2)
mtext(side = 1, text = "Nguồn: Zing News và BCTC của Sabeco, Habeco",
      line = 1.8, cex = 0.8, col = "gray20", adj = 1)
text(x = 2012, y = 3600, labels = "Sabeco", col = "forestgreen", cex = 0.9)
text(x = 2014, y = 1600, labels = "Habeco", col = "red2", cex = 0.9)

dev.off()
