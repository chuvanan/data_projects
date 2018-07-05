

admission <- data.frame(
  college = c("Cornell", "Upenn", "Harvard", "Columbia", "Yale", "Brown", "Princeton", "Darthmouth"),
  ungtuyen = c(47038, 40413, 39506, 37389, 32900, 32724, 31056, 20034),
  trungtuyen = c(5889, 3699, 2056, 2185, 2272, 2722, 1890, 2092),
  stringsAsFactors = F
)

admission$rate <- round(admission$trungtuyen * 100 / admission$ungtuyen, 1)
admission$total <- 100
admission <- admission[order(admission$rate, decreasing = T), ]

cairo_pdf("~/Dropbox/tyle_trungtuyen/admis_rate.pdf", width=6.791667, height=4.560914)

par(mar = c(3, 9, 4, 2), family = "Lato", bg = "gray98")
bp <- barplot(admission$total, horiz = T, las = 1, border = F, col = "gray92", xaxt = "n")
barplot(admission$rate, horiz = T, add = T, col = "cadetblue", border = F, xaxt = "n")
axis(2, at = bp, labels = admission$college, las = 1, tick = F, line = 2, cex.axis = 0.95)
axis(2, at = bp, labels = paste0(admission$rate, "%"), las = 1, tick = F, line = -1, cex.axis = 0.85)
mtext("The most competitive admissions in US", line = 2.5, adj = 0, cex = 1.2, font = 2)
mtext("Measured by acceptance rate of class of 2017", line = 1, adj = 0, font = 3, cex = 0.9)
mtext("Source: Business Insider | By: @anchu", adj = 1, font = 3, cex = 0.9, side = 1, col = "gray30", line = 1)

dev.off()
