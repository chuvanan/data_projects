

this_base <- "fig01-02_similar-pie-wedges-dot-plot"

my_data <- data.frame(
  variable = c("A", "B", "C", "D", "E"),
  value = c(23, 22, 19.5, 18.5, 17)
)

png(paste0(this_base, ".png"), width = 7, height = 4, units = "in", res = 100)

my_data <- my_data[order(my_data$value, decreasing = T), ]
par(mar = c(2, 3, 3, 1))
plot(my_data$value, my_data$variable, ann = F, axes = F, xlim = c(0, 25), type = "n")
box(col = "grey50")
grid(NA, NULL, lty = "solid", col = "grey50", lwd = 0.3)
points(my_data$value, my_data$variable, pch = 19)
axis(2, at = 5:1, labels = my_data$variable, las = 1, col = "grey50", cex.axis = 0.9)
mtext("Fig 1.2 Similar Pie Wedges: Dot Plot", font = 2, cex = 1.2, line = 0.5)

dev.off()
