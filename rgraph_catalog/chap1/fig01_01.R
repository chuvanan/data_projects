

this_base <- "fig01-01_similar-pie-wedges"

my_data <- data.frame(
  variable = c("A", "B", "C", "D", "E"),
  value = c(23, 22, 19.5, 18.5, 17)
)

png(paste0(this_base, ".png"), width = 6, height = 5, units = "in", res = 100)

par(mar = c(0, 0, 3, 0))
pie(my_data$value,
    labels = my_data$variable,
    init.angle = 90,
    clockwise = T,
    col = "white",
    border = "black",
    main = "Fig 1.1 Similar Pie Wedges",
    cex.main = 1.5)

dev.off()
