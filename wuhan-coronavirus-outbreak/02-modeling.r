


## data sources:
## * https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports
## * https://en.wikipedia.org/wiki/2019%E2%80%9320_Wuhan_coronavirus_outbreak

outbreak <- data.frame(
    date = seq.Date(as.Date("2020-01-16"), as.Date("2020-01-31"), by = "days"),
    cumulative_count = c(45, 62, 121, 198, 291, 440, 571, 830, 1287, 1975, 2744, 4515, 5974, 7711, 9692, 11791)
)

poly_fit <- lm(cumulative_count ~ poly(date, 3), data = outbreak)
summary(poly_fit)
outbreak$preds <- predict(poly_fit)

png(filename = "./wuhan-virus-outbreak-modeling.png", 7, 7, units = "in", res = 300)
par(family = "Carlito")
plot(outbreak$date, outbreak$cumulative_count, pch = 1, col = "red4", type = "b",
     ylab = "number of people infected", xlab = "date (in 2020)", las = 1,
     main = "Wuhan Coronavirus Outbreak", lwd = 1.5)
points(outbreak$date, outbreak$preds, pch = 1, col = "steelblue", type = "b", lwd = 1.5)
legend("center", legend = c("actual", "predicted"), bty = "n", pch = 19, col = c("red4", "steelblue"))
dev.off()
