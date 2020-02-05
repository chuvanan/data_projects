


## data sources:
## * https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports
## * https://en.wikipedia.org/wiki/2019%E2%80%9320_Wuhan_coronavirus_outbreak
## * https://docs.google.com/spreadsheets/d/1UF2pSkFTURko2OvfHWWlFpDFAr1UxCBA4JLwlSP6KFo/htmlview?usp=sharing&sle=true#

outbreak <- data.frame(
    date = seq.Date(as.Date("2020-01-16"), as.Date("2020-01-29"), by = "days"),
    cumulative_count = c(45, 62, 121, 198, 291, 440, 571, 830, 1287, 1975, 2744, 4515, 5974, 7711)
)

poly_fit <- lm(cumulative_count ~ poly(date, 3), data = outbreak)
summary(poly_fit)
outbreak$preds <- predict(poly_fit)

new_outbreak_data <- data.frame(
    date = seq.Date(as.Date("2020-01-30"), length.out = 10, by = "days") # make 10 day ahead forecast
)

new_outbreak_data$cumulative_count <- c(9692, 11791, 14380, rep(NA, 7)) # update actual outbreak here
new_outbreak_data$preds <- predict(poly_fit, newdata = new_outbreak_data)

## combine them all
outbreak <- rbind(outbreak, new_outbreak_data)

png(filename = "./wuhan-virus-outbreak-modeling.png", 7, 7, units = "in", res = 300)
par(family = "Carlito")
plot(outbreak$date, outbreak$cumulative_count, pch = 1, col = "red4", type = "b",
     ylab = "number of people infected", xlab = "date (in 2020)", las = 1,
     main = "Wuhan Coronavirus Outbreak Prediction (10 days ahead)", lwd = 1.5, ylim = c(0, max(outbreak$preds)))
points(outbreak$date[outbreak$date >= as.Date("2020-01-30")], outbreak$preds[outbreak$date >= as.Date("2020-01-30")],
       pch = 1, col = "steelblue", type = "b", lwd = 1.5)
abline(v = as.Date("2020-01-29"), lty = "dashed")
legend("bottomright", legend = c("actual", "predicted"), bty = "n", pch = 19, col = c("red4", "steelblue"))
dev.off()
