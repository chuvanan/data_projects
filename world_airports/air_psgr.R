
library(reshape2)

air_psgr <- read.csv("~/Documents/data_projects/world_airports/API_IS.AIR.PSGR_DS2_en_csv_v2.csv",
                     stringsAsFactors = FALSE,
                     skip = 4)

air_psgr$Indicator.Code <- NULL
air_psgr$Indicator.Name <- NULL
air_psgr$X2016 <- NULL
air_psgr$X <- NULL

names(air_psgr)[1:2] <- c("country_name", "country_code")

air_psgr <- Filter(function(x) !all(is.na(x)), air_psgr)

air_psgr <- melt(air_psgr,
                 id.vars = c("country_code", "country_name"),
                 variable.name = "year",
                 value.name = "n")

air_psgr$year <- gsub("X", "", air_psgr$year)
air_psgr$year <- as.integer(air_psgr$year)

vietnam <- subset(air_psgr, country_code == "VNM")

par(mar = c(2, 2.5, 2, 2), bg = "gray95", xaxs="i")
plot.new()
plot.window(xlim = range(air_psgr$year),
            ylim = range(air_psgr$n, na.rm = TRUE))
axis(side = 1, lwd = 0,
     at = seq(1970, 2015, by = 15),
     labels = c("1970", "1985", "2000", "2015"),
     cex.axis = 0.8, tck = 0, col.axis = "gray20", line = -1)
axis(2, las = 1, lwd = 0,
     at = seq(0, 3.5e9, by = 5e8),
     labels = c("0", seq(0.5, 3, by = 0.5), "3.5 billion"),
     cex.axis = 0.8, hadj = 0.3, col.axis = "gray20")
abline(h = seq(0, 3e9, by = 5e8), col = "lightgray", lty = "dotted")
for (i in unique(air_psgr$country_code)) {
  out <- subset(air_psgr, country_code == i)
  lines(out$year, out$n, col = "gray")
}
lines(vietnam$year, vietnam$n, col = "red", lwd = 1.1)
segments(1973, 3.5e9, 2015, 3.5e9, col = "lightgray", lty = "dotted")
legend(1975, 12e8, legend = "Vietnam",
       col = "red", lty = "solid", bty = "n",
       cex = 0.8)
