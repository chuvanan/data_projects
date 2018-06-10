## Minimal boxplot of Vietnam CPI
## Mar 06, 2017


## -----------------------------------------------------------------------------
## import and clean data

library(reshape2)

cpi <- read.csv2("~/Downloads/vietnam_cpi.csv", stringsAsFactors = F, skip = 2)
names(cpi)[1] <- "month"
cpi <- melt(cpi, id.vars = "month", variable.name = "year", value.name = "cpi")
cpi$year <- gsub("X", "", cpi$year)
cpi$year <- as.integer(cpi$year)
cpi$cpi <- as.double(cpi$cpi) - 100
cpi$month <- gsub("Tháng ", "", cpi$month)
cpi$month <- as.integer(cpi$month)


## -----------------------------------------------------------------------------
## plotting

cairo_pdf("~/Downloads/CPI.pdf", width = 6.989583, height = 6.992017)

par(family = "Lato Light", mar = c(4, 4, 5, 2))
plot.new()
plot.window(xlim = c(1, 12),
            ylim = range(cpi$cpi))
grid(NA, NULL)
boxplot(cpi ~ month, data = cpi,
        pars = list(boxcol = "transparent", medlty = "blank", medpch=19,
                    medcol = "midnightblue", whisklwd = 1.3,
                    whisklty = c(1, 1), medcex = 0.8,  outcex = 0,
                    staplelty = "blank", whiskcol = "midnightblue"),
        axes = F, ann = F, add = T)
axis(1, at = 1:12, labels = month.abb, lwd = 0, cex.axis = 0.9, col.axis = "gray30", padj = -2)
axis(2, at = -1:4, labels = paste0(-1:4, "%"), lwd = 0, las = 1, cex.axis = 0.9, col.axis = "gray30")
mtext("Variations in Vietnam CPI, 1995-2015", 3, line = 3, adj = 0, cex = 1.4, font = 2)
mtext("Measured as percentage of monthly change", 3, line = 1.5, adj = 0, cex = 1, font = 3)
mtext("Source: gso.gov.vn | By: @anchu", 1, line = 2, adj = 1, font = 3, col = "gray30", cex = 0.9)

dev.off()
## dev.size("in")                          # 6.989583 6.992017
