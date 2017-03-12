
library(viridis)

data <- read.csv("~/Dropbox/teacher_salary_oecd/DP_LIVE_01032017104847898.csv",
                 stringsAsFactors = F)
country_code <- read.csv("~/Dropbox/teacher_salary_oecd/Country.csv",
                         stringsAsFactors = F)

names(data) <- tolower(names(data))

data <- subset(data, subject %in% c("PRY_START", "PRY15YREXP", "PRY_TOP"))

pry_start <- subset(data, subject == "PRY_START", select = c(location, value))
names(pry_start) <- c("location", "value_start")

pry_15exp <- subset(data, subject == "PRY15YREXP" & time == 2012 & measure == "USD",
                    select = c(location, value))
names(pry_15exp) <- c("location", "value_15exp")

pry_top <- subset(data, subject == "PRY_TOP", select = c(location, value))
names(pry_top) <- c("location", "value_top")


pry_start <- merge(pry_start,
                   pry_15exp,
                   by = "location",
                   all.x = T)
pry_start <- merge(pry_start,
                   pry_top,
                   by = "location",
                   all.x = T)

pry_start <- merge(pry_start,
                   country_code[, c("Country.Code", "Short.Name", "Income.Group")],
                   by.x = "location",
                   by.y = "Country.Code",
                   all.x = T)
## pry_start$Short.Name[pry_start$location == "OAVG"] <- "OECD - Average"
pry_start <- pry_start[order(pry_start$value_start), ]
oavg <- subset(pry_start, location == "OAVG")
oavg$Short.Name <- "OECD - Average"

x_range <- range(c(pry_start$value_start, pry_start$value_15exp, pry_start$value_top), na.rm = TRUE)
y_range <- c(1, 35)

cairo_pdf("~/Dropbox/teacher_salary_oecd/teacher_salary.pdf",
          width=7.095895, height=7.492618)

par(bg = "gray98", mar = c(3, 4, 5, 3), family = "Lato Light")
plot.new()
plot.window(xlim = x_range,
            ylim = y_range)
axis(1, lwd = 0, cex.axis = 0.8, col.axis = "gray40",
     at = seq(0, 120000, 20000), padj = -2,
     labels = sprintf("%s$", format(seq(0, 120000, 20000), big.mark = ",")))
grid(NULL, NA)
segments(pry_start$value_start, 1:35, pry_start$value_top, 1:35,
         col = "#21908CFF", lty = "dotted")
points(pry_start$value_start, 1:35, pch = 21, bg = "#21908CFF", col = "white")
points(pry_start$value_top, 1:35, pch = 21, bg = "#21908CFF", col = "white")
segments(oavg$value_start, 21, oavg$value_top, 21, lty = "dotted", col = "orange")
points(oavg$value_start, 21, pch = 21, bg = "orange", col = "white")
points(oavg$value_top, 21, pch = 21, bg = "orange", col = "white")
text(pry_start$value_start - 3000, 1:35, labels = pry_start$Short.Name,
     adj = 1, cex = 0.8, xpd = T, col = "gray40")
text(oavg$value_start - 3000, 21, labels = oavg$Short.Name,
     adj = 1, cex = 0.8, xpd = T, font = 2, col = "orange")
text(66085, 37.5, labels = "starting salary", cex = 0.9, xpd = T, col = "gray40")
text(118412, 37.5, labels = "top of scale", cex = 0.9, xpd = T, col = "gray40")
arrows(66085, 37, 66085, 35.5, length = 0.05, xpd = T, lwd = 0.5, col = "gray40")
arrows(118412, 37, 118412, 35.5, length = 0.05, xpd = T, lwd = 0.5, col = "gray40")
mtext(side = 3, text = "Highest-paying teachers in the world", adj = 0, cex = 1.4,
      line = 3.5, font = 2)
mtext(side = 3, text = "Measured in USD as the average gross salaries before tax and other contributions, 2012",
      adj = 0, cex = 0.95, line = 2, font = 3)
mtext(side = 1, text = "Source: OCED | By: @anchu", adj = 1,
      cex = 0.9, col = "gray40", font = 3, line = 1.7)

dev.off()

## dev.size("in") 7.095895 7.492618
