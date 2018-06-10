

library(WDI)

rm(list = ls())

new_cache <- WDIcache()

WDIsearch("obesity", cache = new_cache)

obesity_male <- WDI(indicator = "SH.STA.OB18.MA.ZS", start = 1960, end = 2016)
obesity_female <- WDI(indicator = "SH.STA.OB18.FE.ZS", start = 1960, end = 2016)

names(obesity_male)[names(obesity_male) == "SH.STA.OB18.MA.ZS"] <- "obesity_rate"
names(obesity_female)[names(obesity_female) == "SH.STA.OB18.FE.ZS"] <- "obesity_rate"

obesity_male$sex <- "m"
obesity_female$sex <- "f"

obesity <- rbind(obesity_male, obesity_female)
obesity <- obesity[obesity$year %in% c(2010, 2014), ]
obesity <- obesity[!is.na(obesity$obesity_rate), ]

cairo_pdf("obesity.pdf", width=6.395833, height=6.379046)

par(bg = "gray98",
    mfrow = c(1, 2),
    mgp = c(1, 1, 0),
    family = "Lato",
    mar = c(3, 3, 3, 4),
    oma = c(2, 1, 3.5, 1),
    xaxs = "i")
## male
plot.new()
plot.window(xlim = c(2010, 2014),
            ylim = range(obesity$obesity_rate))
axis(1, lwd = 0, at = c(2010, 2014), col.axis = "gray40", cex.axis = 0.8, padj = -1)
axis(2, lwd = 0, las = 1, col.axis = "gray40", cex.axis = 0.8)
abline(h = 0, col = "black", cex = 1.2)
mtext(side = 3, text = "Male", font = 2, adj = 0, cex = 0.9)
for (i in unique(obesity$country)) {
  out <- subset(obesity, country == i & sex == "m")
  my_color <- if (i == "Vietnam") "red" else "gray"
  points(out$year, out$obesity_rate, type = "l", col = my_color)
}
points(x = c(2010, 2014),
       y = c(mean(obesity$obesity_rate[obesity$year == 2010 & obesity$sex == "m"]),
             mean(obesity$obesity_rate[obesity$year == 2014 & obesity$sex == "m"])),
       type = "l",
       col = "blue")
text(2014.5, 2.3, labels = "2.3", xpd = TRUE,
     cex = 0.8, col = "red")
text(2014.5, 15.5, labels = "15.5", xpd = TRUE,
     cex = 0.8, col = "blue")
## female
plot.new()
plot.window(xlim = c(2010, 2014),
            ylim = range(obesity$obesity_rate))
axis(1, lwd = 0, at = c(2010, 2014), col.axis = "gray40", cex.axis = 0.8, padj = -1)
axis(2, lwd = 0, las = 1, col.axis = "gray40", cex.axis = 0.8)
abline(h = 0, col = "black", cex = 1.2)
mtext(side = 3, text = "Female", font = 2, adj = 0, cex = 0.9)
for (i in unique(obesity$country)) {
  out <- subset(obesity, country == i & sex == "f")
  my_color <- if (i == "Vietnam") "red" else "gray"
  points(out$year, out$obesity_rate, type = "l", col = my_color)
}
points(x = c(2010, 2014),
       y = c(mean(obesity$obesity_rate[obesity$year == 2010 & obesity$sex == "f"]),
             mean(obesity$obesity_rate[obesity$year == 2014 & obesity$sex == "f"])),
       type = "l",
       col = "blue")
text(2014.5, 4.8, labels = "4.8", xpd = TRUE,
     cex = 0.8, col = "red")
text(2014.5, 22.9, labels = "22.9", xpd = TRUE,
     cex = 0.8, col = "blue")
mtext(side = 3, outer = TRUE,
      text = "Prevalance of obesity", line = 2,
      font = 2, adj = 0, cex = 1.2)
mtext(side = 3, outer = TRUE,
      text = "Measured by the proportion of overweight population aged 18+ of 190 countries",
      line = 0.5, font = 3, adj = 0, cex = 0.8, col = "gray20")
mtext(side = 3, outer = TRUE,
      text = "All values are in percent",
      line = -1, font = 3, adj = 1, cex = 0.7, col = "gray40")
## title
mtext(side = 1, outer = TRUE,
      text = "Source: World Development Indicators, WB", line = 0,
      font = 3, adj = 1, cex = 0.8, col = "gray40")
par(fig = c(0, 1, 0, 1),
    oma = c(2, 1, 3.5, 1),
    mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", inset = c(0, 0), xpd = TRUE, cex = 0.8,  text.col = "gray20",
       lty = 1, col = c("red", "blue"), horiz = TRUE,
       legend = c("Vietnam", "Average"), bty = "n")

dev.off()

## 16.24542 16.20278 (cm)
