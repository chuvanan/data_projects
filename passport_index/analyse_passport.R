

passport <- read.csv("~/Documents/data_projects/passport_index/passport_index.csv",
                     stringsAsFactors = FALSE)

passport <- subset(passport, hdi > 0)
vietnam <- subset(passport, countries == "Viet Nam")
germany <- subset(passport, countries == "Germany")
solomon <- subset(passport, countries == "Solomon Islands")
worst <- subset(passport, countries %in% c("Syria", "Iraq", "Pakistan", "Afghanistan"))

model <- lm(passport$vfs ~ passport$hdi)

png("~/Documents/data_projects/passport_index/passport_power_ranking.png",
    width = 566, height = 576, res = 90)

par(xaxs = "i", yaxs = "i", mar = c(4, 3.5, 4, 1),
    tck = -0.015)
plot(passport$hdi, passport$vfs,
     pch = 19, col = rgb(0, 0, 0, alpha = 0.5),
     xlim = c(0.3, 1), ylim = c(10, 170), axes = FALSE,
     xlab = "", ylab = "")
grid(NULL, NULL, equilogs = TRUE)
points(vietnam$hdi, vietnam$vfs, col = "red", pch = 19)
text(vietnam$hdi, vietnam$vfs - 4, labels = "Vietnam", col = "red", cex = 0.8)
text(germany$hdi, germany$vfs + 4, labels = "Germany",
     col = rgb(0, 0, 0, alpha = 0.5), cex = 0.8)
text(worst$hdi, worst$vfs - 4, labels = worst$countries,
     col = rgb(0, 0, 0, alpha = 0.5), cex = 0.8)
text(solomon$hdi, solomon$vfs + 4, labels = "Solomon Islands",
     col = rgb(0, 0, 0, alpha = 0.5), cex = 0.8)
abline(model, col = "red4", lwd = 2, lty = 2)
axis(1, padj = -0.5, cex.axis = 0.85, col = "gray20", col.axis = "gray20")
axis(2, las = 1, at = seq(20, 160, 20), cex.axis = 0.85, col = "gray20", col.axis = "gray20")
mtext(side = 1, text = "UNDP's HDI", line = 2, cex = 0.85, col = "gray20")
mtext(side = 1, text = "Source: Passport Index", line = 2.5, adj = 1,
      cex = 0.85, col = "gray20")
mtext(side = 2, text = "Visa-Free Score", line = 2.5, cex = 0.85, col = "gray20")
mtext(side = 3, text = "Individual Passport Power Rank 2017",
      line = 2, adj = 0, font = 2, col = "gray20", cex = 1.2)
mtext(side= 3, text = "Higher score, higher ranking",
      line = 0.75, adj = 0, col = "gray20", cex= 0.85, font = 3)
box(col = "gray20")

dev.off()




