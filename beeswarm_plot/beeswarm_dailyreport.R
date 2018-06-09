
library(beeswarm)

report <- read.csv("~/Dropbox/rta_dailyreport/RTA_Daily_Report.csv",
                   stringsAsFactors = FALSE)

report <- subset(report, select = c(starttime, staff_code, rta_department,
                                    rta_title, rta_name))
report <- subset(report, staff_code != "")
report <- subset(report, rta_department != "HR - Admin")

report$starttime <- as.POSIXlt(report$starttime)
report$hour <- report$starttime$hour
report$min <- report$starttime$min
report$min <- report$min/60
report$hour <- report$hour + report$min

rt_lab <- subset(report, rta_department == "RT Lab")
rt_solution <- subset(report, rta_department == "rtSolutions Team")
rt_research <- subset(report, rta_department == "Research & Analytics")

report <- subset(report, hour >= 8)

cairo_pdf("~/Dropbox/beeswarm_dailyreport.pdf", width=6.544454, height=6.441570)

par(bg = "gray98", family = "Lato Light", mar = c(3.5, 3, 4, 2.5))
plot(0, 0, xlim = c(0.5, 3.5), ylim = c(8, 24), bty = "n", ann = F, axes = F,
     type = "n", xaxs = "i")
rect(0, 8, 1.5, 24, col = rgb(34, 139, 34, max = 255, alpha = 30), border = NA)
rect(1.5, 8, 2.5, 24, col = rgb(255, 165, 0, max = 255, alpha = 30), border = NA)
rect(2.5, 8, 3.5, 24, col = rgb(255, 105, 180, max = 255, alpha = 30), border = NA)
abline(h = seq(8, 24, 2), col = "white")
beeswarm(hour ~ rta_department, data = report, add = T,
         priority = "ascending", pch = 21,
         axes = F, ann = F, bg = c("forestgreen", "orange", "hotpink"),
         col = "gray30", ylim = c(8, 24))
axis(side = 1, at = c(1, 2, 3), labels = c("R&A", "rtLab", "rtSolution"),
     lwd = 0, font = 2, padj = -1.5)
axis(side = 2, col = "gray98", col.ticks = "gray98", tck = -0.01,
     at = seq(8, 24, 2), labels = paste0(seq(8, 24, 2), "h"),
     cex.axis = 0.75, col.axis = "gray40", xpd = T, las = 1, hadj = 0.8)
mtext(side = 3, text = "Distribution of daily report submissions",
      font = 2, line = 2.5, cex = 1.2, adj = 0)
mtext(side = 3, text = "An introspection of RTA's reporting system", font = 3,
      line = 1, cex = 1, adj = 0, col = "gray10")
mtext(side = 1, text = "Source: Form RTA_Daily_Report", font = 3,
      line = 2, cex = 0.8, adj = 1, col = "gray40")
text(x = 3.5, y = 24.5, labels = "Each dot is a submission", xpd = T, adj = 1,
     col = "gray40", cex = 0.8, font = 3)

dev.off()
