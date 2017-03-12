## Map of R User Groups
## Feb 26, 2017

library(maps)

meetups <- read.csv("~/Dropbox/R_meetups/Rmeetups.csv", stringsAsFactors = F)
hanoi <- subset(meetups, cities == "Ha Noi")

## cairo_pdf("~/Dropbox/R_meetups/Rmeetups.pdf", width=9.322466, height=5.504993)
## png("~/Dropbox/R_meetups/Rmeetups.png", width=896, height=529, res =100)
cairo_pdf("~/Dropbox/R_meetups/Rmeetups.pdf", width=9, height=6.75)

par(family = "Lato Light", bg = "white")
map("world", lwd = 0.5, boundary = T, bg = "white", col = "gray40")
points(meetups$lon, meetups$lat, cex = 0.6, pch = 19,
       col = rgb(0, 0, 255, max = 255, alpha = 100))
points(hanoi$lon, hanoi$lat, col = "red", cex = 1, pch = 19)
mtext(side = 3, text = "R User Groups Worldwide", adj = 0.5,
      line = 2, cex = 1.4, font = 2)
mtext(side = 3, text = "02/2017", adj = 0.5,
      line = 0.5, cex = 0.9, font = 3)
mtext(side = 1, text = "Source: meetup.com | By: @anchu", adj = 1,
      line = 2, font = 3, col = "gray20", cex = 0.9)

dev.off()
## dev.size("px")
## 9.322466 5.504993 (inches)
## 896 529 (pixels)
