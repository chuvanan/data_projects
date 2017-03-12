

library(viridis)


participants <- read.csv("~/Dropbox/R_meetups/FormResponses.csv",
                         stringsAsFactors = F)

from <- participants$Type

cairo_pdf("~/Dropbox/R_meetups/participants.pdf", width=6.773354, height=6.535228)

par(bg = "white", family = "Lato Light",
    mar = c(2, 2, 3, 2), font = 2, cex = 1.4)
pie(prop.table(table(from)), labels = c("Academia", "Industry"),
    border = 0, init.angle = 90, col = c("#21908CFF", "#FDE725FF"))
mtext(side = 3, text = "HUGM's registers (N = 41)", font = 2, cex = 2)
mtext(side = 1, text = "Source: Google Form/Hanoi UseR Meetup | By: @anchu",
      adj = 1, cex = 1.2, col = "gray20", font = 3)

dev.off()


## dev.size("in")                          # 6.773354 6.535228
