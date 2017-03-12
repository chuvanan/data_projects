



agenda <- data.frame(
  x = rep(0:6),
  y = c(0:6)
)

## cairo_pdf("~/Dropbox/R_meetups/agenda.pdf", width=9, height=6.75)

## par(family = "Lato Light")
## plot(agenda, ann = F, type = "n",
##      ylim = c(0.5, 3.5), xlim = c(0, 20),
##      axes = F, bty = "n")
## rect(0, 0.5, 1, 1, col = "#440154FF", border = "white")
## text(1.2, 0.75, "Networking", adj = 0, font = 2, cex = 1.6, col = "#440154FF")
## rect(0, 1, 1, 1.5, col = "#21908CFF", border = "white")
## rect(0, 1.5, 1, 2, col = "#21908CFF", border = "white")
## text(1.2, 1.75, "Discussion", adj = 0, font = 2, cex = 1.6, col = "#21908CFF")
## rect(0, 2, 1, 2.5, col = "#FDE725FF", border = "white")
## rect(0, 2.5, 1, 3, col = "#FDE725FF", border = "white")
## rect(0, 3, 1, 3.5, col = "#FDE725FF", border = "white")
## text(1.2, 3.25, labels = "Lightning talks", adj = 0, font = 2, cex = 1.6, col = "#FDE725FF")
## mtext(side = 3, text = "Agenda", cex = 2, line = 1.5, adj = 0.1, font = 2)

## dev.off()

cairo_pdf("~/Dropbox/R_meetups/agenda.pdf", width=9, height=6.75)
par(family = "Lato Light", mar = c(2, 2, 4, 2))
plot(agenda, ann = F, type = "n", bty = "n", axes = F)
rect(0, 4, 1, 5, col = "#440154FF", border = "white")
rect(1, 4, 2, 5, col = "#440154FF", border = "white")
rect(2, 4, 3, 5, col = "#440154FF", border = "white")
text(0, 5.5, "Lightning talks", adj = 0, cex = 1.6, col = "#440154FF", font = 2)
rect(3, 4, 4, 5, col = "#21908CFF", border = "white")
rect(4, 4, 5, 5, col = "#21908CFF", border = "white")
text(3, 5.5, "Discussion", adj = 0, cex = 1.6, col = "#21908CFF", font = 2)
rect(5, 4, 6, 5, col = "#FDE725FF", border = "white")
text(5, 5.5, "Networking", adj = 0, cex = 1.6, col = "#FDE725FF", font = 2)
rect(2.5, 2, 3.5, 3)
text(3, 2.5, "30 mins", adj = 0.5, cex = 1.6, font = 3)
mtext("Agenda", line = 1.5, adj = 0.5, font = 2, cex = 2)
dev.off()

