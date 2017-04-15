## Critique by redesign
## Case 1
## http://vnexpress.net/infographics/du-hoc/ty-le-trung-tuyen-cac-dai-hoc-ivy-league-nien-khoa-2017-2021-3563507.html


## -----------------------------------------------------------------------------
## CLEAN DATA
## -----------------------------------------------------------------------------

## import data
dta <- read.csv("tyletrungtuyen.csv", stringsAsFactors = F)

## calculate acceptance rate
dta$rate <- round(dta$trungtuyen * 100 / dta$ungtuyen, 1)
dta$total <- 100
dta <- dta[order(dta$rate, decreasing = T), ]

## -----------------------------------------------------------------------------
## VISUALIZE DATA
## -----------------------------------------------------------------------------

## first try
barplot(dta$rate, names.arg = dta$truong)

## make it horizontal and remove bar border, add color
barplot(dta$rate, names.arg = dta$truong, horiz = T, border = F, col = "cadetblue")

## rotate labes
barplot(dta$rate, names.arg = dta$truong, horiz = T, las = 1, border = F, col = "cadetblue")

## add another barchart
barplot(dta$total, horiz = T, las = 1, border = F)
barplot(dta$rate, names.arg = dta$truong, horiz = T, las = 1, border = F, col = "cadetblue", add = T)

## remove labels
barplot(dta$total, horiz = T, border = F, xaxt = "n")
barplot(dta$rate, horiz = T, border = F, col = "cadetblue", add = T, xaxt = "n")

## make margin larger and get tick marks location
par(mar = c(2, 9, 4, 2))
bp <- barplot(dta$total, horiz = T, border = F, xaxt = "n") # get tick mark location
barplot(dta$rate, horiz = T, border = F, col = "cadetblue", add = T, xaxt = "n")

## add 1st label
bp <- barplot(dta$total, horiz = T, border = F, xaxt = "n")
barplot(dta$rate, horiz = T, border = F, col = "cadetblue", add = T, xaxt = "n")
axis(2, at = bp, labels = paste0(dta$rate, "%"), las = 1, tick = F, line = -1, cex.axis = 0.85)

## add 2nd label
bp <- barplot(dta$total, horiz = T, border = F, xaxt = "n")
barplot(dta$rate, horiz = T, border = F, col = "cadetblue", add = T, xaxt = "n")
axis(2, at = bp, labels = dta$truong, las = 1, tick = F, line = 2, cex.axis = 0.95)
axis(2, at = bp, labels = paste0(dta$rate, "%"), las = 1, tick = F, line = -1, cex.axis = 0.85)

## add title
bp <- barplot(dta$total, horiz = T, border = F, xaxt = "n", col = "gray92")
barplot(dta$rate, horiz = T, border = F, col = "cadetblue", add = T, xaxt = "n")
axis(2, at = bp, labels = dta$truong, las = 1, tick = F, line = 2, cex.axis = 0.95)
axis(2, at = bp, labels = paste0(dta$rate, "%"), las = 1, tick = F, line = -1, cex.axis = 0.85)
mtext("Các trường đại học cạnh tranh nhất nước Mỹ", line = 2.5, adj = 0, cex = 1.2, font = 2) # title

## add subt-title
bp <- barplot(dta$total, horiz = T, border = F, xaxt = "n", col = "gray92")
barplot(dta$rate, horiz = T, border = F, col = "cadetblue", add = T, xaxt = "n")
axis(2, at = bp, labels = dta$truong, las = 1, tick = F, line = 2, cex.axis = 0.95)
axis(2, at = bp, labels = paste0(dta$rate, "%"), las = 1, tick = F, line = -1, cex.axis = 0.85)
mtext("Các trường đại học cạnh tranh nhất nước Mỹ", line = 2.5, adj = 0, cex = 1.2, font = 2) # title
mtext("So sánh dựa trên tỷ lệ trúng tuyển niên khóa 2017-2021", line = 1, adj = 0, font = 3, cex = 0.9) # subtitle

## add source
bp <- barplot(dta$total, horiz = T, border = F, xaxt = "n", col = "gray92")
barplot(dta$rate, horiz = T, border = F, col = "cadetblue", add = T, xaxt = "n")
axis(2, at = bp, labels = dta$truong, las = 1, tick = F, line = 2, cex.axis = 0.95)
axis(2, at = bp, labels = paste0(dta$rate, "%"), las = 1, tick = F, line = -1, cex.axis = 0.85)
mtext(text = "Các trường đại học cạnh tranh nhất nước Mỹ", line = 2.5, adj = 0, cex = 1.2, font = 2) # title
mtext(text ="So sánh dựa trên tỷ lệ trúng tuyển niên khóa 2017-2021", line = 1, adj = 0, font = 3, cex = 0.9) # subtitle
mtext(text = "Nguồn: Business Insider", adj = 1, font = 3, cex = 0.9, side = 1, col = "gray30", line = 0.5) # source

## -----------------------------------------------------------------------------
## EXPORT
## -----------------------------------------------------------------------------

pdf("tyletrungtuyen.pdf", width=6.791667, height=4.560914)

bp <- barplot(dta$total, horiz = T, border = F, xaxt = "n", col = "gray92")
barplot(dta$rate, horiz = T, border = F, col = "cadetblue", add = T, xaxt = "n")
axis(2, at = bp, labels = dta$truong, las = 1, tick = F, line = 2, cex.axis = 0.95)
axis(2, at = bp, labels = paste0(dta$rate, "%"), las = 1, tick = F, line = -1, cex.axis = 0.85)
mtext(text = "Các trường đại học cạnh tranh nhất nước Mỹ", line = 2.5, adj = 0, cex = 1.2, font = 2) # title
mtext(text ="So sánh dựa trên tỷ lệ trúng tuyển niên khóa 2017-2021", line = 1, adj = 0, font = 3, cex = 0.9) # subtitle
mtext(text = "Nguồn: Business Insider", adj = 1, font = 3, cex = 0.9, side = 1, col = "gray30", line = 0.5) # source

dev.off()
