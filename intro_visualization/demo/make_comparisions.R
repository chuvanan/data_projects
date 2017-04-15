



profs <- read.csv("~/Dropbox/intro_visualization/demo/professors.csv",
                  stringsAsFactors = F)

## dim(profs)
## head(profs)

profs <- subset(profs, !ngaysinh %in% c("", "119-05-28", "243-03-26", "339-02-24"))
profs$age <- profs$nam - as.numeric(substr(profs$ngaysinh, 1, 4))


## -----------------------------------------------------------------------------
## BAR CHART
## -----------------------------------------------------------------------------

## count by field
by_field <- aggregate(profs$nganh, list(nganh = profs$nganh), length)
by_field <- by_field[order(by_field$x), ]


## default
barplot(by_field$x, horiz = TRUE)

## add names
barplot(by_field$x, names.arg = by_field$nganh, horiz = TRUE, las = 1)

## remove borders
barplot(by_field$x, names.arg = by_field$nganh, horiz = TRUE, las = 1,
        border = F)

## add colors
barplot(by_field$x, names.arg = by_field$nganh, horiz = TRUE, las = 1,
        border = F, col = "steelblue")

## add grid
barplot(by_field$x, names.arg = by_field$nganh, horiz = TRUE, las = 1,
        border = F, col = "steelblue")
grid(NULL, NA, lwd = 1, lty = 1)

## set x-axis limits
barplot(by_field$x, names.arg = by_field$nganh, horiz = TRUE, las = 1,
        border = F, col = "steelblue", xlim = c(0, 2000))
grid(NULL, NA, lwd = 1, lty = 1)

## add titles
barplot(by_field$x, names.arg = by_field$nganh, horiz = TRUE, las = 1,
        border = F, col = "steelblue", xlim = c(0, 2000))
grid(NULL, NA, lwd = 1, lty = 1)
title(main = "Thống kê số GS/PGS theo ngành, 1980-2016")


## -----------------------------------------------------------------------------

by_title <- table(profs$title, profs$nam)

## stacked bar chart
barplot(by_title, border = F)

## add legend
barplot(by_title, border = F, legend = rownames(by_title))

## grouped bar chart
barplot(by_title, border = F, besid = T, legend = rownames(by_title))

## color
barplot(by_title, border = F, besid = T,
        legend = rownames(by_title), col = c("red4", "cadetblue"))

barplot(by_title, border = F,
        legend = rownames(by_title), col = c("red4", "cadetblue"))

## -----------------------------------------------------------------------------
## DOT CHART
## -----------------------------------------------------------------------------

## default
dotchart(by_field$x, labels = by_field$nganh)

## change point shape
dotchart(by_field$x, labels = by_field$nganh, pch = 19)

## add labels
dotchart(by_field$x, labels = by_field$nganh, pch = 19,
         main = "Thống kê số GS/PGS theo ngành, 1980-2016")
