


profs <- read.csv("~/Dropbox/intro_visualization/demo/professors.csv",
                  stringsAsFactors = F)

## dim(profs)
## head(profs)

profs <- subset(profs, !ngaysinh %in% c("", "119-05-28", "243-03-26", "339-02-24"))
profs$age <- profs$nam - as.numeric(substr(profs$ngaysinh, 1, 4))

## -----------------------------------------------------------------------------
## Lines chart (time series)
## -----------------------------------------------------------------------------

profs_per_year <- aggregate(profs$nam, list(nam = profs$nam), length)

## default
plot(profs_per_year$nam , profs_per_year$x, type = "l")


## dots and lines
plot(profs_per_year$nam , profs_per_year$x, type = "b")

## add colors, change line width and add title, labels
plot(profs_per_year$nam , profs_per_year$x, type = "l", lwd = 2, col = "red4",
     xlab = "Year", ylab = NA, main = "Vietnamese Professors, 1980-2016")


## variants
plot(profs_per_year$nam , profs_per_year$x, type = "s", lwd = 2, col = "red4",
     xlab = "Year", ylab = NA, main = "Vietnamese Professors, 1980-2016")

plot(profs_per_year$nam , profs_per_year$x, type = "h", lwd = 2, col = "red4",
     xlab = "Year", ylab = NA, main = "Vietnamese Professors, 1980-2016")

plot(profs_per_year$nam , profs_per_year$x, type = "S", lwd = 2, col = "red4",
     xlab = "Year", ylab = NA, main = "Vietnamese Professors, 1980-2016")

## -----------------------------------------------------------------------------
## Bar chart
## -----------------------------------------------------------------------------

## default
barplot(profs_per_year$x, names.arg = profs_per_year$nam)

## horizontal bar chart
barplot(profs_per_year$x, names.arg = profs_per_year$nam, horiz = T)

## horizontal bar chart, rotate names
barplot(profs_per_year$x, names.arg = profs_per_year$nam, horiz = T, las = 1)

## disable border
barplot(profs_per_year$x, names.arg = profs_per_year$nam,
        horiz = T, border = F)

## change color
barplot(profs_per_year$x, names.arg = profs_per_year$nam,
        border = F, col = "cadetblue")

barplot(profs_per_year$x, names.arg = profs_per_year$nam,
        border = F, col = colors())

barplot(profs_per_year$x, names.arg = profs_per_year$nam,
        border = F, col = gray.colors(nrow(profs_per_year)))

## texture
barplot(profs_per_year$x, names.arg = profs_per_year$nam,
        border = "gray", density=c(12), col = "orange")
