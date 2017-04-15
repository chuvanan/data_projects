
library(reshape2)

out <- read.csv("~/Dropbox/intro_visualization/demo/nhapkhau_oto.csv",
                stringsAsFactors = F)
out <- melt(out, id.vars = "country", variable.name = "year", value.name = "n")
out$year <- gsub("m2_", "", out$year)
countries <- unique(out$country)


## first try
plot(out$year, out$n)

## add scales
plot(out$year, out$n, type = "n", ann = F, axes = F)
axis(1, tick = F)


## add individual lines
plot(out$year, out$n, type = "n", ann = F, axes = F)
axis(1, tick = F)
for (i in countries) {
  temp <- subset(out, country == i)
  lines(temp$year, temp$n)
}


## add labels
plot(out$year, out$n, type = "n", ann = F, axes = F)
axis(1, tick = F)
for (i in countries) {
  temp <- subset(out, country == i)
  lines(temp$year, temp$n)
  text(2015.8, temp$n[temp$year == 2016], labels = temp$country[temp$year == 2016], xpd = T)
}



## add labels
plot(out$year, out$n, type = "n", ann = F, axes = F)
axis(1, tick = F)
for (i in countries) {
  temp <- subset(out, country == i)
  lines(temp$year, temp$n)
  text(2015.9, temp$n[temp$year == 2016],
       labels = temp$country[temp$year == 2016],
       xpd = T, adj = 1)
}


## make margin larger
par(mar = c(3, 7, 3, 4))
plot(out$year, out$n, type = "n", ann = F, axes = F)
axis(1, tick = F, at = c(2016, 2017))
for (i in countries) {
  temp <- subset(out, country == i)
  lines(temp$year, temp$n)
  text(2015.9, temp$n[temp$year == 2016],
       labels = temp$country[temp$year == 2016],
       xpd = T, adj = 1)
}



## add axis
par(mar = c(3, 7, 3, 4))
plot(out$year, out$n, type = "n", ann = F, axes = F)
axis(1, tick = F, at = c(2016, 2017))
axis(4, las = 1, tick = F)
for (i in countries) {
  temp <- subset(out, country == i)
  lines(temp$year, temp$n)
  text(2015.9, temp$n[temp$year == 2016],
       labels = temp$country[temp$year == 2016],
       xpd = T, adj = 1)
}
mtext("Các quốc gia xuất ô tô sang Việt Nam", adj = 0, cex = 1.2, line = 1)


## add grid
par(mar = c(3, 7, 3, 4))
plot(out$year, out$n, type = "n", ann = F, axes = F)
axis(1, tick = F, at = c(2016, 2017))
axis(4, las = 1, tick = F)
for (i in countries) {
  temp <- subset(out, country == i)
  lines(temp$year, temp$n)
  text(2015.9, temp$n[temp$year == 2016],
       labels = temp$country[temp$year == 2016],
       xpd = T, adj = 1)
}
mtext("Các quốc gia xuất ô tô sang Việt Nam", adj = 0, cex = 1.2, line = 1)
abline(v = c(2016, 2017), lty = 2, lwd = 0.1)
abline(h = seq(0, 6000, 1000), lty = 2, lwd = 0.1)


