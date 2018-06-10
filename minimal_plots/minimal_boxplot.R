

## data(quakes)

## x <- quakes$mag
## y <- quakes$stations

## cairo_pdf("~/Dropbox/minimal_boxplot.pdf", width = 6.989583, height = 6.992017)
## par(bg = "gray98", family = "Lato Light")
## boxplot(y ~ x, main = "", axes = FALSE, xlab=" ", ylab=" ",
##         pars = list(boxcol = "transparent", medlty = "blank", medpch=19, whisklty = c(1, 1),
##                     medcex = 0.7,  outcex = 0, staplelty = "blank"))
## axis(1, at=1:length(unique(x)), label=sort(unique(x)), tick=F, cex.axis = 0.9)
## axis(2, las=2, tick=F, cex.axis = 0.9)
## text(min(x)/3, max(y)/1.1, pos = 4, font = 2,
##      "Number of stations \nreporting Richter Magnitude\nof Fiji earthquakes (n=1000)")
## dev.off()

## dev.size("in")


## x <- 1967:1977
## y <- c(0.5,1.8,4.6,5.3,5.3,5.7,5.4,5,5.5,6,5)

## cairo_pdf("~/Dropbox/minimal_lineplot.pdf", width = 7.114583, height = 5.921916)
## par(bg = "gray98", family = "Lato Light")
## plot(y ~ x, axes=F, xlab="", ylab="", pch=19, type="b")
## axis(1, at=x, label=x, tick=F, cex.axis = 0.9)
## axis(2, at=seq(1,6,1), label=sprintf("$%s", seq(300,400,20)), tick=F, las=2, cex.axis = 0.9)
## abline(h=6,lty=2)
## abline(h=5,lty=2)
## text(max(x), min(y)*2.5,"Per capita\nbudget expanditures\nin constant dollars", adj=1, font = 2, cex = 0.9)
## text(max(x), max(y)/1.08, labels="5%")
## dev.off()

## dev.size("in")


## x <- mtcars$wt
## y <- mtcars$mpg

## cairo_pdf("~/Dropbox/rangeframe_plot.pdf", width = 6.843750, height = 6.306321)
## par(bg = "gray98", family = "Lato Light")
## plot(x, y, main="", axes=FALSE, pch=19, cex=0.8,
##      xlab="Car weight (lb/1000)", ylab="Miles per gallon of fuel")
## axis(1,at=summary(x),labels=round(summary(x),1), tick=F, cex.axis = 0.9)
## axis(2,at=summary(y),labels=round(summary(y),1), tick=F, las=2, cex.axis = 0.9)
## dev.off()

## dev.size("in")


## library(RCurl)
## dd <- read.csv(text = getURL("https://gist.githubusercontent.com/GeekOnAcid/da022affd36310c96cd4/raw/9c2ac2b033979fcf14a8d9b2e3e390a4bcc6f0e3/us_nr_of_crimes_1960_2014.csv"))
## d <- dd[,c(2:11)]


## par(mfrow=c(ncol(d),1), mar=c(1,0,0,8), oma=c(4,1,4,4))
## for (i in 1:ncol(d)){
##   plot(d[,i], lwd=0.5, axes=F, ylab="", xlab="", main="", type="l", new=F)
##   axis(4, at=d[nrow(d),i], labels=round(d[nrow(d),i]), tick=F, las=1, line=-1.5,
##        family="serif", cex.axis=1.2)
##   axis(4, at=d[nrow(d),i], labels=names(d[i]), tick=F, line=1.5,
##        family="serif", cex.axis=1.4, las=1)
##   text(which.max(d[,i]), max(d[,i]), labels=round(max(d[,i]),0),
##        family="serif", cex=1.2, adj=c(0.5,3))
##   text(which.min(d[,i]), min(d[,i]), labels=round(min(d[,i]),0),
##        family="serif", cex=1.2, adj=c(0.5,-2.5))
##   ymin <- min(d[,i]); tmin <- which.min(d[,i]); ymax<-max(d[,i]); tmax<-which.max(d[,i]);
##   points(x=c(tmin,tmax), y=c(ymin,ymax), pch=19, col=c("red","blue"), cex=1)
##   rect(0, summary(d[,i])[2], nrow(d), summary(d[,i])[4], border=0,
##        col = rgb(190, 190, 190, alpha=90, maxColorValue=255))}
## axis(1, at=1:nrow(dd), labels=dd$Year, pos=c(-5), tick=F, family="serif", cex.axis=1.4)
