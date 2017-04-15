
library(reshape2)

profs <- read.csv("~/Dropbox/intro_visualization/demo/professors.csv",
                  stringsAsFactors = F)

## dim(profs)
## head(profs)

profs <- subset(profs, !ngaysinh %in% c("", "119-05-28", "243-03-26", "339-02-24"))
profs$age <- profs$nam - as.numeric(substr(profs$ngaysinh, 1, 4))


## -----------------------------------------------------------------------------
## SCATTER PLOT
## -----------------------------------------------------------------------------

by_title <- aggregate(profs$title,
                      list(title = profs$title,
                           nam = profs$nam),
                      length)

by_title <- dcast(by_title, nam ~ title)

## default
plot(by_title$GS, by_title$PGS)


## change point shapes
plot(by_title$GS, by_title$PGS, pch = 19)

plot(by_title$GS, by_title$PGS, pch = 19,
     xlab = "GS", ylab = "PGS")

## add text
plot(by_title$GS, by_title$PGS, pch = 19, xlab = "GS", ylab = "PGS")
text(by_title$GS + 2, by_title$PGS, labels = by_title$nam, adj = 0)


plot(by_title$GS, by_title$PGS, pch = 19, xlab = "GS", ylab = "PGS")
text(by_title$GS + 2, by_title$PGS, labels = by_title$nam, adj = 0, xpd = T)


## add grid
plot(by_title$GS, by_title$PGS, pch = 19, xlab = "GS", ylab = "PGS")
text(by_title$GS + 2, by_title$PGS, labels = by_title$nam, adj = 0, xpd = T)
grid()
