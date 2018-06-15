

library(readxl)

setwd("~/Documents/malnutrition-children/data/")

dd1999 <- read_xls("dinhduong1999.xls", skip = 2)

head(dd1999)


View(dd1999)


## -----------------------------------------------------------------------------
## Build sample size data set


ss1999 <- dd1999[, 1:3]
names(ss1999) <- c("stt", "province", "n")
ss1999 <- ss1999[!ss1999$stt %in% romans & !is.na(ss1999$stt), ]


extractSampleSize <- function(dtf) {
    romans <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII")
    out <- dtf[, 1:3]
    names(out) <- c("stt", "province", "n")
    out <- out[!out$stt %in% romans & !is.na(out$stt), ]
    out
}

extractSampleSize(dd1999)
