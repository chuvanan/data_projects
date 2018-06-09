

setwd("~/Documents/diem-thi-lop10-2017/")

dthi.chuyen <- readxl::read_xls("diem-ts10-2017baochi-1497260000353.xls", sheet = 1)

dthi.thuong <- readxl::read_xls("diem-ts10-2017baochi-1497260000353.xls", sheet = 2)


## anonymizing data

dthi.chuyen$Ho <- dthi.chuyen$Ten <- NULL

names(dthi.chuyen) <- c("sbd", "van", "ngoaingu", "toan", "chuyen")

dthi.thuong$Ho <- dthi.thuong$Ten <- NULL

names(dthi.thuong) <- c("sbd", "van", "ngoaingu", "toan")


## remove invalid values

dthi.chuyen <- subset(dthi.chuyen, van > 0 & ngoaingu > 0 & toan > 0 & chuyen > 0)

dthi.thuong <- subset(dthi.thuong, van > 0 & ngoaingu > 0 & toan > 0)

## export data

write.csv(dthi.chuyen, "diem-ts10-lopchuyen.csv", row.names = FALSE)

write.csv(dthi.thuong, "diem-ts10-lopthuong.csv", row.names = FALSE)
