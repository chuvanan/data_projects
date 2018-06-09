

## -----------------------------------------------------------------------------
## get data in

if (interactive()) {
    rm(list = ls())
    gc(reset = TRUE)
}

setwd("~/Documents/diem-thi-lop10-2017/")

dt.chuyen <- read.csv("diem-ts10-lopchuyen.csv", stringsAsFactors = FALSE)

dt.thuong <- read.csv("diem-ts10-lopthuong.csv", stringsAsFactors = FALSE)

## -----------------------------------------------------------------------------
## explore data


## distribution of scores for each subject

dt.toan <- table(dt.thuong$toan, dnn = "diem")
dt.toan <- as.data.frame(dt.toan, responseName = "n_toan", stringsAsFactors = FALSE)
dt.toan$diem <- as.double(dt.toan$diem)

dt.van <- table(dt.thuong$van, dnn = "diem")
dt.van <- as.data.frame(dt.van, responseName = "n_van", stringsAsFactors = FALSE)
dt.van$diem <- as.double(dt.van$diem)

dt.nn <- table(dt.thuong$ngoaingu, dnn = "diem")
dt.nn <- as.data.frame(dt.nn, responseName = "n_nn", stringsAsFactors = FALSE)
dt.nn$diem <- as.double(dt.nn$diem)


## fix the zero bug

dt.toan <- merge(dt.toan,
                 data.frame(diem = seq(0, 10, 0.25)),
                 by = "diem",
                 all = TRUE)
dt.toan$n_toan[is.na(dt.toan$n_toan)] <- 0

dt.van <- merge(dt.van,
                data.frame(diem = seq(0, 10, 0.25)),
                by = "diem",
                all = TRUE)
dt.van$n_van[is.na(dt.van$n_van)] <- 0

dt.nn <- merge(dt.nn,
               data.frame(diem = seq(0, 10, 0.25)),
               by = "diem",
               all = TRUE)
dt.nn$n_nn[is.na(dt.nn$n_nn)] <- 0

## compare distributions


y.range <- range(dt.toan$n_toan, dt.van$n_van, dt.nn$n_nn)
van.col <- adjustcolor("blue", alpha.f = 0.4)
toan.col <- adjustcolor("red4", alpha.f = 0.4)
nn.col <- adjustcolor("forestgreen", alpha.f = 0.4)

png("pho-diem-thi.png", width = 464, height = 664, res = 97)

par(mfrow = c(3, 1),
    mar = c(0, 0, 0, 0),
    oma = c(3, 2, 4, 2),
    bg = "gray98",
    family = "Lato")

barplot(dt.van$n_van, col = van.col,
        axes = FALSE, border = FALSE,
        ylim = y.range)
axis(1, col = "gray98", col.ticks = "gray30", labels = FALSE)
mtext("Văn", line = -3, adj = 0)

barplot(dt.toan$n_toan, col = toan.col,
        axes = FALSE, border = FALSE,
        ylim = y.range)
axis(1, col = "gray98", col.ticks = "gray30", labels = FALSE)
mtext("Toán", line = -3, adj = 0)

barplot(dt.nn$n_nn, col = nn.col,
        axes = FALSE, border = FALSE,
        ylim = y.range)
axis(1, col = "gray98", col.ticks = "gray30", at = seq(0, 50, 10), labels = seq(0, 10, 2))
mtext("Ngoại ngữ", line = -3, adj = 0)

## tile
mtext("Phổ điểm thi lớp 10 - khóa 2017 (TP. Hồ Chí Minh)", outer = TRUE, line = 2, font = 2)

dev.off()

## dev.size("px")
## 464 664

## dev.size("in")
## 4.833333 6.898513
