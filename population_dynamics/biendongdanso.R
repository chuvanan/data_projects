

library(reshape2)

pop_io <- read.csv2("~/Dropbox/Public/biendongdanso.csv",
                    skip = 3,
                    stringsAsFactors = FALSE)

names(pop_io) <- c("province",
                   paste0("nc_", c(2005, 2007:2015)),
                   paste0("dc_", c(2005, 2007:2015)),
                   paste0("dct_", c(2005, 2007:2015)))

pop_io[] <- lapply(pop_io, function(x) {x[x == ".."] <- NA; x})
pop_io[, -1] <- lapply(pop_io[, -1], as.double)


pop_io <- melt(pop_io,
               id.vars = "province",
               variable.name = "type",
               value.name = "value",
               factorsAsStrings = TRUE)

pop_io$type <- as.character(pop_io$type)
pop_io$year <- unlist(lapply(strsplit(pop_io$type, split = "_"), function(x) x[2]))
pop_io$type <- unlist(lapply(strsplit(pop_io$type, split = "_"), function(x) x[1]))
pop_io$year <- as.integer(pop_io$year)

pop_io$type_lb[pop_io$type == "dct"] <- "Tỷ suất Di cư thuần"
pop_io$type_lb[pop_io$type == "dc"] <- "Tỷ suất Di cư"
pop_io$type_lb[pop_io$type == "nc"] <- "Tỷ suất Nhập cư"

pop_io <- subset(pop_io, !province %in% c("Bắc Trung Bộ và duyên hải miền Trung",
                                          "CẢ NƯỚC",
                                          "Đồng bằng sông Cửu Long",
                                          "Đồng bằng sông Hồng",
                                          "Trung du và miền núi phía Bắc",
                                          "Từ năm 2008 tới nay, số liệu thành phố Hà Nội bao gồm cả tỉnh ",
                                          "Hà Tây.",
                                          "Đông Nam Bộ"))

hanoi <- subset(pop_io, province == "Hà Nội" & type == "nc")
hanoi_dc <- subset(pop_io, province == "Hà Nội" & type == "dc")




par(mar = c(4, 3, 3, 1), bg = "gray95")
plot(hanoi$year, hanoi$value, axes = FALSE, xlab = "", ylab = "",
     ylim = c(2, 16))
grid(NULL, NULL)
points(hanoi$year, hanoi$value, col = "red4", type = "b", lwd = 2)
points(hanoi_dc$year, hanoi_dc$value, col = "blue", type = "b", lwd = 2)
mtext(side = 3, text = "Tỷ suất nhập cư - Hà Nội", line = 1, font = 2, adj = 0)
mtext(side = 1, text = "Nguồn: GSO", adj = 1, line = 2.5, cex = 0.8, col = "gray20")
axis(1, tick = FALSE, at = c(2005, 2007:2015),
     labels = c("2005", "'07", "'08", "'09", "'10", "'11", "'12", "'13", "'14", "'15"),
     cex.axis = 0.8, col.axis = "gray20")
axis(2, tick = FALSE, las = 1, cex.axis = 0.8, col.axis = "gray20",
     at = seq(2, 16, by = 2), labels = paste0(seq(2, 16, by = 2), "%"))
