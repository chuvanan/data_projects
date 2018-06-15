

setwd("~/Documents/vietnam-forest-fire/data/")

admins.unit <- read.csv("danh-muc-cap-tinh.csv", stringsAsFactors = FALSE)

admins.unit$`Tên.Tiếng.Anh` <- NULL

names(admins.unit) <- c("province_id", "province_lb", "province_lv")

admins.unit <- admins.unit[!is.na(admins.unit$province_id), ]

admins.unit$province_id <- as.character(admins.unit$province_id)
admins.unit$province_id <- vapply(admins.unit$province_id,
                                  function(x) x <- if (nchar(x) == 1L) paste0("0", x) else x,
                                  "1")

admins.unit$province_lb <- gsub("Tỉnh ", "", admins.unit$province_lb)
admins.unit$province_lb <- gsub("Thành phố ", "TP. ", admins.unit$province_lb)

write.csv(admins.unit, "prov-admins-unit.csv", row.names = FALSE)
