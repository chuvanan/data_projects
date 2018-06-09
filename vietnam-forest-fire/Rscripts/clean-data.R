

library(reshape2)

setwd("~/Documents/vietnam-forest-fire/data/")

forest.fire <- read.csv2("V06.52.csv", skip = 2, stringsAsFactors = FALSE)

names(forest.fire) <- c("province", paste0("y", 1995:2015))


## re-decode missing values

forest.fire[] <- lapply(forest.fire, function(x) replace(x, x == "..", NA))


## remove empyt records

forest.fire <- forest.fire[-c(67:70), ]

## keep individual provinces only

forest.fire <- forest.fire[!forest.fire$province %in% c("Trung du và miền núi phía Bắc",
                                                        "Đông Nam Bộ",
                                                        "Đồng bằng sông Hồng",
                                                        "Đồng bằng sông Cửu Long",
                                                        "CẢ NƯỚC",
                                                        "Bắc Trung Bộ và Duyên hải miền Trung",
                                                        "Tây Nguyên"), ]

forest.fire[, -1] <- lapply(forest.fire[, -1], as.double)

## tidying data

forest.fire <- melt(forest.fire,
                    id.vars = "province",
                    variable.name = "year",
                    value.name = "fire_damage")

forest.fire$year <- gsub("y", "", forest.fire$year)
forest.fire$year <- as.integer(forest.fire$year)

## fix the zero bug
## http://www.win-vector.com/blog/2017/02/the-zero-bug/

forest.fire$fire_damage[is.na(forest.fire$fire_damage)] <- 0

## export data

write.csv(forest.fire, "vietnam-forest-fire.csv", row.names = FALSE)
