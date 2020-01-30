
options(digits = 4)
## fisf <- read.csv("../data/Solieu_FISF.csv",
##                  stringsAsFactors = FALSE)

fisf <- readxl::read_xlsx("../data/SoLieu_Gui An.xlsx")

table(fisf$TTNT, useNA = "ifany")
table(fisf$TINH, useNA = "ifany")
table(fisf$HUYEN, useNA = "ifany")
table(fisf$XA, useNA = "ifany")
table(fisf$DIABAN, useNA = "ifany")


names(fisf)[1:30]

dmnn <- readxl::read_xls("../data/Danh muc nghe-final.xls",
                         sheet = "C. 1-2-3-4", skip = 3)

dim(fisf)                               # 188 obs, 412 vars
names(fisf)
sapply(fisf, class)

names(dmnn)
names(dmnn) <- c("CAP1", "CAP2", "CAP3", "CAP4", "FULL_TITLE")
head(dmnn)
sapply(dmnn, class)


count_na <- function(x) sum(is.na(x)) * 100 / length(x)
count_empty_char <- function(x) sum(x %in% "") * 100 / length(x)

anyDuplicated(fisf)
table(duplicated(fisf))
fisf <- fisf[!duplicated(fisf), ]

vars_has_NA <- Filter(function(x) x > 0, sort(sapply(fisf, count_na)))
length(vars_has_NA) / ncol(fisf)

Filter(function(x) x > 0, sort(sapply(fisf, count_empty_char)))
## should we convert "" to NA?

## DAN TOC ------------------------------

table(fisf$A5, useNA = "ifany")

table(fisf$A5MA, useNA = "ifany")

table(fisf$A5MOTA, useNA = "ifany")


## NGHE NGHIEP ------------------------------

table(fisf$A7)
table(fisf$A7MOTA, useNA = "ifany")
as.data.frame(table(fisf$A7MANGHE, useNA = "ifany"))

dmnn$FULL_TITLE <- tolower(dmnn$FULL_TITLE)

dmnn[stringr::str_detect(dmnn$FULL_TITLE, "y tá"), ]
dmnn$FULL_TITLE[stringr::str_detect(dmnn$FULL_TITLE, "xây dựng")]
dmnn$FULL_TITLE[stringr::str_detect(dmnn$FULL_TITLE, "tự do")]
dmnn$FULL_TITLE[stringr::str_detect(dmnn$FULL_TITLE, "trồng trọt")]
dmnn$FULL_TITLE[stringr::str_detect(dmnn$FULL_TITLE, "thợ cơ khí")]
dmnn$FULL_TITLE[stringr::str_detect(dmnn$FULL_TITLE, "phụ hồ")]
dmnn$FULL_TITLE[stringr::str_detect(dmnn$FULL_TITLE, "nhân viên văn phòng")]
