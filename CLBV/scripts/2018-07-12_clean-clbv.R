

library(readxl)
library(dplyr)

dta2013 <- read_xls("./data/Du lieu CLBV Data2.xls", sheet = "CL 2013 (B12)", skip = 2)
dta2014 <- read_xls("./data/Du lieu CLBV Data2.xls", sheet = "CL2014(B12)", skip = 2)
dta2015 <- read_xls("./data/Du lieu CLBV Data2.xls", sheet = "CL 2015(B12)", skip = 2)
dta2016 <- read_xls("./data/Du lieu CLBV Data2.xls", sheet = "CL 2016(B12)", skip = 2)


# Helper functions --------------------------------------------------------

rename_columns <- function(x) {

    make_names <- function(prefix, len) {
        c(prefix, paste0(prefix, "_", letters[1:len]))
    }

    a1_cols <- make_names("a1", 6)
    a2_cols <- make_names("a2", 5)
    a3_cols <- make_names("a3", 2)
    a4_cols <- make_names("a4", 6)
    b1_cols <- make_names("b1", 3)
    b2_cols <- make_names("b2", 3)
    b3_cols <- make_names("b3", 4)
    b4_cols <- make_names("b4", 4)
    c1_cols <- make_names("c1", 2)
    c2_cols <- make_names("c2", 2)
    c3_cols <- make_names("c3", 2)
    c4_cols <- make_names("c4", 6)
    c5_cols <- make_names("c5", 6)
    c6_cols <- make_names("c6", 5)
    c7_cols <- make_names("c7", 5)
    c8_cols <- make_names("c8", 2)
    c9_cols <- make_names("c9", 6)
    c10_cols <- make_names("c10", 2)
    d1_cols <- make_names("d1", 3)
    d2_cols <- make_names("d3", 2)
    d3_cols <- make_names("d3", 4)
    e1_cols <- make_names("e1", 4)

    names(x) <- c("stt", "benh_vien", "noi_danh_gia",
                  a1_cols, a2_cols, a3_cols, a4_cols,
                  b1_cols, b2_cols, b3_cols, b4_cols,
                  c1_cols, c2_cols, c3_cols, c4_cols, c5_cols,
                  c6_cols, c7_cols, c8_cols, c9_cols, c10_cols,
                  d1_cols, d2_cols, d3_cols, e1_cols)
    x
}


# 2013 --------------------------------------------------------------------

dta2013 <- filter(dta2013, STT != "A" & !is.na(`Bệnh viện`))

dta2013 <- select(dta2013, -`Tỉnh/TP`, -`Tuyến`, -`Hạng`, -`Loại`,
                  -`X__1`, -matches("^PHẦN.[A-E]"))

dta2013 <- rename_columns(dta2013)

# 2014 --------------------------------------------------------------------

dta2014 <- filter(dta2014, STT != "A" & !is.na(`Bệnh viện`))

dta2014 <- select(dta2014, -`Tỉnh/TP`, -`Tuyến`, -`Hạng`,
                  -matches("^PHẦN.[A-E]"), -matches("^C10[a-d]"))

dta2014 <- rename_columns(dta2014)

# 2015 --------------------------------------------------------------------

dta2015 <- filter(dta2015, !is.na(STT) & !is.na(`Bệnh viện`))

dta2015 <- select(dta2015, -`Tỉnh/TP`, -`Tuyến`, -`Hạng`, -`X__1`,
                  -matches("^PHẦN.[A-E]"), -matches("^C10[a-d]"))

dta2015 <- rename_columns(dta2015)


# 2016 --------------------------------------------------------------------

dta2016 <- filter(dta2016, !`Bệnh viện` %in% c("TB", "MP"))

dta2016 <- select(dta2016, -`Tỉnh/TP`, -`Tuyến`, -`Hạng`,
                  -`Thực hiện danh mục kỹ thuật theo phân tuyến kỹ thuật`,
                  -matches("^PHẦN.[A-E]"), -matches("^C10[a-d]"))

dta2016 <- rename_columns(dta2016)

