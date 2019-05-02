
fill_missval <- function(x) {
    freq <- table(x)
    if (length(freq) == 0) freq <- c("1" = 0)
    max_val <- names(freq)[freq == max(freq)]
    x[is.na(x)] <- as.numeric(max_val[1])
    x
}

to_NA <- function(x) {x[x %in% 0 | is.nan(x)] <- NA; x}

count_by_perct <- function(dta, what, index, col) {
    col_q <- enquo(col)
    what_q <- enquo(what)
    dta %>%
        count(!!what_q, !!col_q, .drop = FALSE) %>%
        mutate(perct = round(n * 100 / nrow(dta), 1)) %>%
        select(-n) %>%
        spread(!!what_q, perct) %>%
        mutate(STT = paste0(index, ".", 1:n())) %>%
        select(STT, everything())
}


count_by_number <- function(dta, what, index, col) {
    col_q <- enquo(col)
    what_q <- enquo(what)
    dta %>%
        count(!!what_q, !!col_q, .drop = FALSE) %>%
        spread(!!what_q, n) %>%
        mutate(STT = paste0(index, ".", 1:n())) %>%
        select(STT, everything())
}

count_by_ <- function(col, lab) {

    col_q <- ensym(col)
    col_c <- as.character(col_q)

    function(dta, what, pct = TRUE, index) {
        if (nrow(dta) == 0) return(NULL)
        what_q <- enquo(what)
        if (pct) {
            out <- count_by_perct(dta = dta, what = !!what_q,
                                  index = index, col = !!col_q)
        } else {
            out <- count_by_number(dta = dta, what = !!what_q,
                                   index = index, col = !!col_q)
        }

        foo <- slice(out, 1) %>% mutate_all(function(x) NA)
        foo[[col_c]] <- lab

        out <- mutate_if(out, is.factor, as.character)
        out <- mutate_if(out, is.numeric, to_NA)
        out <- bind_rows(foo, out)
        names(out)[names(out) == col_c] <- "Phân loại ngừơi trả lời"
        out
    }

}

count_by_gender <- count_by_(A4_LABEL, lab = "Theo giới tính")
count_by_age <- count_by_(AGE_BINS, lab = "Theo độ tuổi")
count_by_region <- count_by_(TTNT_LABEL, lab = "Theo khu vực")
count_by_economic_region <- count_by_(REGION, lab = "Theo vùng kinh tế")
count_by_education <- count_by_(A6_LABEL, lab = "Theo trình độ học vấn")
count_by_ethnicity <- count_by_(A5_LABEL, lab = "Theo dân tộc")
count_by_occupation <- count_by_(A7_LVL1_LABEL, lab = "Theo nghề nghiệp")
count_by_median_income <- count_by_(A8_LABEL, lab = "Theo thu nhập bình quân 1 tháng của đáp viên")
count_by_income_per_capita <- count_by_(A9_LABEL, lab = "Theo thu nhập bình quân 1 người 1 tháng")

count_all <- function(dta, what, index, pct = TRUE) {
    if (nrow(dta) == 0) return(NULL)
    what_q <- enquo(what)

    if (pct) {
        out <- dta %>%
            count(!!what_q, .drop = FALSE) %>%
            mutate(perct = round(n * 100 / nrow(dta), 1)) %>%
            select(-n) %>%
            spread(!!what_q, perct)
    } else {
        out <- dta %>%
            count(!!what_q, .drop = FALSE) %>%
            spread(!!what_q, n)
    }
    out <- out %>%
        mutate(STT = index,
               `Phân loại ngừơi trả lời` = "Tổng") %>%
        select(STT, `Phân loại ngừơi trả lời`, everything()) %>%
        mutate_if(is.factor, as.character) %>%
        mutate_if(is.numeric, to_NA)
    out
}

count_response <- function(dta, what, pct = TRUE, index) {
    what_q <- enquo(what)
    index_letters <- paste0(index, letters[1:9])
    bind_rows(
        count_all(dta = dta, what = !!what_q, index = index),
        count_by_gender(dta = dta, what = !!what_q, pct = pct, index = index_letters[1]),
        count_by_age(dta = dta, what = !!what_q, pct = pct, index = index_letters[2]),
        count_by_region(dta = dta, what = !!what_q, pct = pct, index = index_letters[3]),
        count_by_economic_region(dta = dta, what = !!what_q, pct = pct, index = index_letters[4]),
        count_by_education(dta = dta, what = !!what_q, pct = pct, index = index_letters[5]),
        count_by_ethnicity(dta = dta, what = !!what_q, pct = pct, index = index_letters[6]),
        count_by_occupation(dta = dta, what = !!what_q, pct = pct, index = index_letters[7]),
        count_by_median_income(dta = dta, what = !!what_q, pct = pct, index = index_letters[8]),
        count_by_income_per_capita(dta = dta, what = !!what_q, pct = pct, index = index_letters[9])
    )
}

count_response_stratified_by_region <- function(dta, what, pct = TRUE, index) {
    what_q <- enquo(what)
    index_letters <- paste0(index, letters[1:6])
    bind_rows(
        count_all(dta = dta, what = !!what_q, index = index),
        count_by_gender(dta = dta, what = !!what_q, pct = pct, index = index_letters[1]),
        count_by_age(dta = dta, what = !!what_q, pct = pct, index = index_letters[2]),
        count_by_region(dta = dta, what = !!what_q, pct = pct, index = index_letters[3]),
        count_by_economic_region(dta = dta, what = !!what_q, pct = pct, index = index_letters[4]),
        count_by_education(dta = dta, what = !!what_q, pct = pct, index = index_letters[5]),
        count_by_ethnicity(dta = dta, what = !!what_q, pct = pct, index = index_letters[6]),
        count_by_occupation(dta = dta, what = !!what_q, pct = pct, index = index_letters[7]),
        count_by_median_income(dta = dta, what = !!what_q, pct = pct, index = index_letters[8]),
        count_by_income_per_capita(dta = dta, what = !!what_q, pct = pct, index = index_letters[9])
    )
}

onehot_encoding <- function(dta, val) {
    apply(dta, 1, function(x) sum(x %in% val, na.rm = TRUE)) ->.;
    ifelse(. == 1, "Có", "Không")
}

add_rsp <- function(x, rsp) {
    if (nrow(x) == 0) return(NULL)
    stopifnot(length(rsp) >= 1)
    stopifnot(is.character(rsp))
    for (i in seq_along(rsp)) {
        chk <- rsp[i]
        if (!any(names(x) %in% chk)) x[[chk]] <- NA
    }
    x
}

adjust_colnames <- function(x, prefix) {
    if (is.null(x) || nrow(x) == 0) return(NULL)
    fixed_colnames <- c("STT", "Phân loại ngừơi trả lời")
    names(x)[!names(x) %in% fixed_colnames] <-
        paste(prefix, "-", names(x)[!names(x) %in% fixed_colnames])
    x
}
