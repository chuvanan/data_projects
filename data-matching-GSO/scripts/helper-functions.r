
fill_missval <- function(x) {
    freq <- table(x)
    max_val <- names(freq)[freq == max(freq)]
    x[is.na(x)] <- as.numeric(max_val[1])
    x
}

count_by_gender <- function(dta, what, pct = TRUE, index) {
    what_q <- enquo(what)

    if (pct) {
        out <- dta %>%
            count(!!what_q, A4_LABEL, .drop = FALSE) %>%
            mutate(perct = round(n * 100 / nrow(dta), 1)) %>%
            select(-n) %>%
            spread(!!what_q, perct) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    } else {
        out <- dta %>%
            count(!!what_q, A4_LABEL, .drop = FALSE) %>%
            spread(!!what_q, n) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    }

    foo <- slice(out, 1) %>%
        mutate_all(function(x) NA) %>%
        mutate(A4_LABEL = "Theo giới tính")

    out <- mutate_if(out, is.factor, as.character)
    out <- bind_rows(foo, out)
    names(out)[names(out) == "A4_LABEL"] <- "Phân loại ngừơi trả lời"
    out
}

count_by_age <- function(dta, what, pct = TRUE, index) {
    what_q <- enquo(what)

    if (pct) {
        out <- dta %>%
            count(!!what_q, AGE_BINS, .drop = FALSE) %>%
            mutate(perct = round(n * 100 / nrow(dta), 1)) %>%
            select(-n) %>%
            spread(!!what_q, perct) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    } else {
        out <- dta %>%
            count(!!what_q, AGE_BINS, .drop = FALSE) %>%
            spread(!!what_q, n) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    }

    foo <- slice(out, 1) %>%
        mutate_all(function(x) NA) %>%
        mutate(AGE_BINS = "Theo độ tuổi")

    out <- mutate_if(out, is.factor, as.character)
    out <- bind_rows(foo, out)
    names(out)[names(out) == "AGE_BINS"] <- "Phân loại ngừơi trả lời"
    out
}

count_by_region <- function(dta, what, pct = TRUE, index) {
    what_q <- enquo(what)

    if (pct) {
        out <- dta %>%
            count(!!what_q, TTNT_LABEL, .drop = FALSE) %>%
            mutate(perct = round(n * 100 / nrow(dta), 1)) %>%
            select(-n) %>%
            spread(!!what_q, perct) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    } else {
        out <- dta %>%
            count(!!what_q, TTNT_LABEL, .drop = FALSE) %>%
            spread(!!what_q, n) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    }

    foo <- slice(out, 1) %>%
        mutate_all(function(x) NA) %>%
        mutate(TTNT_LABEL = "Theo khu vực")

    out <- mutate_if(out, is.factor, as.character)
    out <- bind_rows(foo, out)
    names(out)[names(out) == "TTNT_LABEL"] <- "Phân loại ngừơi trả lời"
    out
}


count_by_education <- function(dta, what, pct = TRUE, index) {
    what_q <- enquo(what)

    if (pct) {
        out <- dta %>%
            count(!!what_q, A6_LABEL, .drop = FALSE) %>%
            mutate(perct = round(n * 100 / nrow(dta), 1)) %>%
            select(-n) %>%
            spread(!!what_q, perct) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    } else {
        out <- dta %>%
            count(!!what_q, A6_LABEL, .drop = FALSE) %>%
            spread(!!what_q, n) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    }

    foo <- slice(out, 1) %>%
        mutate_all(function(x) NA) %>%
        mutate(A6_LABEL = "Theo trình độ học vấn")

    out <- mutate_if(out, is.factor, as.character)
    out <- bind_rows(foo, out)
    names(out)[names(out) == "A6_LABEL"] <- "Phân loại ngừơi trả lời"
    out
}

count_by_ethnicity <- function(dta, what, pct = TRUE, index) {
    what_q <- enquo(what)
    if (pct) {
        out <- dta %>%
            count(!!what_q, A5_LABEL, .drop = FALSE) %>%
            mutate(perct = round(n * 100 / nrow(dta), 1)) %>%
            select(-n) %>%
            spread(!!what_q, perct) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    } else {
        out <- dta %>%
            count(!!what_q, A5_LABEL, .drop = FALSE) %>%
            spread(!!what_q, n) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    }

    foo <- slice(out, 1) %>%
        mutate_all(function(x) NA) %>%
        mutate(A5_LABEL = "Theo dân tộc")

    out <- mutate_if(out, is.factor, as.character)
    out <- bind_rows(foo, out)
    names(out)[names(out) == "A5_LABEL"] <- "Phân loại ngừơi trả lời"
    out
}

count_by_occupation <- function(dta, what, pct = TRUE, index) {
    what_q <- enquo(what)

    if (pct) {
        out <- dta %>%
            count(!!what_q, A7_LVL1_LABEL, .drop = FALSE) %>%
            mutate(perct = round(n * 100 / nrow(dta), 1)) %>%
            select(-n) %>%
            spread(!!what_q, perct) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    } else {
        out <- dta %>%
            count(!!what_q, A7_LVL1_LABEL, .drop = FALSE) %>%
            spread(!!what_q, n) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    }

    foo <- slice(out, 1) %>%
        mutate_all(function(x) NA) %>%
        mutate(A7_LVL1_LABEL = "Theo nghề nghiệp")

    out <- mutate_if(out, is.factor, as.character)
    out <- bind_rows(foo, out)
    names(out)[names(out) == "A7_LVL1_LABEL"] <- "Phân loại ngừơi trả lời"
    out
}

count_by_median_income <- function(dta, what, pct = TRUE, index) {
    what_q <- enquo(what)
    if (pct) {
        out <- dta %>%
            count(!!what_q, A8_LABEL, .drop = FALSE) %>%
            mutate(perct = round(n * 100 / nrow(dta), 1)) %>%
            select(-n) %>%
            spread(!!what_q, perct) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    } else {
        out <- dta %>%
            count(!!what_q, A8_LABEL, .drop = FALSE) %>%
            spread(!!what_q, n) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    }

    foo <- slice(out, 1) %>%
        mutate_all(function(x) NA) %>%
        mutate(A8_LABEL = "Theo thu nhập bình quân 1 tháng của đáp viên")

    out <- mutate_if(out, is.factor, as.character)
    out <- bind_rows(foo, out)
    names(out)[names(out) == "A8_LABEL"] <- "Phân loại ngừơi trả lời"
    out
}

count_by_income_per_capita <- function(dta, what, pct = TRUE, index) {
    what_q <- enquo(what)

    if (pct) {
        out <- dta %>%
            count(!!what_q, A9_LABEL, .drop = FALSE) %>%
            mutate(perct = round(n * 100 / nrow(dta), 1)) %>%
            select(-n) %>%
            spread(!!what_q, perct) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    } else {
        out <- dta %>%
            count(!!what_q, A9_LABEL, .drop = FALSE) %>%
            spread(!!what_q, n) %>%
            mutate(STT = paste0(index, ".", 1:n())) %>%
            select(STT, everything())
    }

    foo <- slice(out, 1) %>%
        mutate_all(function(x) NA) %>%
        mutate(A9_LABEL = "Theo thu nhập bình quân 1 người 1 tháng")

    out <- mutate_if(out, is.factor, as.character)
    out <- bind_rows(foo, out)
    names(out)[names(out) == "A9_LABEL"] <- "Phân loại ngừơi trả lời"
    out
}

count_all <- function(dta, what, index) {
    what_q <- enquo(what)
    out <- dta %>%
        count(!!what_q, .drop = FALSE) %>%
        spread(!!what_q, n) %>%
        mutate(STT = index,
               `Phân loại ngừơi trả lời` = "Tổng") %>%
        select(STT, `Phân loại ngừơi trả lời`, everything())
    out <- mutate_if(out, is.factor, as.character)
    out
}

count_response <- function(dta, what, pct = TRUE, index) {
    what_q <- enquo(what)
    index_letters <- paste0(index, letters[1:7])
    bind_rows(
        count_all(dta = dta, what = !!what_q, index = index),
        count_by_gender(dta = dta, what = !!what_q, pct = pct, index = index_letters[1]),
        count_by_age(dta = dta, what = !!what_q, pct = pct, index = index_letters[2]),
        count_by_region(dta = dta, what = !!what_q, pct = pct, index = index_letters[3]),
        count_by_education(dta = dta, what = !!what_q, pct = pct, index = index_letters[4]),
        count_by_ethnicity(dta = dta, what = !!what_q, pct = pct, index = index_letters[5]),
        count_by_occupation(dta = dta, what = !!what_q, pct = pct, index = index_letters[6]),
        count_by_median_income(dta = dta, what = !!what_q, pct = pct, index = index_letters[7])
    )
}

onehot_encoding <- function(dta, val) {
    apply(dta, 1, function(x) sum(x %in% val, na.rm = TRUE)) ->.;
    ifelse(. == 1, "Có", "Không")
}

add_rsp <- function(x, rsp) {
    stopifnot(length(rsp) >= 1)
    stopifnot(is.character(rsp))
    for (i in seq_along(rsp)) {
        chk <- rsp[i]
        if (!any(names(x) %in% chk)) x[[chk]] <- NA
    }
    x
}

adjust_colnames <- function(x, prefix) {
    fixed_colnames <- c("STT", "Phân loại ngừơi trả lời")
    names(x)[!names(x) %in% fixed_colnames] <-
        paste(prefix, "-", names(x)[!names(x) %in% fixed_colnames])
    x
}
