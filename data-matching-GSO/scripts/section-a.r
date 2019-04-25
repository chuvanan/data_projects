## @ancv

## -----------------------------------------------------------------------------
## A - Nhan Khau Hoc va Cau Hoi Chung

source("02-process.R")

## A1 ------------------------------

A1_0 <- local({
    cnt_education <- count(fisf, A6_LABEL, .drop = FALSE) %>%
        mutate(STT = paste0("A1.", 1:nrow(.))) %>%
        rename(`Phân loại ngừơi trả lời` = A6_LABEL) %>%
        mutate_if(is.factor, as.character)

    cnt_occupation <- count(fisf, A7_LVL1_LABEL, .drop = FALSE) %>%
        mutate(STT = paste0("A2.", 1:nrow(.))) %>%
        rename(`Phân loại ngừơi trả lời` = A7_LVL1_LABEL) %>%
        mutate_if(is.factor, as.character)

    cnt_median_income <- count(fisf, A8_LABEL, .drop = FALSE) %>%
        mutate(STT = paste0("A3.", 1:nrow(.))) %>%
        rename(`Phân loại ngừơi trả lời` = A8_LABEL) %>%
        mutate_if(is.factor, as.character)

    cnt_income_per_captia <- count(fisf, A9_LABEL, .drop = FALSE) %>%
        mutate(STT = paste0("A4.", 1:nrow(.))) %>%
        rename(`Phân loại ngừơi trả lời` = A9_LABEL) %>%
        mutate_if(is.factor, as.character)

    cnt_all <- tibble(STT = "A", n = nrow(fisf),
                      `Phân loại ngừơi trả lời` = "Tổng")

    bind_rows(cnt_all, cnt_education, cnt_occupation,
              cnt_median_income, cnt_income_per_captia) %>%
        select(STT, `Phân loại ngừơi trả lời`, `Tổng` = n)
})

A1_1 <- bind_rows(
    count_all(fisf, AGE_BINS, index = "A", pct = FALSE),
    count_by_education(fisf, what = AGE_BINS, pct = FALSE, index = "A1"),
    count_by_occupation(fisf, what = AGE_BINS, pct = FALSE, index = "A2"),
    count_by_median_income(fisf, what = AGE_BINS, pct = FALSE, index = "A3"),
    count_by_income_per_capita(fisf, what = AGE_BINS, pct = FALSE, index = "A4")
)
A1_1 <- adjust_colnames(A1_1, "Theo độ tuổi")

A1_2 <- bind_rows(
    count_all(fisf, A4_LABEL, index = "A", pct = FALSE),
    count_by_education(fisf, what = A4_LABEL, pct = FALSE, index = "A1"),
    count_by_occupation(fisf, what = A4_LABEL, pct = FALSE, index = "A2"),
    count_by_median_income(fisf, what = A4_LABEL, pct = FALSE, index = "A3"),
    count_by_income_per_capita(fisf, what = A4_LABEL, pct = FALSE, index = "A4")
)
A1_2 <- adjust_colnames(A1_2, "Theo giới tính")

A1_3 <- bind_rows(
    count_all(fisf, A5_LABEL, index = "A", pct = FALSE),
    count_by_education(fisf, what = A5_LABEL, pct = FALSE, index = "A1"),
    count_by_occupation(fisf, what = A5_LABEL, pct = FALSE, index = "A2"),
    count_by_median_income(fisf, what = A5_LABEL, pct = FALSE, index = "A3"),
    count_by_income_per_capita(fisf, what = A5_LABEL, pct = FALSE, index = "A4")
)
A1_3 <- adjust_colnames(A1_3, "Theo dân tộc")

A1_4 <- bind_rows(
    count_all(fisf, TTNT_LABEL, index = "A", pct = FALSE),
    count_by_education(fisf, what = TTNT_LABEL, pct = FALSE, index = "A1"),
    count_by_occupation(fisf, what = TTNT_LABEL, pct = FALSE, index = "A2"),
    count_by_median_income(fisf, what = TTNT_LABEL, pct = FALSE, index = "A3"),
    count_by_income_per_capita(fisf, what = TTNT_LABEL, pct = FALSE, index = "A4")
)
A1_4 <- adjust_colnames(A1_2, "Theo khu vực")

A <- bind_cols(
    A1_1,
    select(A1_2, -STT, -`Phân loại ngừơi trả lời`),
    select(A1_3, -STT, -`Phân loại ngừơi trả lời`),
    select(A1_4, -STT, -`Phân loại ngừơi trả lời`)
)
A <- left_join(A, A1_0)
A <- select(A, STT, `Phân loại ngừơi trả lời`, `Tổng`, everything())

## Export ------------------------------

openxlsx::write.xlsx(list(A = A), file = "../outputs/SECTION-A TOANQUOC.xlsx")
