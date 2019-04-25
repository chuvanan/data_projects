## @ancv

## -----------------------------------------------------------------------------
## B - Co So Ha Tang

source("02-process.R")

## B1 ------------------------------

fisf <- fisf %>%
    mutate(B1A = fill_missval(B1A),
           B1B = fill_missval(B1B),
           B1C = fill_missval(B1C))

fisf <- fisf %>%
    mutate(B1A_LABEL = B1A,
           B1B_LABEL = B1B,
           B1C_LABEL = B1C)

fisf <- fisf %>%
    mutate_at(c("B1A_LABEL", "B1B_LABEL", "B1C_LABEL"),
              function(x) case_when(
                              x == 1 ~ "Có",
                              x == 2 ~ "Không",
                              x == 7 ~ "Không có điện thoại di động, cố định và thiết bị để kết nối internet"
                          )) %>%
    mutate_at(c("B1A_LABEL", "B1B_LABEL", "B1C_LABEL"),
              function(x) factor(x, levels = c("Có", "Không", "Không có điện thoại di động, cố định và thiết bị để kết nối internet"),
                                 ordered = TRUE))

fisf <- fisf %>%
    mutate(B1D_LABEL = {apply(select(fisf, matches("B1[A-C]$")),
                              MARGIN = 1,
                              function(x) sum(any(x == 7, na.rm = TRUE))) ->.;
                              ifelse(. == 1, "Có", "Không")})

B1A <- count_response(fisf, B1A_LABEL, index = "B1")
B1A <- add_rsp(B1A, c("Có", "Không"))
B1A <- adjust_colnames(B1A, "Điện thoại cố định để gọi và nhận cuộc gọi cá nhân")

B1B <- count_response(fisf, B1B_LABEL, index = "B1")
B1B <- add_rsp(B1B, c("Có", "Không"))
B1B <- adjust_colnames(B1B, "Điện thoại di động để gọi và nhận cuộc gọi cá nhân")

B1C <- count_response(fisf, B1C_LABEL, index = "B1")
B1C <- add_rsp(B1C, c("Có", "Không"))
B1C <- adjust_colnames(B1C, "Điện thoại di động, máy tính hoặc các thiết bị khác để kết nối Internet")

B1D <- count_response(fisf, B1D_LABEL, index = "B1")
B1D <- add_rsp(B1D, c("Có", "Không"))
B1D <- B1D %>%
    rename(`Không có điện thoại di động, cố định và thiết bị để kết nối internet` = `Có`)

B1 <- bind_cols(
    select(B1A, -starts_with("Không")),
    select(B1B, -STT, -`Phân loại ngừơi trả lời`, -starts_with("Không")),
    select(B1C, -STT, -`Phân loại ngừơi trả lời`, -starts_with("Không")),
    select(B1D, -STT, -`Phân loại ngừơi trả lời`, -`Không`)
)

## B2 ------------------------------

fisf <- fisf %>%
    mutate(B2A = fill_missval(B2A),
           B2B = fill_missval(B2B),
           B2C = fill_missval(B2C),
           B2D = fill_missval(B2D))

fisf <- fisf %>%
    mutate(B2A_LABEL = B2A,
           B2B_LABEL = B2B,
           B2C_LABEL = B2C,
           B2D_LABEL = B2D)

fisf <- fisf %>%
    mutate_at(c("B2A_LABEL", "B2B_LABEL", "B2C_LABEL", "B2D_LABEL"),
              function(x) case_when(
                              x == 1 ~ "Có",
                              x == 2 ~ "Không",
                              x == 8 ~ "Không biết"
                          )) %>%
    mutate_at(c("B2A_LABEL", "B2B_LABEL", "B2C_LABEL", "B2D_LABEL"),
              function(x) factor(x, levels = c("Có", "Không", "Không biết"),
                                 ordered = TRUE))


B2A <- count_response(fisf, B2A_LABEL, index = "B2")
B2A <- add_rsp(B2A, c("Có", "Không", "Không biết"))
B2A <- adjust_colnames(B2A, "Nhà của ông/bà đang ở là của gia đình?")


B2B <- count_response(fisf, B2B_LABEL, index = "B2")
B2B <- add_rsp(B2B, c("Có", "Không", "Không biết"))
B2B <- adjust_colnames(B2B, "Ông/bà sẽ bán nhà nếu như cần một khoản tiền lớn?")

B2C <- count_response(fisf, B2C_LABEL, index = "B2")
B2C <- add_rsp(B2C, c("Có", "Không", "Không biết"))
B2C <- adjust_colnames(B2C, "Ông/bà sẽ dùng nhà để thế chấp khi vay tiền")


B2D <- count_response(fisf, B2D_LABEL, index = "B2")
B2D <- add_rsp(B2D, c("Có", "Không", "Không biết"))
B2D <- adjust_colnames(B2D, "Ông/bà có nhà cho thuê hay đầu tư sinh lời?")

B2 <- bind_cols(
    B2A,
    select(B2B, -STT, -`Phân loại ngừơi trả lời`),
    select(B2C, -STT, -`Phân loại ngừơi trả lời`),
    select(B2D, -STT, -`Phân loại ngừơi trả lời`)
)

## B3 ------------------------------

fisf <- fisf %>%
    mutate(B3A = fill_missval(B3A),
           B3B = fill_missval(B3B),
           B3C = fill_missval(B3C),
           B3D = fill_missval(B3D),
           B3E = fill_missval(B3E),
           B3F = fill_missval(B3F),
           B3G = fill_missval(B3G),
           B3H = fill_missval(B3H))

fisf <- fisf %>%
    mutate(B3A_LABEL = B3A,
           B3B_LABEL = B3B,
           B3C_LABEL = B3C,
           B3D_LABEL = B3D,
           B3E_LABEL = B3E,
           B3F_LABEL = B3F,
           B3G_LABEL = B3G,
           B3H_LABEL = B3H)

fisf <- fisf %>%
    mutate_at(c("B3A_LABEL", "B3B_LABEL", "B3C_LABEL", "B3D_LABEL",
                "B3E_LABEL", "B3F_LABEL", "B3G_LABEL", "B3H_LABEL"),
              function(x) case_when(
                              x == 1 ~ "Dưới 15 phút",
                              x == 2 ~ "Từ 15 phút đến dưới 30 phút",
                              x == 3 ~ "Từ 30 phút đến dưới 1 tiếng",
                              x == 4 ~ "Từ 1 tiếng đến dưới 3 tiếng",
                              x == 5 ~ "Từ 3 tiếng trở lên",
                              x == 8 ~ "Không biết"
                          )) %>%
    mutate_at(c("B3A_LABEL", "B3B_LABEL", "B3C_LABEL", "B3D_LABEL",
                "B3E_LABEL", "B3F_LABEL", "B3G_LABEL", "B3H_LABEL"),
              function(x) factor(x, levels = c("Dưới 15 phút",
                                               "Từ 15 phút đến dưới 30 phút",
                                               "Từ 30 phút đến dưới 1 tiếng",
                                               "Từ 1 tiếng đến dưới 3 tiếng",
                                               "Từ 3 tiếng trở lên",
                                               "Không biết"),
                                 ordered = TRUE))

B3_0_TT <- bind_rows(
    count_all(filter(fisf, TTNT == 1), B3A_LABEL, index = "B3a.1"),
    count_all(filter(fisf, TTNT == 1), B3B_LABEL, index = "B3a.2"),
    count_all(filter(fisf, TTNT == 1), B3C_LABEL, index = "B3a.3"),
    count_all(filter(fisf, TTNT == 1), B3D_LABEL, index = "B3a.4"),
    count_all(filter(fisf, TTNT == 1), B3E_LABEL, index = "B3a.5"),
    count_all(filter(fisf, TTNT == 1), B3F_LABEL, index = "B3a.6"),
    count_all(filter(fisf, TTNT == 1), B3G_LABEL, index = "B3a.7"),
    count_all(filter(fisf, TTNT == 1), B3H_LABEL, index = "B3a.8")
)

B3_0_TT <- tibble(STT = NA, `Phân loại ngừơi trả lời` = "Khu vực thành thị") %>%
    bind_rows(B3_0_TT) %>%
    mutate(`Phân loại ngừơi trả lời` = case_when(
               STT == "B3a.1" ~ "Chi nhánh, phòng giao dịch ngân hàng",
               STT == "B3a.2" ~ "Máy ATM",
               STT == "B3a.3" ~ "Bưu điện",
               STT == "B3a.4" ~ "Điểm giao dịch/trụ sở của công ty bảo hiểm/môi giới/đại lý",
               STT == "B3a.5" ~ "Các tổ chức tài chính khác",
               STT == "B3a.6" ~ "Trạm xăng",
               STT == "B3a.7" ~ "Trung tâm thương mại/Siêu thị/cửa hàng bán lẻ",
               STT == "B3a.8" ~ "Chợ",
               TRUE ~ `Phân loại ngừơi trả lời`
           )) %>%
    rename(`Các loại tiện ích tại khu vực thành thị/nông thôn` = `Phân loại ngừơi trả lời`)

B3_0_NT <- bind_rows(
    count_all(filter(fisf, TTNT == 2), B3A_LABEL, index = "B3b.1"),
    count_all(filter(fisf, TTNT == 2), B3B_LABEL, index = "B3b.2"),
    count_all(filter(fisf, TTNT == 2), B3C_LABEL, index = "B3b.3"),
    count_all(filter(fisf, TTNT == 2), B3D_LABEL, index = "B3b.4"),
    count_all(filter(fisf, TTNT == 2), B3E_LABEL, index = "B3b.5"),
    count_all(filter(fisf, TTNT == 2), B3F_LABEL, index = "B3b.6"),
    count_all(filter(fisf, TTNT == 2), B3G_LABEL, index = "B3b.7"),
    count_all(filter(fisf, TTNT == 2), B3H_LABEL, index = "B3b.8")
)

B3_0_NT <- tibble(STT = NA, `Phân loại ngừơi trả lời` = "Khu vực nông thôn") %>%
    bind_rows(B3_0_NT) %>%
    mutate(`Phân loại ngừơi trả lời` = case_when(
               STT == "B3b.1" ~ "Chi nhánh, phòng giao dịch ngân hàng",
               STT == "B3b.2" ~ "Máy ATM",
               STT == "B3b.3" ~ "Bưu điện",
               STT == "B3b.4" ~ "Điểm giao dịch/trụ sở của công ty bảo hiểm/môi giới/đại lý",
               STT == "B3b.5" ~ "Các tổ chức tài chính khác",
               STT == "B3b.6" ~ "Trạm xăng",
               STT == "B3b.7" ~ "Trung tâm thương mại/Siêu thị/cửa hàng bán lẻ",
               STT == "B3b.8" ~ "Chợ",
               TRUE ~ `Phân loại ngừơi trả lời`
           )) %>%
    rename(`Các loại tiện ích tại khu vực thành thị/nông thôn` = `Phân loại ngừơi trả lời`)

B3_1_TT <- bind_rows(
    count_all(filter(fisf, TTNT == 1), B3A_LABEL, index = "B3a.1", pct = FALSE),
    count_all(filter(fisf, TTNT == 1), B3B_LABEL, index = "B3a.2", pct = FALSE),
    count_all(filter(fisf, TTNT == 1), B3C_LABEL, index = "B3a.3", pct = FALSE),
    count_all(filter(fisf, TTNT == 1), B3D_LABEL, index = "B3a.4", pct = FALSE),
    count_all(filter(fisf, TTNT == 1), B3E_LABEL, index = "B3a.5", pct = FALSE),
    count_all(filter(fisf, TTNT == 1), B3F_LABEL, index = "B3a.6", pct = FALSE),
    count_all(filter(fisf, TTNT == 1), B3G_LABEL, index = "B3a.7", pct = FALSE),
    count_all(filter(fisf, TTNT == 1), B3H_LABEL, index = "B3a.8", pct = FALSE)
)

B3_1_TT <- tibble(STT = NA, `Phân loại ngừơi trả lời` = "Khu vực thành thị") %>%
    bind_rows(B3_1_TT) %>%
    mutate(`Phân loại ngừơi trả lời` = case_when(
               STT == "B3a.1" ~ "Chi nhánh, phòng giao dịch ngân hàng",
               STT == "B3a.2" ~ "Máy ATM",
               STT == "B3a.3" ~ "Bưu điện",
               STT == "B3a.4" ~ "Điểm giao dịch/trụ sở của công ty bảo hiểm/môi giới/đại lý",
               STT == "B3a.5" ~ "Các tổ chức tài chính khác",
               STT == "B3a.6" ~ "Trạm xăng",
               STT == "B3a.7" ~ "Trung tâm thương mại/Siêu thị/cửa hàng bán lẻ",
               STT == "B3a.8" ~ "Chợ",
               TRUE ~ `Phân loại ngừơi trả lời`
           )) %>%
    rename(`Các loại tiện ích tại khu vực thành thị/nông thôn` = `Phân loại ngừơi trả lời`)

B3_1_NT <- bind_rows(
    count_all(filter(fisf, TTNT == 2), B3A_LABEL, index = "B3b.1", pct = FALSE),
    count_all(filter(fisf, TTNT == 2), B3B_LABEL, index = "B3b.2", pct = FALSE),
    count_all(filter(fisf, TTNT == 2), B3C_LABEL, index = "B3b.3", pct = FALSE),
    count_all(filter(fisf, TTNT == 2), B3D_LABEL, index = "B3b.4", pct = FALSE),
    count_all(filter(fisf, TTNT == 2), B3E_LABEL, index = "B3b.5", pct = FALSE),
    count_all(filter(fisf, TTNT == 2), B3F_LABEL, index = "B3b.6", pct = FALSE),
    count_all(filter(fisf, TTNT == 2), B3G_LABEL, index = "B3b.7", pct = FALSE),
    count_all(filter(fisf, TTNT == 2), B3H_LABEL, index = "B3b.8", pct = FALSE)
)

B3_1_NT <- tibble(STT = NA, `Phân loại ngừơi trả lời` = "Khu vực nông thôn") %>%
    bind_rows(B3_1_NT) %>%
    mutate(`Phân loại ngừơi trả lời` = case_when(
               STT == "B3b.1" ~ "Chi nhánh, phòng giao dịch ngân hàng",
               STT == "B3b.2" ~ "Máy ATM",
               STT == "B3b.3" ~ "Bưu điện",
               STT == "B3b.4" ~ "Điểm giao dịch/trụ sở của công ty bảo hiểm/môi giới/đại lý",
               STT == "B3b.5" ~ "Các tổ chức tài chính khác",
               STT == "B3b.6" ~ "Trạm xăng",
               STT == "B3b.7" ~ "Trung tâm thương mại/Siêu thị/cửa hàng bán lẻ",
               STT == "B3b.8" ~ "Chợ",
               TRUE ~ `Phân loại ngừơi trả lời`
           )) %>%
    rename(`Các loại tiện ích tại khu vực thành thị/nông thôn` = `Phân loại ngừơi trả lời`)

B3_0 <- bind_rows(B3_0_TT, B3_0_NT)
B3_1 <- bind_rows(B3_1_TT, B3_1_NT)

B3_2 <- B3_1 %>%
    mutate_at(c("Dưới 15 phút", "Từ 15 phút đến dưới 30 phút",
                "Từ 30 phút đến dưới 1 tiếng", "Từ 1 tiếng đến dưới 3 tiếng",
                "Từ 3 tiếng trở lên"),
              function(x) {x[is.na(x)] <- 0; x}) %>%
    mutate(`Khoảng thời gian` = ((`Dưới 15 phút` * 15) + (`Từ 15 phút đến dưới 30 phút` * 22.5) +
                                 (`Từ 30 phút đến dưới 1 tiếng` * 45) + (`Từ 1 tiếng đến dưới 3 tiếng` * 120) +
                                 (`Từ 3 tiếng trở lên` * 180)) / nrow(fisf)) %>%
    mutate(`Khoảng thời gian` = round(`Khoảng thời gian`, 1)) %>%
    select(STT, `Các loại tiện ích tại khu vực thành thị/nông thôn`,
           `Khoảng thời gian`) %>%
    mutate(`Khoảng thời gian` = if_else(is.na(STT), NA_real_, `Khoảng thời gian`))

## B4 ------------------------------

fisf <- fisf %>%
    mutate(B4 = fill_missval(B4))

fisf <- fisf %>%
    mutate(B4_LABEL = case_when(
               B4 == 1 ~ "Có",
               B4 == 2 ~ "Không",
               B4 == 8 ~ "Không biết"
           ))

B4A <- bind_rows(
    count_all(fisf, B4_LABEL, index = "B4"),
    count_by_gender(fisf, B4_LABEL, index = "B4a"),
    count_by_age(fisf, B4_LABEL, index = "B4b"),
    count_by_education(fisf, B4_LABEL, index = "B4c"),
    count_by_ethnicity(fisf, B4_LABEL, index = "B4d"),
    count_by_occupation(fisf, B4_LABEL, index = "B4e"),
    count_by_median_income(fisf, B4_LABEL, index = "B4f")
)

B4A <- add_rsp(B4A, c("Có", "Không", "Không biết"))
B4A <- adjust_colnames(B4A, "Tổng mẫu/cả nước")

B4B <- bind_rows(
    count_all(filter(fisf, TTNT == 1), B4_LABEL, index = "B4"),
    count_by_gender(filter(fisf, TTNT == 1), B4_LABEL, index = "B4a"),
    count_by_age(filter(fisf, TTNT == 1), B4_LABEL, index = "B4b"),
    count_by_education(filter(fisf, TTNT == 1), B4_LABEL, index = "B4c"),
    count_by_ethnicity(filter(fisf, TTNT == 1), B4_LABEL, index = "B4d"),
    count_by_occupation(filter(fisf, TTNT == 1), B4_LABEL, index = "B4e"),
    count_by_median_income(filter(fisf, TTNT == 1), B4_LABEL, index = "B4f")
)

B4B <- add_rsp(B4B, c("Có", "Không", "Không biết"))
B4B <- adjust_colnames(B4B, "Tại khu vực thành thị")

B4C <- bind_rows(
    count_all(filter(fisf, TTNT == 2), B4_LABEL, index = "B4"),
    count_by_gender(filter(fisf, TTNT == 2), B4_LABEL, index = "B4a"),
    count_by_age(filter(fisf, TTNT == 2), B4_LABEL, index = "B4b"),
    count_by_education(filter(fisf, TTNT == 2), B4_LABEL, index = "B4c"),
    count_by_ethnicity(filter(fisf, TTNT == 2), B4_LABEL, index = "B4d"),
    count_by_occupation(filter(fisf, TTNT == 2), B4_LABEL, index = "B4e"),
    count_by_median_income(filter(fisf, TTNT == 2), B4_LABEL, index = "B4f")
)

B4C <- add_rsp(B4C, c("Có", "Không", "Không biết"))
B4C <- adjust_colnames(B4C, "Tại khu vực nông thôn")

## Not really elegant, but this works!!!
B4 <- try({
    bind_cols(
        B4A,
        select(B4B, -STT, -`Phân loại ngừơi trả lời`),
        select(B4C, -STT, -`Phân loại ngừơi trả lời`)
    )
}, silent = TRUE)

## Export ------------------------------

if (STRATIFIED_BY_REGION) {
    filename <- paste0("../outputs/SECTION-B ", WHICH_REGION, ".xlsx")
    openxlsx::write.xlsx(list(B1 = B1,
                              B2 = B2),
                         file = filename)
} else {
    filename <- paste0("../outputs/SECTION-B TOANQUOC.xlsx")
    openxlsx::write.xlsx(list(B1 = B1,
                              B2 = B2,
                              B3_1 = B3_0,
                              B3_2 = B3_2,
                              B4 = B4),
                         file = filename)
}
