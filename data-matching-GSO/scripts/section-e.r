
## -----------------------------------------------------------------------------
## E - Tin Dung

source("02-process.R")

## E1 ------------------------------

fisf <- fisf %>%
    mutate(E1 = fill_missval(E1))

fisf <- fisf %>%
    mutate(E1_LABEL = case_when(
               E1 == 1 ~ "Có",
               E1 == 2 ~ "Không"
           ))

E1 <- count_response(fisf, E1_LABEL, index = "E1")

E1 <- E1 %>%
    select(STT, `Phân loại ngừơi trả lời`,
           `Có vay tiền 1 triệu đồng trở lên` = `Có`,
           `Không vay tiền` = `Không`)

## E2 ------------------------------

fisf <- fisf %>%
    mutate(E2_1_LABEL = onehot_encoding(select(fisf, matches("E2_[0-9]$")), 1)) %>%
    mutate(E2_2_LABEL = onehot_encoding(select(fisf, matches("E2_[0-9]$")), 2)) %>%
    mutate(E2_3_LABEL = onehot_encoding(select(fisf, matches("E2_[0-9]$")), 3)) %>%
    mutate(E2_4_LABEL = onehot_encoding(select(fisf, matches("E2_[0-9]$")), 4)) %>%
    mutate(E2_5_LABEL = onehot_encoding(select(fisf, matches("E2_[0-9]$")), 5)) %>%
    mutate(E2_6_LABEL = onehot_encoding(select(fisf, matches("E2_[0-9]$")), 6)) %>%
    mutate(E2_7_LABEL = onehot_encoding(select(fisf, matches("E2_[0-9]$")), 7)) %>%
    mutate(E2_8_LABEL = onehot_encoding(select(fisf, matches("E2_[0-9]$")), 8)) %>%
    mutate(E2_9_LABEL = onehot_encoding(select(fisf, matches("E2_[0-9]$")), 9))

E2_1 <- count_response(filter(fisf, E1 == 2), E2_1_LABEL, index = "E2_1")
E2_2 <- count_response(filter(fisf, E1 == 2), E2_2_LABEL, index = "E2_2")
E2_3 <- count_response(filter(fisf, E1 == 2), E2_3_LABEL, index = "E2_3")
E2_4 <- count_response(filter(fisf, E1 == 2), E2_4_LABEL, index = "E2_4")
E2_5 <- count_response(filter(fisf, E1 == 2), E2_5_LABEL, index = "E2_5")
E2_6 <- count_response(filter(fisf, E1 == 2), E2_6_LABEL, index = "E2_6")
E2_7 <- count_response(filter(fisf, E1 == 2), E2_7_LABEL, index = "E2_7")
E2_8 <- count_response(filter(fisf, E1 == 2), E2_8_LABEL, index = "E2_8")
E2_9 <- count_response(filter(fisf, E1 == 2), E2_9_LABEL, index = "E2_9")

E2 <- bind_cols(
    select(E2_1, STT, `Phân loại ngừơi trả lời`, `Sợ cảnh nợ nần` = `Không`),
    select(E2_2, `Tự trang trải được chi phí sống` = `Không`),
    select(E2_3, `Lãi suất quá cao` = `Không`),
    select(E2_4, `Không biết vay tiền ở đâu` = `Không`),
    select(E2_5, `Không biết cách làm hồ sơ xin vay` = `Không`),
    select(E2_6, `Không tin các Tổ chức tín dụng` = `Không`),
    select(E2_7, `Vợ/chồng/người thân/gia đình không cho phép vay tiền` = `Không`),
    select(E2_8, `Bị từ chối do không có đủ điều kiện` = `Không`),
    select(E2_9, `Lý do khác` = `Có`)
)

E1_2 <- left_join(
    select(E1, -`Có vay tiền 1 triệu đồng trở lên`),
    select(E2, -STT),
    by = "Phân loại ngừơi trả lời"
)

## E4 ------------------------------

fisf <- fisf %>%
    mutate(E4_1_LABEL = onehot_encoding(select(fisf, matches("E4_[0-9]$")), 1)) %>%
    mutate(E4_2_LABEL = onehot_encoding(select(fisf, matches("E4_[0-9]$")), 2)) %>%
    mutate(E4_3_LABEL = onehot_encoding(select(fisf, matches("E4_[0-9]$")), 3)) %>%
    mutate(E4_4_LABEL = onehot_encoding(select(fisf, matches("E4_[0-9]$")), 4)) %>%
    mutate(E4_5_LABEL = onehot_encoding(select(fisf, matches("E4_[0-9]$")), 5)) %>%
    mutate(E4_6_LABEL = onehot_encoding(select(fisf, matches("E4_[0-9]$")), 6)) %>%
    mutate(E4_7_LABEL = onehot_encoding(select(fisf, matches("E4_[0-9]$")), 7)) %>%
    mutate(E4_8_LABEL = onehot_encoding(select(fisf, matches("E4_[0-9]$")), 8)) %>%
    mutate(E4_9_LABEL = onehot_encoding(select(fisf, matches("E4_[0-9]$")), 9))

E4_1 <- count_response(filter(fisf, E1 == 1), E4_1_LABEL, index = "E4_1")
E4_2 <- count_response(filter(fisf, E1 == 1), E4_2_LABEL, index = "E4_2")
E4_3 <- count_response(filter(fisf, E1 == 1), E4_3_LABEL, index = "E4_3")
E4_4 <- count_response(filter(fisf, E1 == 1), E4_4_LABEL, index = "E4_4")
E4_5 <- count_response(filter(fisf, E1 == 1), E4_5_LABEL, index = "E4_5")
E4_6 <- count_response(filter(fisf, E1 == 1), E4_6_LABEL, index = "E4_6")
E4_7 <- count_response(filter(fisf, E1 == 1), E4_7_LABEL, index = "E4_7")
E4_8 <- count_response(filter(fisf, E1 == 1), E4_8_LABEL, index = "E4_8")
E4_9 <- count_response(filter(fisf, E1 == 1), E4_9_LABEL, index = "E4_9")

E4 <- bind_cols(
    select(E4_1, STT, `Phân loại ngừơi trả lời`, `Nhu cầu tiêu dùng hàng ngày` = `Có`),
    select(E4_2, `Mua nhà hoặc BĐS` = `Có`),
    select(E4_3, `Mua phương tiện đi lại` = `Có`),
    select(E4_4, `Chi phí kinh doanh` = `Có`),
    select(E4_5, `Mua vật nuôi, cây trồng, phân bón` = `Có`),
    select(E4_6, `Tổ chức ngày lễ truyền thống` = `Có`),
    select(E4_7, `Trả nợ` = `Có`),
    select(E4_8, `Chi trả học phí/khám chữa bệnh` = `Có`),
    select(E4_9, `Lý do khác` = `Có`)
)

E1_4 <- left_join(
    select(E1, -`Không vay tiền`),
    select(E4, -STT),
    by = "Phân loại ngừơi trả lời"
)

## E3 ------------------------------

fisf <- fisf %>%
    mutate(E3_1_LABEL = onehot_encoding(select(fisf, matches("E3_[0-9]$")), 1)) %>%
    mutate(E3_2_LABEL = onehot_encoding(select(fisf, matches("E3_[0-9]$")), 2)) %>%
    mutate(E3_3_LABEL = onehot_encoding(select(fisf, matches("E3_[0-9]$")), 3)) %>%
    mutate(E3_4_LABEL = onehot_encoding(select(fisf, matches("E3_[0-9]$")), 4)) %>%
    mutate(E3_5_LABEL = onehot_encoding(select(fisf, matches("E3_[0-9]$")), 5)) %>%
    mutate(E3_6_LABEL = onehot_encoding(select(fisf, matches("E3_[0-9]$")), 6)) %>%
    mutate(E3_7_LABEL = onehot_encoding(select(fisf, matches("E3_[0-9]$")), 7)) %>%
    mutate(E3_8_LABEL = onehot_encoding(select(fisf, matches("E3_[0-9]$")), 8)) %>%
    mutate(E3_9_LABEL = onehot_encoding(select(fisf, matches("E3_[0-9]$")), 9)) %>%
    mutate(E3_10_LABEL = onehot_encoding(select(fisf, matches("E3_[0-9]$")), 10)) %>%
    mutate(E3_11_LABEL = onehot_encoding(select(fisf, matches("E3_[0-9]$")), 11))

E3_1 <- count_response(filter(fisf, E1 == 1), E3_1_LABEL, index = "E3_1")
E3_2 <- count_response(filter(fisf, E1 == 1), E3_2_LABEL, index = "E3_2")
E3_3 <- count_response(filter(fisf, E1 == 1), E3_3_LABEL, index = "E3_3")
E3_4 <- count_response(filter(fisf, E1 == 1), E3_4_LABEL, index = "E3_4")
E3_5 <- count_response(filter(fisf, E1 == 1), E3_5_LABEL, index = "E3_5")
E3_6 <- count_response(filter(fisf, E1 == 1), E3_6_LABEL, index = "E3_6")
E3_7 <- count_response(filter(fisf, E1 == 1), E3_7_LABEL, index = "E3_7")
E3_8 <- count_response(filter(fisf, E1 == 1), E3_8_LABEL, index = "E3_8")
E3_9 <- count_response(filter(fisf, E1 == 1), E3_9_LABEL, index = "E3_9")
E3_10 <- count_response(filter(fisf, E1 == 1), E3_10_LABEL, index = "E3_10")
E3_11 <- count_response(filter(fisf, E1 == 1), E3_11_LABEL, index = "E3_11")

E3 <- bind_cols(
    select(E3_1, STT, `Phân loại ngừơi trả lời`, `NHTM` = `Có`),
    select(E3_2, `NHCSXH` = `Có`),
    select(E3_3, `TCTC khác` = `Có`),
    select(E3_4, `Vay theo chương trình hỗ trợ của Chính phủ` = `Có`),
    select(E3_5, `Cơ quan/chủ nơi làm việc` = `Có`),
    select(E3_6, `Gia đình, họ hàng hay bạn bè` = `Có`),
    select(E3_7, `Từ một bên cho vay tư nhân` = `Có`),
    select(E3_8, `Từ các bên cho vay mang tính chất tương hỗ, thiện nguyện` = `Có`),
    select(E3_9, `Bên mua/bán sản phẩm nông nghiệp` = `Có`),
    select(E3_10, `Tổ chức chính trị xã hội` = `Có`),
    select(E3_11, `Nguồn khác` = `Có`)
)

## E5 ------------------------------

fisf <- fisf %>%
    mutate(E5 = fill_missval(E5))

fisf <- fisf %>%
    mutate(E5_LABEL = case_when(
               E5 == 1 ~ "Có",
               E5 == 2 ~ "Không"
           ))

E5 <- count_response(fisf, E5_LABEL, index = "E5")

E5 <- E5 %>%
    select(STT, `Phân loại ngừơi trả lời`,
           `% người trên 18 tuổi có khoản vay quá hạn/tổng số người đi vay trên 1 triệu đồng` = `Có`,
           `% người trên 18 tuổi không có khoản vay quá hạn/tổng số người đi vay trên 1 triệu đồng` = `Không`)

## E6 ------------------------------

fisf <- fisf %>%
    mutate(E6 = fill_missval(E6))

fisf <- fisf %>%
    mutate(E6_LABEL = case_when(
               E6 == 1 ~ "Có",
               E6 == 2 ~ "Không"
           ))

E6 <- count_response(fisf, E6_LABEL, index = "E6")

E6 <- E6 %>%
    select(STT, `Phân loại ngừơi trả lời`,
           `% người trên 18 tuổi có kế hoạch trả nợ khoản vay quá hạn/tổng số người có khoản vay quá hạn` = `Có`,
           `% người trên 18 tuổi không có kế hoạch trả nợ khoản vay quá hạn/tổng số người có khoản vay quá hạn` = `Không`)

## E7 ------------------------------

NNfisf <- fisf %>%
    mutate(E7 = fill_missval(E7))

fisf <- fisf %>%
    mutate(E7_LABEL = case_when(
               E7 == 1 ~ "Có",
               E7 == 2 ~ "Không"
           ))

E7 <- count_response(fisf, E7_LABEL, index = "E7")

E7 <- E7 %>%
    select(STT, `Phân loại ngừơi trả lời`,
           `% người trên 18 tuổi có thẻ tín dụng/tổng số người trên 18 tuổi` = `Có`,
           `% người trên 18 tuổi có thẻ tín dụng/tổng số người trên 18 tuổi vay tiền từ 1 triệu đồng trở lên` = `Không`)


## E8 ------------------------------

fisf <- fisf %>%
    mutate(E8 = fill_missval(E8))

fisf <- fisf %>%
    mutate(E8_LABEL = case_when(
               E8 == 1 ~ "Có",
               E8 == 2 ~ "Không"
           ))

E8 <- count_response(fisf, E8_LABEL, index = "E8")

E8 <- E8 %>%
    select(STT, `Phân loại ngừơi trả lời`,
           `% người trên 18 tuổi có sử dụng thẻ tín dụng trong 12 tháng qua/tổng số người trên 18 tuổi` = `Có`,
           `% người trên 18 tuổi có sử dụng thẻ tín dụng trong 12 tháng qua/tổng số người trên 18 tuổi có thẻ tín dụng` = `Không`)

E5_6_7_8 <- bind_cols(
    E5,
    select(G6, -STT, -`Phân loại ngừơi trả lời`),
    select(G7, -STT, -`Phân loại ngừơi trả lời`),
    select(G8, -STT, -`Phân loại ngừơi trả lời`)
)

## Export ------------------------------

openxlsx::write.xlsx(list(E1_2 = E1_2,
                          E1_4 = E1_4,
                          E3 = E3,
                          E5_6_7_8 = E5_6_7_8),
                     file = "../outputs/SECTION-E.xlsx")
