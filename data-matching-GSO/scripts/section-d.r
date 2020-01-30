## @ancv

## -----------------------------------------------------------------------------
## D - Tiet Kiem

source("02-process.R")

## D1 ------------------------------

## THỰC TRẠNG ĐỂ DÀNH TIỀN/TIẾT KIỆM TIỀN

fisf <- fisf %>%
    mutate(D1_1_LABEL = onehot_encoding(select(fisf, matches("D1_[0-9]$")), 1)) %>%
    mutate(D1_2_LABEL = onehot_encoding(select(fisf, matches("D1_[0-9]$")), 2)) %>%
    mutate(D1_3_LABEL = onehot_encoding(select(fisf, matches("D1_[0-9]$")), 3)) %>%
    mutate(D1_4_LABEL = onehot_encoding(select(fisf, matches("D1_[0-9]$")), 4)) %>%
    mutate(D1_5_LABEL = onehot_encoding(select(fisf, matches("D1_[0-9]$")), 5)) %>%
    mutate(D1_6_LABEL = onehot_encoding(select(fisf, matches("D1_[0-9]$")), 6)) %>%
    mutate(D1_7_LABEL = onehot_encoding(select(fisf, matches("D1_[0-9]$")), 7))

D1_1 <- count_response(fisf, D1_1_LABEL, index = "D1_1")
D1_1 <- add_rsp(D1_1, c("Có", "Không"))
D1_2 <- count_response(fisf, D1_2_LABEL, index = "D1_2")
D1_2 <- add_rsp(D1_2, c("Có", "Không"))
D1_3 <- count_response(fisf, D1_3_LABEL, index = "D1_3")
D1_3 <- add_rsp(D1_3, c("Có", "Không"))
D1_4 <- count_response(fisf, D1_4_LABEL, index = "D1_4")
D1_4 <- add_rsp(D1_4, c("Có", "Không"))
D1_5 <- count_response(fisf, D1_5_LABEL, index = "D1_5")
D1_5 <- add_rsp(D1_5, c("Có", "Không"))
D1_6 <- count_response(fisf, D1_6_LABEL, index = "D1_6")
D1_6 <- add_rsp(D1_6, c("Có", "Không"))
D1_7 <- count_response(fisf, D1_7_LABEL, index = "D1_7")
D1_7 <- add_rsp(D1_7, c("Có", "Không"))

D1 <- bind_cols(
    select(D1_1, STT, `Phân loại ngừơi trả lời`, `Mục đích - Dự phòng ốm đau` = `Có`),
    select(D1_2, `Mục đích - Để chuẩn bị cho tuổi già` = `Có`),
    select(D1_3, `Mục đích - Giáo dục` = `Có`),
    select(D1_4, `Mục đích - Để đầu tư hoặc mở rộng kinh doanh` = `Có`),
    select(D1_5, `Mục đích - Mua tài sản có giá trị lớn trong tương lai` = `Có`),
    select(D1_6, `Mục đích khác` = `Có`),
    select(D1_7, `Không bao giờ để dành tiền/tiết kiệm tiền` = `Có`)
)

## D2 ------------------------------

## TẦN SUẤT ĐỂ DÁNH TIỀN/TIẾT KIỆM TIỀN TRONG 12 THÁNG QUA

fisf <- process_select_one_question(fisf, D2,
                                    rsps = c("1" = "Để dành/tiết kiệm tiền hàng tháng",
                                             "2" = "Để dành/tiết kiệm tiền hàng quý",
                                             "3" = "Để dành/tiết kiệm tiền hàng năm",
                                             "4" = "Không theo định kỳ",
                                             "9" = "Từ chối trả lời"))

D2 <- count_response(fisf, D2_LABEL, index = "D2")
D2 <- add_rsp(D2, c("Để dành/tiết kiệm tiền hàng tháng",
                    "Để dành/tiết kiệm tiền hàng quý",
                    "Để dành/tiết kiệm tiền hàng năm",
                    "Không theo định kỳ", "Từ chối trả lời"))

D1_2 <- bind_cols(
    select(D1, -contains("Không")),
    select(D2, -STT, -`Phân loại ngừơi trả lời`, -`Từ chối trả lời`),
    select(D1, contains("Không"))
)

## D3 ------------------------------

## HÌNH THỨC TIẾT KIỆM

fisf <- fisf %>%
    mutate(D3_1_LABEL = onehot_encoding(select(fisf, matches("D3_[0-9]$")), 1)) %>%
    mutate(D3_2_LABEL = onehot_encoding(select(fisf, matches("D3_[0-9]$")), 2)) %>%
    mutate(D3_3_LABEL = onehot_encoding(select(fisf, matches("D3_[0-9]$")), 3)) %>%
    mutate(D3_4_LABEL = onehot_encoding(select(fisf, matches("D3_[0-9]$")), 4)) %>%
    mutate(D3_5_LABEL = onehot_encoding(select(fisf, matches("D3_[0-9]$")), 5)) %>%
    mutate(D3_6_LABEL = onehot_encoding(select(fisf, matches("D3_[0-9]$")), 6)) %>%
    mutate(D3_7_LABEL = onehot_encoding(select(fisf, matches("D3_[0-9]$")), 7))

fisf <- fisf %>%
    mutate(D3_2_UNIQ_LABEL = {
        apply(select(fisf, matches("D3_[0-9]$")), MARGIN = 1,
              function(x) as.numeric(!any(x != 2, na.rm = TRUE))) ->.;
        ifelse(. == 1, "Có", "Không")
    }) %>%
    mutate(D3_5_UNIQ_LABEL = {
        apply(select(fisf, matches("D3_[0-9]$")), MARGIN = 1,
              function(x) as.numeric(!any(x != 5, na.rm = TRUE))) ->.;
        ifelse(. == 1, "Có", "Không")
    })

D3_1 <- count_response(filter(fisf, D1_7_LABEL != "Không"), D3_1_LABEL, index = "D3")
D3_1 <- add_rsp(D3_1, c("Có", "Không"))
D3_2 <- count_response(filter(fisf, D1_7_LABEL != "Không"), D3_2_LABEL, index = "D3_2")
D3_2 <- add_rsp(D3_2, c("Có", "Không"))
D3_3 <- count_response(filter(fisf, D1_7_LABEL != "Không"), D3_3_LABEL, index = "D3_3")
D3_3 <- add_rsp(D3_3, c("Có", "Không"))
D3_4 <- count_response(filter(fisf, D1_7_LABEL != "Không"), D3_4_LABEL, index = "D3_4")
D3_4 <- add_rsp(D3_4, c("Có", "Không"))
D3_5 <- count_response(filter(fisf, D1_7_LABEL != "Không"), D3_5_LABEL, index = "D3_5")
D3_5 <- add_rsp(D3_5, c("Có", "Không"))
D3_6 <- count_response(filter(fisf, D1_7_LABEL != "Không"), D3_6_LABEL, index = "D3_6")
D3_6 <- add_rsp(D3_6, c("Có", "Không"))
D3_7 <- count_response(filter(fisf, D1_7_LABEL != "Không"), D3_7_LABEL, index = "D3_7")
D3_7 <- add_rsp(D3_7, c("Có", "Không"))

D3_2UNIQ <- count_response(filter(fisf, D1_7_LABEL != "Không"), D3_2_UNIQ_LABEL, index = "D3_2")
D3_2UNIQ <- add_rsp(D3_2UNIQ, c("Có", "Không"))
D3_5UNIQ <- count_response(filter(fisf, D1_7_LABEL != "Không"), D3_5_UNIQ_LABEL, index = "D3_5")
D3_5UNIQ <- add_rsp(D3_5UNIQ, c("Có", "Không"))

D3 <- bind_cols(
    select(D3_1, STT, `Phân loại ngừơi trả lời`, `Giữ tại nhà` = `Có`),
    select(D3_2, `Trong một tài khoản tại ngân hàng hay TCTC khác` = `Có`),
    select(D3_2UNIQ, `Chỉ tiết kiệm tiền tại ngân hàng hay TCTC khác` = `Có`),
    select(D3_3, `Đầu tư vào thị trường chứng khoán` = `Có`),
    select(D3_4, `Hợp đồng ủy thác đầu tư/Quỹ hưu trí/Kế hoạch giáo dục` = `Có`),
    select(D3_5, `Thông qua các nhóm tiết kiệm không chính thức` = `Có`),
    select(D3_5UNIQ, `Chỉ tiết kiệm tiền thông qua các nhóm tiết kiệm không chính thức` = `Có`),
    select(D3_6, `Đầu tư vào tài sản với ý định sẽ bán trong tương lai` = `Có`),
    select(D3_7, `Khác` = `Có`)
)

## Export ------------------------------

if (STRATIFIED_BY_REGION) {
    filename <- paste0("../outputs/SECTION-D ", WHICH_REGION, ".xlsx")
} else {
    filename <- paste0("../outputs/SECTION-D TOANQUOC.xlsx")
}

openxlsx::write.xlsx(list(D1_2 = D1_2,
                          D3 = D3),
                     file = filename)
