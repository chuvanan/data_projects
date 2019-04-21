## @ancv

## -----------------------------------------------------------------------------
## G - Hieu Biet Tai Chinh

source("02-process.R")

## G1 ------------------------------

fisf <- fisf %>%
    mutate(G1 = fill_missval(G1))

fisf <- fisf %>%
    mutate(G1_LABEL = case_when(
               G1 == 1 ~ "Có",
               G1 == 2 ~ "Không",
               G1 == 8 ~ "Không biết"
           ))

G1 <- count_response(fisf, G1_LABEL, index = "G1")
G1 <- G1 %>%
    select(STT, `Phân loại ngừơi trả lời`,
           `Có huy động đuợc` = `Có`,
           `Không huy động được` = `Không`)

## G2 ------------------------------

fisf <- fisf %>%
    mutate(G2_1_LABEL = onehot_encoding(select(fisf, matches("G2_[0-9]$")), 1)) %>%
    mutate(G2_2_LABEL = onehot_encoding(select(fisf, matches("G2_[0-9]$")), 2)) %>%
    mutate(G2_3_LABEL = onehot_encoding(select(fisf, matches("G2_[0-9]$")), 3)) %>%
    mutate(G2_4_LABEL = onehot_encoding(select(fisf, matches("G2_[0-9]$")), 4)) %>%
    mutate(G2_5_LABEL = onehot_encoding(select(fisf, matches("G2_[0-9]$")), 5)) %>%
    mutate(G2_6_LABEL = onehot_encoding(select(fisf, matches("G2_[0-9]$")), 6)) %>%
    mutate(G2_7_LABEL = onehot_encoding(select(fisf, matches("G2_[0-9]$")), 7)) %>%
    mutate(G2_8_LABEL = onehot_encoding(select(fisf, matches("G2_[0-9]$")), 8)) %>%
    mutate(G2_9_LABEL = onehot_encoding(select(fisf, matches("G2_[0-9]$")), 9))

G2_1 <- count_response(fisf, G2_1_LABEL, index = "G2_1")
G2_2 <- count_response(fisf, G2_2_LABEL, index = "G2_2")
G2_3 <- count_response(fisf, G2_3_LABEL, index = "G2_3")
G2_4 <- count_response(fisf, G2_4_LABEL, index = "G2_4")
G2_5 <- count_response(fisf, G2_5_LABEL, index = "G2_5")
G2_6 <- count_response(fisf, G2_6_LABEL, index = "G2_6")
G2_7 <- count_response(fisf, G2_7_LABEL, index = "G2_7")
G2_8 <- count_response(fisf, G2_8_LABEL, index = "G2_8")
G2_9 <- count_response(fisf, G2_9_LABEL, index = "G2_9")

G2 <- bind_cols(
    select(G2_1, STT, `Phân loại ngừơi trả lời`, `Nguồn tiết kiệm` = `Có`),
    select(G2_2, `Xin từ gia đình, người thân hoặc bạn bè` = `Có`),
    select(G2_3, `Từ bán tài sản` = `Có`),
    select(G2_4, `Tiền làm thêm` = `Có`),
    select(G2_5, `Vay từ gia đình, người thân hoặc bạn bè` = `Có`),
    select(G2_6, `Vay từ TCTD hoặc TCTC khác` = `Có`),
    select(G2_7, `Vay từ nguồn hỗ trợ của Nhà nước hoặc cộng đồng` = `Có`),
    select(G2_8, `Vay từ người chủ lao động hoặc bên cho vay tư nhân` = `Có`),
    select(G2_9, `Nguồn khác` = `Có`)
)

G1_2 <- bind_cols(
    G1,
    select(G2, -STT, -`Phân loại ngừơi trả lời`)
)

## G3 ------------------------------

fisf <- fisf %>%
    mutate(G3 = fill_missval(G3))

fisf <- fisf %>%
    mutate(G3_LABEL = case_when(
               G3 == 1 ~ "Thường xuyên",
               G3 == 2 ~ "Đôi khi (một số lần)",
               G3 == 3 ~ "Không bao giờ"
           )) %>%
    mutate(G3_LABEL = paste("% người trên 18 tuổi thiếu tiền để trang trải chi phí sinh hoạt cần thiết", "-", G3_LABEL))

G3 <- count_response(fisf, G3_LABEL, index = "G3")

## G4 ------------------------------

fisf <- fisf %>%
    mutate(G4 = fill_missval(G4))

fisf <- fisf %>%
    mutate(G4_LABEL = case_when(
               G4 == 1 ~ "Có kế hoạch chi tiêu định kỳ",
               G4 == 2 ~ "Có kế hoạch chi tiêu tùy theo mức thu nhập",
               G4 == 3 ~ "Không có kế hoạch chi tiêu"
           )) %>%
    mutate(G4_LABEL = paste("% người trên 18 tuổi", "-", G4_LABEL))

G4 <- count_response(fisf, G4_LABEL, index = "G4")

## G5 ------------------------------

fisf <- fisf %>%
    mutate(G5 = fill_missval(G5))

fisf <- fisf %>%
    mutate(G5_LABEL = case_when(
               G5 == 1 ~ "Có và đã được giải quyết",
               G5 == 2 ~ "Có nhưng không được giải quyết/giải quyết không thỏa đáng",
               G5 == 3 ~ "Không"
           )) %>%
    mutate(G5_LABEL = paste("% người trên 18 tuổi có gặp vấn đề/trục trặc với ngân hàng hoặc TCTC khác", "-", G5_LABEL))

G5 <- count_response(fisf, G5_LABEL, index = "G5")
G5 <- G5 %>% select(-ends_with("Không"))

## G6 ------------------------------

fisf <- fisf %>%
    mutate(G6 = fill_missval(G6))

fisf <- fisf %>%
    mutate(G6_LABEL = case_when(
               G6 == 1 ~ "Có",
               G6 == 2 ~ "Không"
           ))

G6 <- count_response(fisf, G6_LABEL, index = "G6")
G6 <- G6 %>%
    select(STT, `Phân loại ngừơi trả lời`,
           `% người trên 18 tuổi có biết về CIC` = `Có`)


## G7 ------------------------------

fisf <- fisf %>%
    mutate(G7 = fill_missval(G7))

fisf <- fisf %>%
    mutate(G7_LABEL = case_when(
               G7 == 1 ~ "Có",
               G7 == 2 ~ "Không"
           ))

G7 <- count_response(fisf, G7_LABEL, index = "G7")
G7 <- G7 %>%
    select(STT, `Phân loại ngừơi trả lời`,
           `% người trên 18 tuổi có biết lịch sử tín dụng của bản thân có ảnh hưởng đến quyết định của TCTC khi phê duyệt đề nghị vay vốn` = `Có`)

G3_4_5_6_7 <- bind_cols(
    G3,
    select(G4, -STT, -`Phân loại ngừơi trả lời`),
    select(G5, -STT, -`Phân loại ngừơi trả lời`),
    select(G6, -STT, -`Phân loại ngừơi trả lời`),
    select(G7, -STT, -`Phân loại ngừơi trả lời`)
)

## G8 ------------------------------

fisf <- fisf %>%
    mutate(G8A = fill_missval(G8A),
           G8B = fill_missval(G8B),
           G8C = fill_missval(G8C),
           G8D = fill_missval(G8D),
           G8E = fill_missval(G8E),
           G8F = fill_missval(G8F),
           G8G = fill_missval(G8G),
           G8H = fill_missval(G8H),
           G8I = fill_missval(G8I),
           G8J = fill_missval(G8J),
           G8K = fill_missval(G8K),
           G8L = fill_missval(G8L))

fisf <- fisf %>%
    mutate(G8A_LABEL = G8A,
           G8B_LABEL = G8B,
           G8C_LABEL = G8C,
           G8D_LABEL = G8D,
           G8E_LABEL = G8E,
           G8F_LABEL = G8F,
           G8G_LABEL = G8G,
           G8H_LABEL = G8H,
           G8I_LABEL = G8I,
           G8J_LABEL = G8J,
           G8K_LABEL = G8K,
           G8L_LABEL = G8L)

fisf <- fisf %>%
    mutate_at(c("G8A_LABEL", "G8B_LABEL", "G8C_LABEL", "G8D_LABEL",
                "G8E_LABEL", "G8F_LABEL", "G8G_LABEL", "G8H_LABEL",
                "G8I_LABEL", "G8J_LABEL", "G8K_LABEL", "G8L_LABEL"),
              function(x) case_when(
                              x == 1 ~ "Không biết",
                              x == 2 ~ "Có nghe đến",
                              x == 3 ~ "Có biết",
                              x == 4 ~ "Biết rõ",
                              )) %>%
    mutate_at(c("G8A_LABEL", "G8B_LABEL", "G8C_LABEL", "G8D_LABEL",
                "G8E_LABEL", "G8F_LABEL", "G8G_LABEL", "G8H_LABEL",
                "G8I_LABEL", "G8J_LABEL", "G8K_LABEL", "G8L_LABEL"),
              function(x) factor(x, levels = c("Không biết", "Có nghe đến", "Có biết", "Biết rõ"),
                                 ordered = TRUE))

G8_1 <- bind_rows(
    count_all(fisf, G8A_LABEL, index = "G8.1"),
    count_all(fisf, G8B_LABEL, index = "G8.2"),
    count_all(fisf, G8C_LABEL, index = "G8.3"),
    count_all(fisf, G8D_LABEL, index = "G8.4"),
    count_all(fisf, G8E_LABEL, index = "G8.5"),
    count_all(fisf, G8F_LABEL, index = "G8.6"),
    count_all(fisf, G8G_LABEL, index = "G8.7"),
    count_all(fisf, G8H_LABEL, index = "G8.8"),
    count_all(fisf, G8I_LABEL, index = "G8.9"),
    count_all(fisf, G8J_LABEL, index = "G8.10"),
    count_all(fisf, G8K_LABEL, index = "G8.11"),
    count_all(fisf, G8L_LABEL, index = "G8.12")
)

G8_1 <- G8_1 %>%
    mutate(`Phân loại ngừơi trả lời` = case_when(
               STT == "G8.1" ~ "Tài khoản tiền gửi thanh toán",
               STT == "G8.2" ~ "Ví điện tử/thẻ trả trước",
               STT == "G8.3" ~ "Thẻ ghi nợ",
               STT == "G8.4" ~ "Thẻ tín dụng",
               STT == "G8.5" ~ "Tiền gửi tại các Tổ chức tín dụng để hưởng lãi",
               STT == "G8.6" ~ "Sản phẩm hưu trí",
               STT == "G8.7" ~ "Vay vốn bảo đảm bằng tài sản",
               STT == "G8.8" ~ "Vay vốn không có tài sản bảo đảm (tín chấp)",
               STT == "G8.9" ~ "Sản phẩm đầu tư",
               STT == "G8.10" ~ "Bảo hiểm nhân thọ",
               STT == "G8.11" ~ "Bảo hiểm phi nhân thọ",
               STT == "G8.12" ~ "Tài chính vi mô"
           )) %>%
    rename(`Các sản phẩm, dịch vụ của các tổ chức được cấp phép hoạt động` = `Phân loại ngừơi trả lời`)
## mutate(`Chỉ số cân bằng` = round(`Biết rõ` + `Có biết` * 0.5 - `Có nghe đến` * 0.5 - `Không biết`, 0))

fisf <- fisf %>%
    mutate(G8A_LABEL = paste("Tài khoản tiền gửi thanh toán", "-", G8A_LABEL),
           G8B_LABEL = paste("Ví điện tử/thẻ trả trước", "-", G8B_LABEL),
           G8C_LABEL = paste("Thẻ ghi nợ", "-", G8C_LABEL),
           G8D_LABEL = paste("Thẻ tín dụng", "-", G8D_LABEL),
           G8E_LABEL = paste("Tiền gửi tại các tổ chức tín dụng để hưởng lãi", "-", G8E_LABEL),
           G8F_LABEL = paste("Sản phẩm hưu trí", "-", G8F_LABEL),
           G8G_LABEL = paste("Vay vốn bảo đảm bằng tài sản", "-", G8G_LABEL),
           G8H_LABEL = paste("Vay vốn không có tài sản bảo đảm", "-", G8H_LABEL),
           G8I_LABEL = paste("Sản phẩm đầu tư", "-", G8I_LABEL),
           G8J_LABEL = paste("Bảo hiểm nhân thọ", "-", G8J_LABEL),
           G8K_LABEL = paste("Bảo hiểm phi nhân thọ", "-", G8K_LABEL),
           G8L_LABEL = paste("Tài chính vi mô", "-", G8L_LABEL))

G8A <- count_response(fisf, G8A_LABEL, index = "G8")
G8B <- count_response(fisf, G8B_LABEL, index = "G8B")
G8C <- count_response(fisf, G8C_LABEL, index = "G8C")
G8D <- count_response(fisf, G8D_LABEL, index = "G8D")
G8E <- count_response(fisf, G8E_LABEL, index = "G8E")
G8F <- count_response(fisf, G8F_LABEL, index = "G8F")
G8G <- count_response(fisf, G8G_LABEL, index = "G8G")
G8H <- count_response(fisf, G8H_LABEL, index = "G8H")
G8I <- count_response(fisf, G8I_LABEL, index = "G8I")
G8J <- count_response(fisf, G8J_LABEL, index = "G8J")
G8K <- count_response(fisf, G8K_LABEL, index = "G8K")
G8L <- count_response(fisf, G8L_LABEL, index = "G8L")

G8_2 <- bind_cols(
    G8A,
    select(G8B, -STT, -`Phân loại ngừơi trả lời`),
    select(G8C, -STT, -`Phân loại ngừơi trả lời`),
    select(G8D, -STT, -`Phân loại ngừơi trả lời`),
    select(G8E, -STT, -`Phân loại ngừơi trả lời`),
    select(G8F, -STT, -`Phân loại ngừơi trả lời`),
    select(G8G, -STT, -`Phân loại ngừơi trả lời`),
    select(G8H, -STT, -`Phân loại ngừơi trả lời`),
    select(G8I, -STT, -`Phân loại ngừơi trả lời`),
    select(G8J, -STT, -`Phân loại ngừơi trả lời`),
    select(G8K, -STT, -`Phân loại ngừơi trả lời`),
    select(G8L, -STT, -`Phân loại ngừơi trả lời`)
)

## G9 ------------------------------

fisf <- fisf %>%
    mutate(G9A = fill_missval(G9A),
           G9B = fill_missval(G9B),
           G9C = fill_missval(G9C),
           G9D = fill_missval(G9D),
           G9E = fill_missval(G9E),
           G9F = fill_missval(G9F),
           G9G = fill_missval(G9G),
           G9H = fill_missval(G9H))

fisf <- fisf %>%
    mutate(G9A_LABEL = G9A,
           G9B_LABEL = G9B,
           G9C_LABEL = G9C,
           G9D_LABEL = G9D,
           G9E_LABEL = G9E,
           G9F_LABEL = G9F,
           G9G_LABEL = G9G,
           G9H_LABEL = G9H)

fisf <- fisf %>%
    mutate_at(c("G9A_LABEL", "G9B_LABEL", "G9C_LABEL", "G9D_LABEL",
                "G9E_LABEL", "G9F_LABEL", "G9G_LABEL", "G9H_LABEL"),
              function(x) case_when(
                              x == 1 ~ "Không tin tưởng",
                              x == 2 ~ "Ít tin tưởng",
                              x == 3 ~ "Tin tưởng",
                              x == 8 ~ "Không biết"
                          )) %>%
    mutate_at(c("G9A_LABEL", "G9B_LABEL", "G9C_LABEL", "G9D_LABEL",
                "G9E_LABEL", "G9F_LABEL", "G9G_LABEL", "G9H_LABEL"),
              function(x) factor(x, levels = c("Không tin tưởng", "Ít tin tưởng", "Tin tưởng", "Không biết"),
                                 ordered = TRUE))

G9_1 <- bind_rows(
    count_all(fisf, G9A_LABEL, index = "G9.1"),
    count_all(fisf, G9B_LABEL, index = "G9.2"),
    count_all(fisf, G9C_LABEL, index = "G9.3"),
    count_all(fisf, G9D_LABEL, index = "G9.4"),
    count_all(fisf, G9E_LABEL, index = "G9.5"),
    count_all(fisf, G9F_LABEL, index = "G9.6"),
    count_all(fisf, G9G_LABEL, index = "G9.7"),
    count_all(fisf, G9H_LABEL, index = "G9.8")
)

G9_1 <- G9_1 %>%
    mutate(`Phân loại ngừơi trả lời` = case_when(
               STT == "G9.1" ~ "Ngân hàng thương mại",
               STT == "G9.2" ~ "Ngân hàng Chính sách xã hội",
               STT == "G9.3" ~ "Tổ chức tài chính vi mô",
               STT == "G9.4" ~ "Quỹ tín dụng nhân dân",
               STT == "G9.5" ~ "Tổ chức bảo hiểm nhân thọ",
               STT == "G9.6" ~ "Công ty quản lý quỹ",
               STT == "G9.7" ~ "Tổ chức tín dụng phi ngân hàng",
               STT == "G9.8" ~ "Tổ chức cung ứng dịch vụ trung gian thanh toán"
           )) %>%
    rename(`Các tổ chức tài chính` = `Phân loại ngừơi trả lời`)

fisf <- fisf %>%
    mutate(G9A_LABEL = paste("Ngân hàng thương mại", "-", G9A_LABEL),
           G9B_LABEL = paste("Ngân hàng Chính sách xã hội", "-", G9B_LABEL),
           G9C_LABEL = paste("Tổ chức tài chính vi mô", "-", G9C_LABEL),
           G9D_LABEL = paste("Quỹ tín dụng nhân dân", "-", G9D_LABEL),
           G9E_LABEL = paste("Tổ chức bảo hiểm nhân thọ", "-", G9E_LABEL),
           G9F_LABEL = paste("Công ty quản lý quỹ", "-", G9F_LABEL),
           G9G_LABEL = paste("Tổ chức tín dụng phi ngân hàng", "-", G9G_LABEL),
           G9H_LABEL = paste("Tổ chức cung ứng dịch vụ trung gian thanh toán", "-", G9H_LABEL))

G9A <- count_response(fisf, G9A_LABEL, index = "G9")
G9B <- count_response(fisf, G9B_LABEL, index = "G9B")
G9C <- count_response(fisf, G9C_LABEL, index = "G9C")
G9D <- count_response(fisf, G9D_LABEL, index = "G9D")
G9E <- count_response(fisf, G9E_LABEL, index = "G9E")
G9F <- count_response(fisf, G9F_LABEL, index = "G9F")
G9G <- count_response(fisf, G9G_LABEL, index = "G9G")
G9H <- count_response(fisf, G9H_LABEL, index = "G9H")

G9_2 <- bind_cols(
    G9A,
    select(G9B, -STT, -`Phân loại ngừơi trả lời`),
    select(G9C, -STT, -`Phân loại ngừơi trả lời`),
    select(G9D, -STT, -`Phân loại ngừơi trả lời`),
    select(G9E, -STT, -`Phân loại ngừơi trả lời`),
    select(G9F, -STT, -`Phân loại ngừơi trả lời`),
    select(G9G, -STT, -`Phân loại ngừơi trả lời`),
    select(G9H, -STT, -`Phân loại ngừơi trả lời`)
)

G9_2 <- select(G9_2, -contains("Không biết"))


## G10 ------------------------------

fisf <- fisf %>%
    mutate(G10A = fill_missval(G10A),
           G10B = fill_missval(G10B))

fisf <- fisf %>%
    mutate(G10A_LABEL = G10A,
           G10B_LABEL = G10B)

fisf <- fisf %>%
    mutate_at(c("G10A_LABEL"),
              function(x) case_when(
                              x == 1 ~ "Hàng ngày - Ghi thu nhập",
                              x == 2 ~ "Hàng tuần - Ghi thu nhập",
                              x == 3 ~ "Hàng tháng - Ghi thu nhập",
                              x == 4 ~ "Hàng năm - Ghi thu nhập",
                              x == 5 ~ "Thỉng thoảng - Ghi thu nhập",
                              x == 6 ~ "Không bao giờ - Ghi thu nhập"
                          ))

fisf <- fisf %>%
    mutate_at(c("G10B_LABEL"),
              function(x) case_when(
                              x == 1 ~ "Hàng ngày - Ghi chi phí",
                              x == 2 ~ "Hàng tuần - Ghi chi phí",
                              x == 3 ~ "Hàng tháng - Ghi chi phí",
                              x == 4 ~ "Hàng năm - Ghi chi phí",
                              x == 5 ~ "Thỉng thoảng - Ghi chi phí",
                              x == 6 ~ "Không bao giờ - Ghi chi phí"
                          ))

G10A <- count_response(fisf, G10A_LABEL, index = "G10")
G10B <- count_response(fisf, G10B_LABEL, index = "G10B")
G10 <- bind_cols(G10A, select(G10B, -STT, -`Phân loại ngừơi trả lời`))
G10 <- select(G10, STT, `Phân loại ngừơi trả lời`,
              contains("Hàng ngày"), contains("Hàng tuần"), contains("Hàng tháng"),
              contains("Hàng năm"), contains("Thỉng thoảng"), contains("Không bao giờ"))


## G11 ------------------------------

fisf <- fisf %>%
    mutate(G11 = fill_missval(G11))

fisf <- fisf %>%
    mutate(G11_LABEL = case_when(
               G11 == 1 ~ "Có",
               G11 == 2 ~ "Không",
               G11 == 8 ~ "Không nhớ/Không biết"
           ))

G11 <- count_response(fisf, G11_LABEL, index = "G11")

G11 <- select(G11, STT, `Phân loại ngừơi trả lời`,
              `% người trên 18 tuổi được dạy về các kiến thức tài chính` = `Có`,
              `% người trên 18 tuổi không được dạy về các kiến thức tài chính` = `Không`,
              `% người trên 18 tuổi không rõ có được dạy về các kiến thức tài chính` = `Không nhớ/Không biết`)

## G12 ------------------------------

fisf <- fisf %>%
    mutate(G12_1_LABEL = onehot_encoding(select(fisf, matches("G12_[0-9]$")), 1)) %>%
    mutate(G12_2_LABEL = onehot_encoding(select(fisf, matches("G12_[0-9]$")), 2)) %>%
    mutate(G12_3_LABEL = onehot_encoding(select(fisf, matches("G12_[0-9]$")), 3)) %>%
    mutate(G12_4_LABEL = onehot_encoding(select(fisf, matches("G12_[0-9]$")), 4)) %>%
    mutate(G12_5_LABEL = onehot_encoding(select(fisf, matches("G12_[0-9]$")), 5)) %>%
    mutate(G12_6_LABEL = onehot_encoding(select(fisf, matches("G12_[0-9]$")), 6)) %>%
    mutate(G12_7_LABEL = onehot_encoding(select(fisf, matches("G12_[0-9]$")), 7)) %>%
    mutate(G12_8_LABEL = onehot_encoding(select(fisf, matches("G12_[0-9]$")), 8)) %>%
    mutate(G12_9_LABEL = onehot_encoding(select(fisf, matches("G12_[0-9]$")), 9))

G12_1 <- count_response(fisf, G12_1_LABEL, index = "G12_1")
G12_2 <- count_response(fisf, G12_2_LABEL, index = "G12_2")
G12_3 <- count_response(fisf, G12_3_LABEL, index = "G12_3")
G12_4 <- count_response(fisf, G12_4_LABEL, index = "G12_4")
G12_5 <- count_response(fisf, G12_5_LABEL, index = "G12_5")
G12_6 <- count_response(fisf, G12_6_LABEL, index = "G12_6")
G12_7 <- count_response(fisf, G12_7_LABEL, index = "G12_7")
G12_8 <- count_response(fisf, G12_8_LABEL, index = "G12_8")
G12_9 <- count_response(fisf, G12_9_LABEL, index = "G12_9")

G12 <- bind_cols(
    select(G12_1, STT, `Phân loại ngừơi trả lời`, `Tự quyết định` = `Có`),
    select(G12_2, `Vợ/chồng` = `Có`),
    select(G12_3, `Họ hàng, người thân` = `Có`),
    select(G12_4, `Đồng nghiệp` = `Có`),
    select(G12_5, `Chuyên viên tư vấn tài chính` = `Có`),
    select(G12_6, `Ngân hàng` = `Có`),
    select(G12_7, `Internet` = `Có`),
    select(G12_8, `Các nhóm, hội tài chính` = `Có`),
    select(G12_9, `Khác` = `Có`)
)

G11_12 <- bind_cols(G11, select(G12, -STT, -`Phân loại ngừơi trả lời`))

## G13 ------------------------------

fisf <- fisf %>%
    mutate(G13_1_LABEL = onehot_encoding(select(fisf, matches("G13_[0-9]$")), 1)) %>%
    mutate(G13_2_LABEL = onehot_encoding(select(fisf, matches("G13_[0-9]$")), 2)) %>%
    mutate(G13_3_LABEL = onehot_encoding(select(fisf, matches("G13_[0-9]$")), 3)) %>%
    mutate(G13_4_LABEL = onehot_encoding(select(fisf, matches("G13_[0-9]$")), 4)) %>%
    mutate(G13_5_LABEL = onehot_encoding(select(fisf, matches("G13_[0-9]$")), 5)) %>%
    mutate(G13_6_LABEL = onehot_encoding(select(fisf, matches("G13_[0-9]$")), 6))

G13_1 <- count_response(fisf, G13_1_LABEL, index = "G13")
G13_2 <- count_response(fisf, G13_2_LABEL, index = "G13_2")
G13_3 <- count_response(fisf, G13_3_LABEL, index = "G13_3")
G13_4 <- count_response(fisf, G13_4_LABEL, index = "G13_4")
G13_5 <- count_response(fisf, G13_5_LABEL, index = "G13_5")
G13_6 <- count_response(fisf, G13_6_LABEL, index = "G13_6")

G13 <- bind_cols(
    select(G13_1, STT, `Phân loại ngừơi trả lời`, `Gửi ngân hàng` = `Có`),
    select(G13_2, `Chơi hụi/gửi tiền các nhóm, phường hội tiết kiệm` = `Có`),
    select(G13_3, `Gửi người quen tin cậy giữ hộ` = `Có`),
    select(G13_4, `Giữ tại một nơi an toàn trong nhà` = `Có`),
    select(G13_5, `Quy đổi vàng hoặc ngoại tệ để giữ` = `Có`),
    select(G13_6, `Khác` = `Có`)
)


## G14 ------------------------------

fisf <- fisf %>%
    mutate(G14_1_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 1)) %>%
    mutate(G14_2_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 2)) %>%
    mutate(G14_3_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 3)) %>%
    mutate(G14_4_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 4)) %>%
    mutate(G14_5_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 5)) %>%
    mutate(G14_6_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 6)) %>%
    mutate(G14_7_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 7)) %>%
    mutate(G14_8_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 8)) %>%
    mutate(G14_9_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 9)) %>%
    mutate(G14_10_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 10)) %>%
    mutate(G14_11_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 11)) %>%
    mutate(G14_12_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 12)) %>%
    mutate(G14_13_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 13)) %>%
    mutate(G14_14_LABEL = onehot_encoding(select(fisf, matches("G14_[0-9]$")), 14))

G14_1 <- count_response(fisf, G14_1_LABEL, index = "G14")
G14_2 <- count_response(fisf, G14_2_LABEL, index = "G14_2")
G14_3 <- count_response(fisf, G14_3_LABEL, index = "G14_3")
G14_4 <- count_response(fisf, G14_4_LABEL, index = "G14_4")
G14_5 <- count_response(fisf, G14_5_LABEL, index = "G14_5")
G14_6 <- count_response(fisf, G14_6_LABEL, index = "G14_6")
G14_7 <- count_response(fisf, G14_7_LABEL, index = "G14_7")
G14_8 <- count_response(fisf, G14_8_LABEL, index = "G14_8")
G14_9 <- count_response(fisf, G14_9_LABEL, index = "G14_9")
G14_10 <- count_response(fisf, G14_10_LABEL, index = "G14_10")
G14_11 <- count_response(fisf, G14_11_LABEL, index = "G14_11")
G14_12 <- count_response(fisf, G14_12_LABEL, index = "G14_12")
G14_13 <- count_response(fisf, G14_13_LABEL, index = "G14_13")
G14_14 <- count_response(fisf, G14_14_LABEL, index = "G14_14")

G14 <- bind_cols(
    select(G14_1, STT, `Phân loại ngừơi trả lời`, `Bị mất tiền` = `Có`),
    select(G14_2, `Được tư vấn sai định hướng tài chính` = `Có`),
    select(G14_3, `Nguồn thu nhập chính bị mất` = `Có`),
    select(G14_4, `Không đủ tiền để trang trải cho vài năm tới hoặc khi về già` = `Có`),
    select(G14_5, `Không đủ tiền mua lương thực, thực phẩm` = `Có`),
    select(G14_6, `Không đủ tiền mua, đóng tiền bảo hiểm` = `Có`),
    select(G14_7, `Không đủ tiền trả nợ` = `Có`),
    select(G14_8, `Thất bại trong việc kinh doanh` = `Có`),
    select(G14_9, `Phải cho tiền bạn bè/gia đình-họ hàng/hàng xóm-cộng đồng quá nhiều` = `Có`),
    select(G14_10, `Tài chính cho con đi học` = `Có`),
    select(G14_11, `Sự an toàn của các thành viên cũng như tài sản của hộ gia đình mình` = `Có`),
    select(G14_12, `Những khoản chi tiêu không lường trước được` = `Có`),
    select(G14_13, `Thảm họa thiên nhiên không lường trước được` = `Có`),
    select(G14_14, `Khác` = `Có`)
)


## G15 ------------------------------

fisf <- fisf %>%
    mutate(G15 = fill_missval(G15))

fisf <- fisf %>%
    mutate(G15_LABEL = case_when(
               G15 == 1 ~ "Toàn bộ/Hầu hết",
               G15 == 2 ~ "50%",
               G15 == 3 ~ "Rất ít",
               G15 == 4 ~ "Không ra quyết định",
               G15 == 8 ~ "Không biết"
           )) %>%
    mutate(G15_LABEL = factor(G15_LABEL,
                              levels = c("Toàn bộ/Hầu hết", "50%", "Rất ít", "Không ra quyết định", "Không biết"),
                              ordered = TRUE))

G15 <- count_response(fisf, G15_LABEL, index = "G15")

## Export ------------------------------

openxlsx::write.xlsx(list(G1_2 = G1_2,
                          G3_4_5_6_7 = G3_4_5_6_7,
                          G8_1 = G8_1,
                          G8_2 = G8_2,
                          G9_1 = G9_1,
                          G9_2 = G9_2,
                          G10 = G10,
                          G11_12 = G11_12,
                          G13 = G13,
                          G14 = G14,
                          G15 = G15),
                     file = "../outputs/SECTION-G.xlsx")
