## @ancv

## -----------------------------------------------------------------------------
## C - Tai Khoan Va Thanh Toan Qua Tai Khoan

source("02-process.R")

## C1 ------------------------------

## CÓ TÀI KHOẢN TIẾT KIỆM?

fisf <- fisf %>%
    mutate(C1A1_LABEL = onehot_encoding(select(fisf, matches("C1A_[0-9]$")), 1)) %>%
    mutate(C1B1_LABEL = onehot_encoding(select(fisf, matches("C1B_[0-9]$")), 1)) %>%
    mutate(C1C1_LABEL = onehot_encoding(select(fisf, matches("C1C_[0-9]$")), 1)) %>%
    mutate(C1D1_LABEL = onehot_encoding(select(fisf, matches("C1D_[0-9]$")), 1)) %>%
    mutate(C1E1_LABEL = onehot_encoding(select(fisf, matches("C1E$")), 1)) %>%
    mutate(C1F1_LABEL = onehot_encoding(select(fisf, matches("C1F$")), 1))

C1A1 <- count_response(fisf, C1A1_LABEL, index = "C1.1")
C1A1 <- add_rsp(C1A1, c("Có", "Không"))
C1B1 <- count_response(fisf, C1B1_LABEL, index = "C1.1")
C1B1 <- add_rsp(C1B1, c("Có", "Không"))
C1C1 <- count_response(fisf, C1C1_LABEL, index = "C1.1")
C1C1 <- add_rsp(C1C1, c("Có", "Không"))
C1D1 <- count_response(fisf, C1D1_LABEL, index = "C1.1")
C1D1 <- add_rsp(C1D1, c("Có", "Không"))
C1E1 <- count_response(fisf, C1E1_LABEL, index = "C1.1")
C1E1 <- add_rsp(C1E1, c("Có", "Không"))
C1F1 <- count_response(fisf, C1F1_LABEL, index = "C1.1")
C1F1 <- add_rsp(C1F1, c("Có", "Không"))

C1_1 <- bind_cols(
    select(C1A1, STT, `Phân loại ngừơi trả lời`, `Ngân hàng thương mại` = `Có`),
    select(C1B1, `Ngân hàng Chính sách xã hội` = `Có`),
    select(C1C1, `Ngân hàng Hợp tác xã` = `Có`),
    select(C1D1, `Tổ chức cung ứng dịch vụ trung gian thanh toán/Tổ chức công nghệ tài chính - FinTech` = `Có`),
    select(C1E1, `Tổ chức tài chính vi mô` = `Có`),
    select(C1F1, `Quỹ tín dụng nhân dân` = `Có`)
)

## CÓ TÀI KHOẢN GIAO DỊCH/THANH TOÁN?

fisf <- fisf %>%
    mutate(C1A2_LABEL = onehot_encoding(select(fisf, matches("C1A_[0-9]$")), 2)) %>%
    mutate(C1B2_LABEL = onehot_encoding(select(fisf, matches("C1B_[0-9]$")), 2)) %>%
    mutate(C1C2_LABEL = onehot_encoding(select(fisf, matches("C1C_[0-9]$")), 2)) %>%
    mutate(C1D2_LABEL = onehot_encoding(select(fisf, matches("C1D_[0-9]$")), 2))

C1A2 <- count_response(fisf, C1A2_LABEL, index = "C1.2")
C1A2 <- add_rsp(C1A2, c("Có", "Không"))
C1B2 <- count_response(fisf, C1B2_LABEL, index = "C1.2")
C1B2 <- add_rsp(C1B2, c("Có", "Không"))
C1C2 <- count_response(fisf, C1C2_LABEL, index = "C1.2")
C1C2 <- add_rsp(C1C2, c("Có", "Không"))
C1D2 <- count_response(fisf, C1D2_LABEL, index = "C1.2")
C1D2 <- add_rsp(C1D2, c("Có", "Không"))

C1_2 <- bind_cols(
    select(C1A2, STT, `Phân loại ngừơi trả lời`, `Ngân hàng thương mại` = `Có`),
    select(C1B2, `Ngân hàng Chính sách xã hội` = `Có`),
    select(C1C2, `Ngân hàng Hợp tác xã` = `Có`),
    select(C1D2, `Tổ chức cung ứng dịch vụ trung gian thanh toán/Tổ chức công nghệ tài chính - FinTech` = `Có`)
)

## CÓ TÀI KHOẢN TIẾT KIỆM HOẶC TÀI KHOẢN GIAO DỊCH/THANH TOÁN?

fisf <- fisf %>%
    mutate(C1A3_LABEL = onehot_encoding(select(fisf, matches("C1A_[0-9]$")), c(1, 2))) %>%
    mutate(C1B3_LABEL = onehot_encoding(select(fisf, matches("C1B_[0-9]$")), c(1, 2))) %>%
    mutate(C1C3_LABEL = onehot_encoding(select(fisf, matches("C1C_[0-9]$")), c(1, 2))) %>%
    mutate(C1D3_LABEL = onehot_encoding(select(fisf, matches("C1D_[0-9]$")), c(1, 2)))

C1A3 <- count_response(fisf, C1A3_LABEL, index = "C1.3")
C1A3 <- add_rsp(C1A3, c("Có", "Không"))
C1B3 <- count_response(fisf, C1B3_LABEL, index = "C1.3")
C1B3 <- add_rsp(C1B3, c("Có", "Không"))
C1C3 <- count_response(fisf, C1C3_LABEL, index = "C1.3")
C1C3 <- add_rsp(C1C3, c("Có", "Không"))
C1D3 <- count_response(fisf, C1D3_LABEL, index = "C1.3")
C1D3 <- add_rsp(C1D3, c("Có", "Không"))

C1_3 <- bind_cols(
    select(C1A3, STT, `Phân loại ngừơi trả lời`, `Ngân hàng thương mại` = `Có`),
    select(C1B3, `Ngân hàng Chính sách xã hội` = `Có`),
    select(C1C3, `Ngân hàng Hợp tác xã` = `Có`),
    select(C1D3, `Tổ chức cung ứng dịch vụ trung gian thanh toán/Tổ chức công nghệ tài chính - FinTech` = `Có`)
)


## C2 ------------------------------

## TRONG 12 THÁNG QUA, ÔNG/ BÀ CÓ THỰC HIỆN CÁC GIAO DỊCH DƯỚI ĐÂY KHÔNG?

fisf <- local({
    ori_cols <- paste0("C2", LETTERS[1:14])
    new_cols <- paste0(ori_cols, "_LABEL")
    for (i in seq_along(ori_cols)) {
        fisf[[ori_cols[i]]] <- fill_missval(fisf[[ori_cols[i]]])
        fisf[[new_cols[i]]] <- fisf[[ori_cols[i]]]
    }
    fisf
})

fisf <- fisf %>%
    mutate_at(c("C2A_LABEL", "C2B_LABEL", "C2C_LABEL", "C2D_LABEL",
                "C2E_LABEL", "C2F_LABEL", "C2G_LABEL", "C2H_LABEL",
                "C2I_LABEL", "C2J_LABEL", "C2K_LABEL", "C2L_LABEL",
                "C2M_LABEL", "C2N_LABEL"),
              function(x) {
                  factor(case_when(x == 1 ~ "Có", x == 2 ~ "Không"),
                         levels = c("Có", "Không"), ordered = TRUE)
              })

C2A <- count_response(fisf, C2A_LABEL, index = "C2A")
C2A <- add_rsp(C2A, c("Có", "Không"))
C2B <- count_response(fisf, C2B_LABEL, index = "C2B")
C2B <- add_rsp(C2B, c("Có", "Không"))
C2C <- count_response(fisf, C2C_LABEL, index = "C2C")
C2C <- add_rsp(C2C, c("Có", "Không"))
C2D <- count_response(fisf, C2D_LABEL, index = "C2D")
C2D <- add_rsp(C2D, c("Có", "Không"))
C2E <- count_response(fisf, C2E_LABEL, index = "C2E")
C2E <- add_rsp(C2E, c("Có", "Không"))
C2F <- count_response(fisf, C2F_LABEL, index = "C2F")
C2F <- add_rsp(C2F, c("Có", "Không"))
C2G <- count_response(fisf, C2G_LABEL, index = "C2G")
C2G <- add_rsp(C2G, c("Có", "Không"))
C2H <- count_response(fisf, C2H_LABEL, index = "C2H")
C2H <- add_rsp(C2H, c("Có", "Không"))
C2I <- count_response(fisf, C2I_LABEL, index = "C2I")
C2I <- add_rsp(C2I, c("Có", "Không"))
C2J <- count_response(fisf, C2J_LABEL, index = "C2J")
C2J <- add_rsp(C2J, c("Có", "Không"))
C2K <- count_response(fisf, C2K_LABEL, index = "C2K")
C2K <- add_rsp(C2K, c("Có", "Không"))
C2L <- count_response(fisf, C2L_LABEL, index = "C2L")
C2L <- add_rsp(C2L, c("Có", "Không"))
C2M <- count_response(fisf, C2M_LABEL, index = "C2M")
C2M <- add_rsp(C2M, c("Có", "Không"))
C2N <- count_response(fisf, C2N_LABEL, index = "C2N")
C2N <- add_rsp(C2N, c("Có", "Không"))

C2 <- bind_cols(
    select(C2A, STT, `Phân loại ngừơi trả lời`, `Gửi-chuyển tiền vào tài khoản của chính mình` = `Có`, -`Không`),
    select(C2B, -STT, -`Phân loại ngừơi trả lời`, -`Không`, `Rút-chuyển tiền từ tài khoản của chính mình` = `Có`),
    select(C2C, -STT, -`Phân loại ngừơi trả lời`, -`Không`, `Nhận lương hoặc tiền công` = `Có`),
    select(C2D, -STT, -`Phân loại ngừơi trả lời`, -`Không`, `Nhận tiền từ người thân, bạn bè sống tại Việt Nam` = `Có`),
    select(C2E, -STT, -`Phân loại ngừơi trả lời`, -`Không`, `Gửi tiền cho người thân, bạn bè sống tại Việt Nam` = `Có`),
    select(C2F, -STT, -`Phân loại ngừơi trả lời`, -`Không`, `Nhận tiền từ người thân, bạn bè sống tại nước ngoài` = `Có`),
    select(C2G, -STT, -`Phân loại ngừơi trả lời`, -`Không`, `Gửi tiền cho người thân, bạn bè sống tại nước ngoài` = `Có`),
    select(C2H, -STT, -`Phân loại ngừơi trả lời`, -`Không`, `Nhận tiền trợ cấp từ chính phủ` = `Có`),
    select(C2I, -STT, -`Phân loại ngừơi trả lời`, -`Không`, `Thanh toán hóa đơn tiện ích` = `Có`),
    select(C2J, -STT, -`Phân loại ngừơi trả lời`, -`Không`, `Thanh toán học phí` = `Có`),
    select(C2K, -STT, -`Phân loại ngừơi trả lời`, -`Không`, `Thanh toán hóa đơn mua hàng trực tuyến` = `Có`),
    select(C2L, -STT, -`Phân loại ngừơi trả lời`, -`Không`, `Nhận-trả tiền thông qua việc kinh doanh, buôn bán các sản phẩm nông nghiệp` = `Có`),
    select(C2M, -STT, -`Phân loại ngừơi trả lời`, -`Không`, `Nhận/trả tiền thông qua việc kinh doanh/ buôn bán, các sản phẩm, dịch vụ phi nông nghiệp` = `Có`),
    select(C2M, -STT, -`Phân loại ngừơi trả lời`, -`Không`, `Mục đích khác` = `Có`)
)

## C3 ------------------------------

## ÔNG/BÀ ĐÃ THỰC HIỆN NHỮNG GIAO DỊCH NÀY BẰNG TIỀN MẶT HAY THÔNG QUA TÀI KHOẢN
## GIAO DỊCH TẠI CÁC TỔ CHỨC TÀI CHÍNH SAU?

fisf <- local({

    C3_LABEL_COLS <- paste0(
        "C3",
        rep(LETTERS[1:14], each = 3),
        rep(1:3, times = 3),
        "_LABEL"
    )

    C3_REGXS <- paste0("C3", rep(LETTERS[1:14], each = 3), "_[0-9]$")

    C3_RSP_VALS <- rep(1:3, times = length(C3_LABEL_COLS) / 3)

    for (i in seq_along(C3_LABEL_COLS)) {
        new_col <- C3_LABEL_COLS[i]
        regx <- C3_REGXS[i]
        rsp_val <- C3_RSP_VALS[i]
        fisf[[new_col]] <- onehot_encoding(select(fisf, matches(regx)), rsp_val)
    }

    fisf
})

bind_cols_C3 <- function(x1, x2, x3) {
    x1 <- add_rsp(x1, c("Có", "Không"))
    x2 <- add_rsp(x2, c("Có", "Không"))
    x3 <- add_rsp(x3, c("Có", "Không"))
    bind_cols(
        select(x1, STT, `Phân loại ngừơi trả lời`, `Tiền mặt` = `Có`),
        select(x2, `Tài khoản tại ngân hàng` = `Có`),
        select(x3, `Tài khoản tại tổ chức cung ứng dịch vụ thanh toán` = `Có`)
    )
}

C3A1 <- count_response(fisf, C3A1_LABEL, index = "C3A")
C3A2 <- count_response(fisf, C3A2_LABEL, index = "C3A")
C3A3 <- count_response(fisf, C3A3_LABEL, index = "C3A")
C3A <- bind_cols_C3(C3A1, C3A2, C3A3)

C3B1 <- count_response(fisf, C3B1_LABEL, index = "C3B")
C3B2 <- count_response(fisf, C3B2_LABEL, index = "C3B")
C3B3 <- count_response(fisf, C3B3_LABEL, index = "C3B")
C3B <- bind_cols_C3(C3B1, C3B2, C3B3)

C3C1 <- count_response(fisf, C3C1_LABEL, index = "C3C")
C3C2 <- count_response(fisf, C3C2_LABEL, index = "C3C")
C3C3 <- count_response(fisf, C3C3_LABEL, index = "C3C")
C3C <- bind_cols_C3(C3C1, C3C2, C3C3)

C3D1 <- count_response(fisf, C3D1_LABEL, index = "C3D")
C3D2 <- count_response(fisf, C3D2_LABEL, index = "C3D")
C3D3 <- count_response(fisf, C3D3_LABEL, index = "C3D")
C3D <- bind_cols_C3(C3D1, C3D2, C3D3)

C3E1 <- count_response(fisf, C3E1_LABEL, index = "C3E")
C3E2 <- count_response(fisf, C3E2_LABEL, index = "C3E")
C3E3 <- count_response(fisf, C3E3_LABEL, index = "C3E")
C3E <- bind_cols_C3(C3E1, C3E2, C3E3)

C3F1 <- count_response(fisf, C3F1_LABEL, index = "C3F")
C3F2 <- count_response(fisf, C3F2_LABEL, index = "C3F")
C3F3 <- count_response(fisf, C3F3_LABEL, index = "C3F")
C3F <- bind_cols_C3(C3F1, C3F2, C3F3)

C3G1 <- count_response(fisf, C3G1_LABEL, index = "C3G")
C3G2 <- count_response(fisf, C3G2_LABEL, index = "C3G")
C3G3 <- count_response(fisf, C3G3_LABEL, index = "C3G")
C3G <- bind_cols_C3(C3G1, C3G2, C3G3)

C3H1 <- count_response(fisf, C3H1_LABEL, index = "C3H")
C3H2 <- count_response(fisf, C3H2_LABEL, index = "C3H")
C3H3 <- count_response(fisf, C3H3_LABEL, index = "C3H")
C3H <- bind_cols_C3(C3H1, C3H2, C3H3)

C3I1 <- count_response(fisf, C3I1_LABEL, index = "C3I")
C3I2 <- count_response(fisf, C3I2_LABEL, index = "C3I")
C3I3 <- count_response(fisf, C3I3_LABEL, index = "C3I")
C3I <- bind_cols_C3(C3I1, C3I2, C3I3)

C3J1 <- count_response(fisf, C3J1_LABEL, index = "C3J")
C3J2 <- count_response(fisf, C3J2_LABEL, index = "C3J")
C3J3 <- count_response(fisf, C3J3_LABEL, index = "C3J")
C3J <- bind_cols_C3(C3J1, C3J2, C3J3)

C3K1 <- count_response(fisf, C3K1_LABEL, index = "C3K")
C3K2 <- count_response(fisf, C3K2_LABEL, index = "C3K")
C3K3 <- count_response(fisf, C3K3_LABEL, index = "C3K")
C3K <- bind_cols_C3(C3K1, C3K2, C3K3)

C3L1 <- count_response(fisf, C3L1_LABEL, index = "C3L")
C3L2 <- count_response(fisf, C3L2_LABEL, index = "C3L")
C3L3 <- count_response(fisf, C3L3_LABEL, index = "C3L")
C3L <- bind_cols_C3(C3L1, C3L2, C3L3)

C3M1 <- count_response(fisf, C3M1_LABEL, index = "C3M")
C3M2 <- count_response(fisf, C3M2_LABEL, index = "C3M")
C3M3 <- count_response(fisf, C3M3_LABEL, index = "C3M")
C3M <- bind_cols_C3(C3M1, C3M2, C3M3)

C3N1 <- count_response(fisf, C3N1_LABEL, index = "C3N")
C3N2 <- count_response(fisf, C3N2_LABEL, index = "C3N")
C3N3 <- count_response(fisf, C3N3_LABEL, index = "C3N")
C3N <- bind_cols_C3(C3N1, C3N2, C3N3)

## C4 ------------------------------

## KÊNH CUNG ỨNG?

fisf <- local({

    C4_LABEL_COLS <- paste0(
        "C4",
        rep(LETTERS[1:14], each = 8),
        rep(1:8, times = 8),
        "_LABEL"
    )

    C4_REGXS <- paste0("C4", rep(LETTERS[1:14], each = 8), "_[0-9]$")

    C4_RSP_VALS <- rep(1:8, times = length(C4_LABEL_COLS) / 8)

    for (i in seq_along(C4_LABEL_COLS)) {
        new_col <- C4_LABEL_COLS[i]
        regx <- C4_REGXS[i]
        rsp_val <- C4_RSP_VALS[i]
        fisf[[new_col]] <- onehot_encoding(select(fisf, matches(regx)), rsp_val)
    }

    fisf
})

bind_cols_C4 <- function(x1, x2, x3, x4, x5, x6, x7, x8) {
    x1 <- add_rsp(x1, c("Có", "Không"))
    x2 <- add_rsp(x2, c("Có", "Không"))
    x3 <- add_rsp(x3, c("Có", "Không"))
    x4 <- add_rsp(x4, c("Có", "Không"))
    x5 <- add_rsp(x5, c("Có", "Không"))
    x6 <- add_rsp(x6, c("Có", "Không"))
    x7 <- add_rsp(x7, c("Có", "Không"))
    x8 <- add_rsp(x8, c("Có", "Không"))
    bind_cols(
        select(x1, STT, `Phân loại ngừơi trả lời`, `Chi nhánh-phòng giao dịch của ngân hàng` = `Có`),
        select(x2, `ATM` = `Có`),
        select(x3, `POS-EFTPOS-EDC` = `Có`),
        select(x4, `Ứng dụng ngân hàng trên điện thoại` = `Có`),
        select(x5, `Internet banking website` = `Có`),
        select(x6, `Ví điện tử, thẻ trả trước` = `Có`),
        select(x7, `Đại lý (Trạm xăng, bưu điện)` = `Có`),
        select(x8, `Kênh khác` = `Có`)
    )
}

C4A1 <- count_response(fisf, C4A1_LABEL, index = "C4A_1")
C4A2 <- count_response(fisf, C4A2_LABEL, index = "C4A_2")
C4A3 <- count_response(fisf, C4A3_LABEL, index = "C4A_3")
C4A4 <- count_response(fisf, C4A4_LABEL, index = "C4A_4")
C4A5 <- count_response(fisf, C4A5_LABEL, index = "C4A_5")
C4A6 <- count_response(fisf, C4A6_LABEL, index = "C4A_6")
C4A7 <- count_response(fisf, C4A7_LABEL, index = "C4A_7")
C4A8 <- count_response(fisf, C4A8_LABEL, index = "C4A_8")
C4A <- bind_cols_C4(C4A1, C4A2, C4A3, C4A4, C4A5, C4A6, C4A7, C4A8)

C4B1 <- count_response(fisf, C4B1_LABEL, index = "C4B_1")
C4B2 <- count_response(fisf, C4B2_LABEL, index = "C4B_2")
C4B3 <- count_response(fisf, C4B3_LABEL, index = "C4B_3")
C4B4 <- count_response(fisf, C4B4_LABEL, index = "C4B_4")
C4B5 <- count_response(fisf, C4B5_LABEL, index = "C4B_5")
C4B6 <- count_response(fisf, C4B6_LABEL, index = "C4B_6")
C4B7 <- count_response(fisf, C4B7_LABEL, index = "C4B_7")
C4B8 <- count_response(fisf, C4B8_LABEL, index = "C4B_8")
C4B <- bind_cols_C4(C4B1, C4B2, C4B3, C4B4, C4B5, C4B6, C4B7, C4B8)

C4C1 <- count_response(fisf, C4C1_LABEL, index = "C4C_1")
C4C2 <- count_response(fisf, C4C2_LABEL, index = "C4C_2")
C4C3 <- count_response(fisf, C4C3_LABEL, index = "C4C_3")
C4C4 <- count_response(fisf, C4C4_LABEL, index = "C4C_4")
C4C5 <- count_response(fisf, C4C5_LABEL, index = "C4C_5")
C4C6 <- count_response(fisf, C4C6_LABEL, index = "C4C_6")
C4C7 <- count_response(fisf, C4C7_LABEL, index = "C4C_7")
C4C8 <- count_response(fisf, C4C8_LABEL, index = "C4C_8")
C4C <- bind_cols_C4(C4C1, C4C2, C4C3, C4C4, C4C5, C4C6, C4C7, C4C8)

C4D1 <- count_response(fisf, C4D1_LABEL, index = "C4D_1")
C4D2 <- count_response(fisf, C4D2_LABEL, index = "C4D_2")
C4D3 <- count_response(fisf, C4D3_LABEL, index = "C4D_3")
C4D4 <- count_response(fisf, C4D4_LABEL, index = "C4D_4")
C4D5 <- count_response(fisf, C4D5_LABEL, index = "C4D_5")
C4D6 <- count_response(fisf, C4D6_LABEL, index = "C4D_6")
C4D7 <- count_response(fisf, C4D7_LABEL, index = "C4D_7")
C4D8 <- count_response(fisf, C4D8_LABEL, index = "C4D_8")
C4D <- bind_cols_C4(C4D1, C4D2, C4D3, C4D4, C4D5, C4D6, C4D7, C4D8)

C4E1 <- count_response(fisf, C4E1_LABEL, index = "C4E_1")
C4E2 <- count_response(fisf, C4E2_LABEL, index = "C4E_2")
C4E3 <- count_response(fisf, C4E3_LABEL, index = "C4E_3")
C4E4 <- count_response(fisf, C4E4_LABEL, index = "C4E_4")
C4E5 <- count_response(fisf, C4E5_LABEL, index = "C4E_5")
C4E6 <- count_response(fisf, C4E6_LABEL, index = "C4E_6")
C4E7 <- count_response(fisf, C4E7_LABEL, index = "C4E_7")
C4E8 <- count_response(fisf, C4E8_LABEL, index = "C4E_8")
C4E <- bind_cols_C4(C4E1, C4E2, C4E3, C4E4, C4E5, C4E6, C4E7, C4E8)

C4F1 <- count_response(fisf, C4F1_LABEL, index = "C4F_1")
C4F2 <- count_response(fisf, C4F2_LABEL, index = "C4F_2")
C4F3 <- count_response(fisf, C4F3_LABEL, index = "C4F_3")
C4F4 <- count_response(fisf, C4F4_LABEL, index = "C4F_4")
C4F5 <- count_response(fisf, C4F5_LABEL, index = "C4F_5")
C4F6 <- count_response(fisf, C4F6_LABEL, index = "C4F_6")
C4F7 <- count_response(fisf, C4F7_LABEL, index = "C4F_7")
C4F8 <- count_response(fisf, C4F8_LABEL, index = "C4F_8")
C4F <- bind_cols_C4(C4F1, C4F2, C4F3, C4F4, C4F5, C4F6, C4F7, C4F8)

C4G1 <- count_response(fisf, C4G1_LABEL, index = "C4G_1")
C4G2 <- count_response(fisf, C4G2_LABEL, index = "C4G_2")
C4G3 <- count_response(fisf, C4G3_LABEL, index = "C4G_3")
C4G4 <- count_response(fisf, C4G4_LABEL, index = "C4G_4")
C4G5 <- count_response(fisf, C4G5_LABEL, index = "C4G_5")
C4G6 <- count_response(fisf, C4G6_LABEL, index = "C4G_6")
C4G7 <- count_response(fisf, C4G7_LABEL, index = "C4G_7")
C4G8 <- count_response(fisf, C4G8_LABEL, index = "C4G_8")
C4G <- bind_cols_C4(C4G1, C4G2, C4G3, C4G4, C4G5, C4G6, C4G7, C4G8)


C4H1 <- count_response(fisf, C4H1_LABEL, index = "C4H_1")
C4H2 <- count_response(fisf, C4H2_LABEL, index = "C4H_2")
C4H3 <- count_response(fisf, C4H3_LABEL, index = "C4H_3")
C4H4 <- count_response(fisf, C4H4_LABEL, index = "C4H_4")
C4H5 <- count_response(fisf, C4H5_LABEL, index = "C4H_5")
C4H6 <- count_response(fisf, C4H6_LABEL, index = "C4H_6")
C4H7 <- count_response(fisf, C4H7_LABEL, index = "C4H_7")
C4H8 <- count_response(fisf, C4H8_LABEL, index = "C4H_8")
C4H <- bind_cols_C4(C4H1, C4H2, C4H3, C4H4, C4H5, C4H6, C4H7, C4H8)

C4I1 <- count_response(fisf, C4I1_LABEL, index = "C4I_1")
C4I2 <- count_response(fisf, C4I2_LABEL, index = "C4I_2")
C4I3 <- count_response(fisf, C4I3_LABEL, index = "C4I_3")
C4I4 <- count_response(fisf, C4I4_LABEL, index = "C4I_4")
C4I5 <- count_response(fisf, C4I5_LABEL, index = "C4I_5")
C4I6 <- count_response(fisf, C4I6_LABEL, index = "C4I_6")
C4I7 <- count_response(fisf, C4I7_LABEL, index = "C4I_7")
C4I8 <- count_response(fisf, C4I8_LABEL, index = "C4I_8")
C4I <- bind_cols_C4(C4I1, C4I2, C4I3, C4I4, C4I5, C4I6, C4I7, C4I8)

C4J1 <- count_response(fisf, C4J1_LABEL, index = "C4J_1")
C4J2 <- count_response(fisf, C4J2_LABEL, index = "C4J_2")
C4J3 <- count_response(fisf, C4J3_LABEL, index = "C4J_3")
C4J4 <- count_response(fisf, C4J4_LABEL, index = "C4J_4")
C4J5 <- count_response(fisf, C4J5_LABEL, index = "C4J_5")
C4J6 <- count_response(fisf, C4J6_LABEL, index = "C4J_6")
C4J7 <- count_response(fisf, C4J7_LABEL, index = "C4J_7")
C4J8 <- count_response(fisf, C4J8_LABEL, index = "C4J_8")
C4J <- bind_cols_C4(C4J1, C4J2, C4J3, C4J4, C4J5, C4J6, C4J7, C4J8)

C4K1 <- count_response(fisf, C4K1_LABEL, index = "C4K_1")
C4K2 <- count_response(fisf, C4K2_LABEL, index = "C4K_2")
C4K3 <- count_response(fisf, C4K3_LABEL, index = "C4K_3")
C4K4 <- count_response(fisf, C4K4_LABEL, index = "C4K_4")
C4K5 <- count_response(fisf, C4K5_LABEL, index = "C4K_5")
C4K6 <- count_response(fisf, C4K6_LABEL, index = "C4K_6")
C4K7 <- count_response(fisf, C4K7_LABEL, index = "C4K_7")
C4K8 <- count_response(fisf, C4K8_LABEL, index = "C4K_8")
C4K <- bind_cols_C4(C4K1, C4K2, C4K3, C4K4, C4K5, C4K6, C4K7, C4K8)

C4L1 <- count_response(fisf, C4L1_LABEL, index = "C4L_1")
C4L2 <- count_response(fisf, C4L2_LABEL, index = "C4L_2")
C4L3 <- count_response(fisf, C4L3_LABEL, index = "C4L_3")
C4L4 <- count_response(fisf, C4L4_LABEL, index = "C4L_4")
C4L5 <- count_response(fisf, C4L5_LABEL, index = "C4L_5")
C4L6 <- count_response(fisf, C4L6_LABEL, index = "C4L_6")
C4L7 <- count_response(fisf, C4L7_LABEL, index = "C4L_7")
C4L8 <- count_response(fisf, C4L8_LABEL, index = "C4L_8")
C4L <- bind_cols_C4(C4L1, C4L2, C4L3, C4L4, C4L5, C4L6, C4L7, C4L8)

C4M1 <- count_response(fisf, C4M1_LABEL, index = "C4M_1")
C4M2 <- count_response(fisf, C4M2_LABEL, index = "C4M_2")
C4M3 <- count_response(fisf, C4M3_LABEL, index = "C4M_3")
C4M4 <- count_response(fisf, C4M4_LABEL, index = "C4M_4")
C4M5 <- count_response(fisf, C4M5_LABEL, index = "C4M_5")
C4M6 <- count_response(fisf, C4M6_LABEL, index = "C4M_6")
C4M7 <- count_response(fisf, C4M7_LABEL, index = "C4M_7")
C4M8 <- count_response(fisf, C4M8_LABEL, index = "C4M_8")
C4M <- bind_cols_C4(C4M1, C4M2, C4M3, C4M4, C4M5, C4M6, C4M7, C4M8)

C4N1 <- count_response(fisf, C4N1_LABEL, index = "C4N_1")
C4N2 <- count_response(fisf, C4N2_LABEL, index = "C4N_2")
C4N3 <- count_response(fisf, C4N3_LABEL, index = "C4N_3")
C4N4 <- count_response(fisf, C4N4_LABEL, index = "C4N_4")
C4N5 <- count_response(fisf, C4N5_LABEL, index = "C4N_5")
C4N6 <- count_response(fisf, C4N6_LABEL, index = "C4N_6")
C4N7 <- count_response(fisf, C4N7_LABEL, index = "C4N_7")
C4N8 <- count_response(fisf, C4N8_LABEL, index = "C4N_8")
C4N <- bind_cols_C4(C4N1, C4N2, C4N3, C4N4, C4N5, C4N6, C4N7, C4N8)

## C5 ------------------------------

## TẦN SUẤT GIAO DỊCH?
fisf <- local({
    ori_cols <- paste0("C5", LETTERS[1:14])
    new_cols <- paste0(ori_cols, "_LABEL")
    for (i in seq_along(ori_cols)) {
        fisf[[ori_cols[i]]] <- fill_missval(fisf[[ori_cols[i]]])
        fisf[[new_cols[i]]] <- fisf[[ori_cols[i]]]
    }
    fisf
})

fisf <- fisf %>%
    mutate_at(c("C5A_LABEL", "C5B_LABEL", "C5C_LABEL", "C5D_LABEL",
                "C5E_LABEL", "C5F_LABEL", "C5G_LABEL", "C5H_LABEL",
                "C5I_LABEL", "C5J_LABEL", "C5K_LABEL", "C5L_LABEL",
                "C5M_LABEL", "C5N_LABEL"),
              function(x) {
                  factor(case_when(x == 1 ~ "Dưới 1 lần/tháng",
                                   x == 2 ~ "1-2 lần/tháng",
                                   x == 3 ~ "Trên 2 lần/tháng"),
                         levels = c("Dưới 1 lần/tháng",
                                    "1-2 lần/tháng",
                                    "Trên 2 lần/tháng"),
                         ordered = TRUE)
              })

C5A <- count_response(fisf, C5A_LABEL, index = "C5A")
C5B <- count_response(fisf, C5B_LABEL, index = "C5B")
C5C <- count_response(fisf, C5C_LABEL, index = "C5C")
C5D <- count_response(fisf, C5D_LABEL, index = "C5D")
C5E <- count_response(fisf, C5E_LABEL, index = "C5E")
C5F <- count_response(fisf, C5F_LABEL, index = "C5F")
C5G <- count_response(fisf, C5G_LABEL, index = "C5G")
C5H <- count_response(fisf, C5H_LABEL, index = "C5H")
C5I <- count_response(fisf, C5I_LABEL, index = "C5I")
C5J <- count_response(fisf, C5J_LABEL, index = "C5J")
C5K <- count_response(fisf, C5K_LABEL, index = "C5K")
C5L <- count_response(fisf, C5L_LABEL, index = "C5L")
C5M <- count_response(fisf, C5M_LABEL, index = "C5M")
C5N <- count_response(fisf, C5N_LABEL, index = "C5N")


## C2-3-4-5 ------------------------------

## GỬI/CHUYỂN TIỀN VÀO TÀI KHOẢN CỦA CHÍNH MÌNH
C3_4_5_A <- bind_cols(
    adjust_colnames(C3A, "Thông qua các hình thức"),
    select(adjust_colnames(C4A, "Thông qua các kênh cung ứng"), -STT, -`Phân loại ngừơi trả lời`),
    select(adjust_colnames(C5A, "Tần suất giao dịch"), -STT, -`Phân loại ngừơi trả lời`)
)

## RÚT RA/CHUYỂN TIỀN TỪ TÀI KHOẢN CỦA CHÍNH MÌNH
C3_4_5_B <- bind_cols(
    adjust_colnames(C3B, "Thông qua các hình thức"),
    select(adjust_colnames(C4B, "Thông qua các kênh cung ứng"), -STT, -`Phân loại ngừơi trả lời`),
    select(adjust_colnames(C5B, "Tần suất giao dịch"), -STT, -`Phân loại ngừơi trả lời`)
)

## NHẬN LƯƠNG HOẶC TIỀN CÔNG (BAO GỒM CẢ LƯƠNG HƯU)
C3_4_5_C <- bind_cols(
    adjust_colnames(C3C, "Thông qua các hình thức"),
    select(adjust_colnames(C4C, "Thông qua các kênh cung ứng"), -STT, -`Phân loại ngừơi trả lời`),
    select(adjust_colnames(C5C, "Tần suất giao dịch"), -STT, -`Phân loại ngừơi trả lời`)
)

## NHẬN TIỀN TỪ NGƯỜI THÂN/BẠN BÈ SỐNG TẠI VIỆT NAM
C3_4_5_D <- bind_cols(
    adjust_colnames(C3D, "Thông qua các hình thức"),
    select(adjust_colnames(C4D, "Thông qua các kênh cung ứng"), -STT, -`Phân loại ngừơi trả lời`),
    select(adjust_colnames(C5D, "Tần suất giao dịch"), -STT, -`Phân loại ngừơi trả lời`)
)

## GỬI TIỀN CHO NGƯỜI THÂN/BẠN BÈ SỐNG TẠI VIỆT NAM
C3_4_5_E <- bind_cols(
    adjust_colnames(C3E, "Thông qua các hình thức"),
    select(adjust_colnames(C4E, "Thông qua các kênh cung ứng"), -STT, -`Phân loại ngừơi trả lời`),
    select(adjust_colnames(C5E, "Tần suất giao dịch"), -STT, -`Phân loại ngừơi trả lời`)
)

## NHẬN TIỀN TỪ NGƯỜI THÂN/BẠN BÈ SỐNG TẠI NƯỚC NGOÀI
C3_4_5_F <- bind_cols(
    adjust_colnames(C3F, "Thông qua các hình thức"),
    select(adjust_colnames(C4F, "Thông qua các kênh cung ứng"), -STT, -`Phân loại ngừơi trả lời`),
    select(adjust_colnames(C5F, "Tần suất giao dịch"), -STT, -`Phân loại ngừơi trả lời`)
)

## GỬI TIỀN CHO NGƯỜI THÂN/BẠN BÈ SỐNG TẠI NƯỚC NGOÀI
C3_4_5_G <- bind_cols(
    adjust_colnames(C3G, "Thông qua các hình thức"),
    select(adjust_colnames(C4G, "Thông qua các kênh cung ứng"), -STT, -`Phân loại ngừơi trả lời`),
    select(adjust_colnames(C5G, "Tần suất giao dịch"), -STT, -`Phân loại ngừơi trả lời`)
)

## NHẬN TIỀN TRỢ CẤP CỦA CHÍNH PHỦ
C3_4_5_H <- bind_cols(
    adjust_colnames(C3H, "Thông qua các hình thức"),
    select(adjust_colnames(C4H, "Thông qua các kênh cung ứng"), -STT, -`Phân loại ngừơi trả lời`),
    select(adjust_colnames(C5H, "Tần suất giao dịch"), -STT, -`Phân loại ngừơi trả lời`)
)


## THINH TOÁN HÓI ĐƠN TIỆN ÍCH
C3_4_5_I <- bind_cols(
    adjust_colnames(C3I, "Thông qua các hình thức"),
    select(adjust_colnames(C4I, "Thông qua các kênh cung ứng"), -STT, -`Phân loại ngừơi trả lời`),
    select(adjust_colnames(C5I, "Tần suất giao dịch"), -STT, -`Phân loại ngừơi trả lời`)
)

## THJNH TOÁN HỌC PHÍ
C3_4_5_J <- bind_cols(
    adjust_colnames(C3J, "Thông qua các hình thức"),
    select(adjust_colnames(C4J, "Thông qua các kênh cung ứng"), -STT, -`Phân loại ngừơi trả lời`),
    select(adjust_colnames(C5J, "Tần suất giao dịch"), -STT, -`Phân loại ngừơi trả lời`)
)

## THKNH TOÁN HÓA ĐƠN MUK HÀNG TRỰC TUYẾN
C3_4_5_K <- bind_cols(
    adjust_colnames(C3K, "Thông qua các hình thức"),
    select(adjust_colnames(C4K, "Thông qua các kênh cung ứng"), -STT, -`Phân loại ngừơi trả lời`),
    select(adjust_colnames(C5K, "Tần suất giao dịch"), -STT, -`Phân loại ngừơi trả lời`)
)

## NHẬN/TRẢ TIỀN THÔNG QUA VIỆC MUA/ BÁN CÁC SẢN PHẨM NÔNG NGHIỆP
C3_4_5_L <- bind_cols(
    adjust_colnames(C3L, "Thông qua các hình thức"),
    select(adjust_colnames(C4L, "Thông qua các kênh cung ứng"), -STT, -`Phân loại ngừơi trả lời`),
    select(adjust_colnames(C5L, "Tần suất giao dịch"), -STT, -`Phân loại ngừơi trả lời`)
)

## NHẬN/TRẢ TIỀN THÔNG QUA VIỆC MUA/ BÁN CÁC SẢN PHẨM/DỊCH VỤ PHI NÔNG NGHIỆP
C3_4_5_M <- bind_cols(
    adjust_colnames(C3M, "Thông qua các hình thức"),
    select(adjust_colnames(C4M, "Thông qua các kênh cung ứng"), -STT, -`Phân loại ngừơi trả lời`),
    select(adjust_colnames(C5M, "Tần suất giao dịch"), -STT, -`Phân loại ngừơi trả lời`)
)

## C6 ------------------------------

fisf <- fisf %>%
    mutate(C6A = fill_missval(C6A),
           C6B = fill_missval(C6B))

fisf <- fisf %>%
    mutate(C6A_LABEL = C6A,
           C6B_LABEL = C6B)

fisf <- fisf %>%
    mutate_at(c("C6A_LABEL", "C6B_LABEL"),
              function(x) case_when(x == 1 ~ "Có",
                                    x == 2 ~ "Không",
                                    x == 7 ~ "Không phù hợp")) %>%
    mutate_at(c("C6A_LABEL", "C6B_LABEL"),
              function(x) factor(x, levels = c("Có", "Không", "Không phù hợp"),
                                 ordered = TRUE))

C6A <- count_response(fisf, C6A_LABEL, index = "C61")
C6A <- add_rsp(C6A, c("Có", "Không", "Không phù hợp"))
C6B <- count_response(fisf, C6B_LABEL, index = "C6B")
C6B <- add_rsp(C6B, c("Có", "Không", "Không phù hợp"))

## C7 ------------------------------

## RÀO CẢN TÍN DỤNG SỬ DỤNG SẢN PHẨM DỊCH VỤ NGÂN HÀNG

fisf <- fisf %>%
    mutate(C7A_1_LABEL = onehot_encoding(select(fisf, matches("C7A_[0-9]$")), 1)) %>%
    mutate(C7A_2_LABEL = onehot_encoding(select(fisf, matches("C7A_[0-9]$")), 2)) %>%
    mutate(C7A_3_LABEL = onehot_encoding(select(fisf, matches("C7A_[0-9]$")), 3)) %>%
    mutate(C7A_4_LABEL = onehot_encoding(select(fisf, matches("C7A_[0-9]$")), 4)) %>%
    mutate(C7A_5_LABEL = onehot_encoding(select(fisf, matches("C7A_[0-9]$")), 5)) %>%
    mutate(C7A_6_LABEL = onehot_encoding(select(fisf, matches("C7A_[0-9]$")), 6)) %>%
    mutate(C7A_7_LABEL = onehot_encoding(select(fisf, matches("C7A_[0-9]$")), 7)) %>%
    mutate(C7A_8_LABEL = onehot_encoding(select(fisf, matches("C7A_[0-9]$")), 8)) %>%
    mutate(C7A_9_LABEL = onehot_encoding(select(fisf, matches("C7A_[0-9]$")), 9)) %>%
    mutate(C7A_10_LABEL = onehot_encoding(select(fisf, matches("C7A_[0-9]$")), 10)) %>%
    mutate(C7A_11_LABEL = onehot_encoding(select(fisf, matches("C7A_[0-9]$")), 11))

C7A_1 <- count_response(filter(fisf, C6A != 1), C7A_1_LABEL, index = "C7A_1")
C7A_1 <- add_rsp(C7A_1, c("Có", "Không"))
C7A_2 <- count_response(filter(fisf, C6A != 1), C7A_2_LABEL, index = "C7A_2")
C7A_2 <- add_rsp(C7A_2, c("Có", "Không"))
C7A_3 <- count_response(filter(fisf, C6A != 1), C7A_3_LABEL, index = "C7A_3")
C7A_3 <- add_rsp(C7A_3, c("Có", "Không"))
C7A_4 <- count_response(filter(fisf, C6A != 1), C7A_4_LABEL, index = "C7A_4")
C7A_4 <- add_rsp(C7A_4, c("Có", "Không"))
C7A_5 <- count_response(filter(fisf, C6A != 1), C7A_5_LABEL, index = "C7A_5")
C7A_5 <- add_rsp(C7A_5, c("Có", "Không"))
C7A_6 <- count_response(filter(fisf, C6A != 1), C7A_6_LABEL, index = "C7A_6")
C7A_6 <- add_rsp(C7A_6, c("Có", "Không"))
C7A_7 <- count_response(filter(fisf, C6A != 1), C7A_7_LABEL, index = "C7A_7")
C7A_7 <- add_rsp(C7A_7, c("Có", "Không"))
C7A_8 <- count_response(filter(fisf, C6A != 1), C7A_8_LABEL, index = "C7A_8")
C7A_8 <- add_rsp(C7A_8, c("Có", "Không"))
C7A_9 <- count_response(filter(fisf, C6A != 1), C7A_9_LABEL, index = "C7A_9")
C7A_9 <- add_rsp(C7A_9, c("Có", "Không"))
C7A_10 <- count_response(filter(fisf, C6A != 1), C7A_10_LABEL, index = "C7A_10")
C7A_10 <- add_rsp(C7A_10, c("Có", "Không"))
C7A_11 <- count_response(filter(fisf, C6A != 1), C7A_11_LABEL, index = "C7A_11")
C7A_11 <- add_rsp(C7A_11, c("Có", "Không"))

C7_1 <- bind_cols(
    select(C6A, STT, `Phân loại ngừơi trả lời`,
           `Có hài lòng với sản phẩm, dịch vụ ngân hàng` = `Có`,
           `Không hài lòng với sản phẩm, dịch vụ ngân hàng` = `Không`),
    select(C7A_1, -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - TCTC quá xa` = `Có`),
    select(C7A_2, -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Phí dịch vụ quá đắt` = `Có`),
    select(C7A_3, -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Không có giấy tờ cần thiết` = `Có`),
    select(C7A_4, -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Không hiểu rõ về thủ tục` = `Có`),
    select(C7A_5, -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Không biết về sản phẩm, dịch vụ` = `Có`),
    select(C7A_6, -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Không biết cách sử dụng sản phẩm-dịch vụ` = `Có`),
    select(C7A_7, -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Không tin các tổ chức tài chính` = `Có`),
    select(C7A_8, -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Sợ lộ thông tin` = `Có`),
    select(C7A_9, -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Sử dụng của người thân` = `Có`),
    select(C7A_10, -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Không cần thiết` = `Có`),
    select(C7A_11, -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Lý do khác` = `Có`)
)

## RÀO CẢN TÍN DỤNG SỬ DỤNG SẢN PHẨM DỊCH VỤ PHI NGÂN HÀNG

fisf <- fisf %>%
    mutate(C7B_1_LABEL = onehot_encoding(select(fisf, matches("C7B_[0-9]$")), 1)) %>%
    mutate(C7B_2_LABEL = onehot_encoding(select(fisf, matches("C7B_[0-9]$")), 2)) %>%
    mutate(C7B_3_LABEL = onehot_encoding(select(fisf, matches("C7B_[0-9]$")), 3)) %>%
    mutate(C7B_4_LABEL = onehot_encoding(select(fisf, matches("C7B_[0-9]$")), 4)) %>%
    mutate(C7B_5_LABEL = onehot_encoding(select(fisf, matches("C7B_[0-9]$")), 5)) %>%
    mutate(C7B_6_LABEL = onehot_encoding(select(fisf, matches("C7B_[0-9]$")), 6)) %>%
    mutate(C7B_7_LABEL = onehot_encoding(select(fisf, matches("C7B_[0-9]$")), 7)) %>%
    mutate(C7B_8_LABEL = onehot_encoding(select(fisf, matches("C7B_[0-9]$")), 8)) %>%
    mutate(C7B_9_LABEL = onehot_encoding(select(fisf, matches("C7B_[0-9]$")), 9)) %>%
    mutate(C7B_10_LABEL = onehot_encoding(select(fisf, matches("C7B_[0-9]$")), 10)) %>%
    mutate(C7B_11_LABEL = onehot_encoding(select(fisf, matches("C7B_[0-9]$")), 11))

C7B_1 <- count_response(filter(fisf, C6B != 1), C7B_1_LABEL, index = "C7B_1")
C7B_1 <- add_rsp(C7B_1, c("Có", "Không"))
C7B_2 <- count_response(filter(fisf, C6B != 1), C7B_2_LABEL, index = "C7B_2")
C7B_2 <- add_rsp(C7B_2, c("Có", "Không"))
C7B_3 <- count_response(filter(fisf, C6B != 1), C7B_3_LABEL, index = "C7B_3")
C7B_3 <- add_rsp(C7B_3, c("Có", "Không"))
C7B_4 <- count_response(filter(fisf, C6B != 1), C7B_4_LABEL, index = "C7B_4")
C7B_4 <- add_rsp(C7B_4, c("Có", "Không"))
C7B_5 <- count_response(filter(fisf, C6B != 1), C7B_5_LABEL, index = "C7B_5")
C7B_5 <- add_rsp(C7B_5, c("Có", "Không"))
C7B_6 <- count_response(filter(fisf, C6B != 1), C7B_6_LABEL, index = "C7B_6")
C7B_6 <- add_rsp(C7B_6, c("Có", "Không"))
C7B_7 <- count_response(filter(fisf, C6B != 1), C7B_7_LABEL, index = "C7B_7")
C7B_7 <- add_rsp(C7B_7, c("Có", "Không"))
C7B_8 <- count_response(filter(fisf, C6B != 1), C7B_8_LABEL, index = "C7B_8")
C7B_8 <- add_rsp(C7B_8, c("Có", "Không"))
C7B_9 <- count_response(filter(fisf, C6B != 1), C7B_9_LABEL, index = "C7B_9")
C7B_9 <- add_rsp(C7B_9, c("Có", "Không"))
C7B_10 <- count_response(filter(fisf, C6B != 1), C7B_10_LABEL, index = "C7B_10")
C7B_10 <- add_rsp(C7B_10, c("Có", "Không"))
C7B_11 <- count_response(filter(fisf, C6B != 1), C7B_11_LABEL, index = "C7B_11")
C7B_11 <- add_rsp(C7B_11, c("Có", "Không"))

C7_2 <- bind_cols(
    select(add_rsp(C6B, "Có"), STT, `Phân loại ngừơi trả lời`,
           `Có hài lòng với sản phẩm, dịch vụ phi ngân hàng` = `Có`,
           `Không hài lòng với sản phẩm, dịch vụ phi ngân hàng` = `Không`),
    select(add_rsp(C7B_1, "Có"), -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - TCTC quá xa` = `Có`),
    select(add_rsp(C7B_2, "Có"), -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Phí dịch vụ quá đắt` = `Có`),
    select(add_rsp(C7B_3, "Có"), -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Không có giấy tờ cần thiết` = `Có`),
    select(add_rsp(C7B_4, "Có"), -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Không hiểu rõ về thủ tục` = `Có`),
    select(add_rsp(C7B_5, "Có"), -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Không biết về sản phẩm, dịch vụ` = `Có`),
    select(add_rsp(C7B_6, "Có"), -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Không biết cách sử dụng sản phẩm-dịch vụ` = `Có`),
    select(add_rsp(C7B_7, "Có"), -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Không tin các tổ chức tài chính` = `Có`),
    select(add_rsp(C7B_8, "Có"), -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Sợ lộ thông tin` = `Có`),
    select(add_rsp(C7B_9, "Có"), -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Sử dụng của người thân` = `Có`),
    select(add_rsp(C7B_10, "Có"), -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Không cần thiết` = `Có`),
    select(add_rsp(C7B_11, "Có"), -STT, -`Phân loại ngừơi trả lời`, `Lý do không sử dụng hoặc không hài lòng - Lý do khác` = `Có`)
)

## Export ------------------------------

if (STRATIFIED_BY_REGION) {
    filename <- paste0("../outputs/SECTION-C ", WHICH_REGION, ".xlsx")
} else {
    filename <- paste0("../outputs/SECTION-C TOANQUOC.xlsx")
}

openxlsx::write.xlsx(list(C1_1 = C1_1,
                          C1_2 = C1_2,
                          C1_3 = C1_3,
                          C2 = C2,
                          C3_4_5_A = C3_4_5_A,
                          C3_4_5_B = C3_4_5_B,
                          C3_4_5_C = C3_4_5_C,
                          C3_4_5_D = C3_4_5_D,
                          C3_4_5_E = C3_4_5_E,
                          C3_4_5_F = C3_4_5_F,
                          C3_4_5_G = C3_4_5_G,
                          C3_4_5_H = C3_4_5_H,
                          C3_4_5_I = C3_4_5_I,
                          C3_4_5_J = C3_4_5_J,
                          C3_4_5_K = C3_4_5_K,
                          C3_4_5_L = C3_4_5_L,
                          C3_4_5_M = C3_4_5_M,
                          C7_1 = C7_1,
                          C7_2 = C7_2),
                     file = filename)
