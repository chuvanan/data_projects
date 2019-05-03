## @ancv

## -----------------------------------------------------------------------------
## F - Bao Hiem

source("02-process.R")

## F1 ------------------------------
## ÔNG/BÀ HIỆN CÓ TỰ MUA HOẶC LÀ ĐỐI TƯỢNG THỤ HƯỞNG CỦA CÁC LOẠI BẢO HIỂM SAU ĐÂY KHÔNG?

fisf <- process_select_one_question(fisf, F1A, F1B, F1C, F1D,
                                    rsps = c("1" = "Có",
                                             "2" = "Không",
                                             "8" = "Không biết"))

## CÁCH 1: TÍNH TRÊN TOÀN BỘ MẪU

F1A <- count_response(fisf, F1A_LABEL, index = "F1")
F1A <- add_rsp(F1A, c("Có", "Không", "Không biết"))
F1A <- F1A %>%
    rename(`% người trên 18 tuổi có bảo hiểm sức khỏe tự nguyện` = `Có`,
           `% người trên 18 tuổi không có bảo hiểm sức khỏe tự nguyện` = `Không`) %>%
    select(-`Không biết`)

F1B <- count_response(fisf, F1B_LABEL, index = "F1B")
F1B <- add_rsp(F1B, c("Có", "Không", "Không biết"))
F1B <- F1B %>%
    rename(`% người trên 18 tuổi có bảo hiểm nhân thọ` = `Có`,
           `% người trên 18 tuổi không có bảo hiểm nhân thọ` = `Không`) %>%
    select(-`Không biết`)

F1C <- count_response(fisf, F1C_LABEL, index = "F1C")
F1C <- add_rsp(F1C, c("Có", "Không", "Không biết"))
F1C <- F1C %>%
    rename(`% người trên 18 tuổi có bảo hiểm phi nhân thọ` = `Có`,
           `% người trên 18 tuổi không có bảo hiểm phi nhân thọ` = `Không`) %>%
    select(-`Không biết`)

F1D <- count_response(fisf, F1D_LABEL, index = "F1D")
F1D <- add_rsp(F1D, c("Có", "Không", "Không biết"))
F1D <- F1D %>%
    rename(`% người trên 18 tuổi có bảo hiểm nông nghiệp` = `Có`,
           `% người trên 18 tuổi không có bảo hiểm nông nghiệp` = `Không`) %>%
    select(-`Không biết`)

F1 <- bind_cols(
    F1A,
    select(F1B, -STT, -`Phân loại ngừơi trả lời`),
    select(F1C, -STT, -`Phân loại ngừơi trả lời`),
    select(F1D, -STT, -`Phân loại ngừơi trả lời`)
)

F1 <- F1 %>%
    select(STT, `Phân loại ngừơi trả lời`,
           contains("tuổi có"), contains("tuổi không có"))

## CÁCH 2: TÍNH TRÊN MẪU ĐÃ LOẠI NHỮNG NGƯỜI TRẢ LỜI KHÔNG BIẾT

F1A_DROP8 <- count_response(filter(fisf, F1A != 8), F1A_LABEL, index = "F1")
F1A_DROP8 <- add_rsp(F1A_DROP8, c("Có", "Không", "Không biết"))
F1A_DROP8 <- F1A_DROP8 %>%
    rename(`% người trên 18 tuổi có bảo hiểm sức khỏe tự nguyện` = `Có`,
           `% người trên 18 tuổi không có bảo hiểm sức khỏe tự nguyện` = `Không`) %>%
    select(-`Không biết`)

F1B_DROP8 <- count_response(filter(fisf, F1B != 8), F1B_LABEL, index = "F1B_DROP8")
F1B_DROP8 <- add_rsp(F1B_DROP8, c("Có", "Không", "Không biết"))
F1B_DROP8 <- F1B_DROP8 %>%
    rename(`% người trên 18 tuổi có bảo hiểm nhân thọ` = `Có`,
           `% người trên 18 tuổi không có bảo hiểm nhân thọ` = `Không`) %>%
    select(-`Không biết`)

F1C_DROP8 <- count_response(filter(fisf, F1C != 8), F1C_LABEL, index = "F1C")
F1C_DROP8 <- add_rsp(F1C_DROP8, c("Có", "Không", "Không biết"))
F1C_DROP8 <- F1C_DROP8 %>%
    rename(`% người trên 18 tuổi có bảo hiểm phi nhân thọ` = `Có`,
           `% người trên 18 tuổi không có bảo hiểm phi nhân thọ` = `Không`) %>%
    select(-`Không biết`)

F1D_DROP8 <- count_response(filter(fisf, F1D != 8), F1D_LABEL, index = "F1D")
F1D_DROP8 <- add_rsp(F1D_DROP8, c("Có", "Không", "Không biết"))
F1D_DROP8 <- F1D_DROP8 %>%
    rename(`% người trên 18 tuổi có bảo hiểm nông nghiệp` = `Có`,
           `% người trên 18 tuổi không có bảo hiểm nông nghiệp` = `Không`) %>%
    select(-`Không biết`)

F1_DROP8 <- bind_cols(
    F1A_DROP8,
    select(F1B_DROP8, -STT, -`Phân loại ngừơi trả lời`),
    select(F1C_DROP8, -STT, -`Phân loại ngừơi trả lời`),
    select(F1D_DROP8, -STT, -`Phân loại ngừơi trả lời`)
)

F1_DROP8 <- F1_DROP8 %>%
    select(STT, `Phân loại ngừơi trả lời`,
           contains("tuổi có"), contains("tuổi không có"))


## F2 ------------------------------
## ÔNG/BÀ ĐỒNG Ý VỚI NHỮNG KHẲNG ĐỊNH NÀO SAU ĐÂY?

## CÁCH 1: TÍNH TRÊN TOÀN BỘ MẪU

fisf <- process_select_one_question(fisf, F2A, F2B, F2C, F2D, F2E, F2F,
                                    rsps = c("1" = "Đồng ý",
                                             "2" = "Không đồng ý",
                                             "8" = "Không biết"))

fisf <- fisf %>%
    mutate(F2A_LABEL = paste("Bảo hiểm chỉ dành cho người giàu", "-", F2A_LABEL),
           F2B_LABEL = paste("Bảo hiểm chỉ dành cho người có tài sản quý giá cần bảo vệ", "-", F2B_LABEL),
           F2C_LABEL = paste("Bảo hiểm là một phương thức tiết kiệm dài hạn", "-", F2C_LABEL),
           F2D_LABEL = paste("Có bảo hiểm thì không cần phải lo lắng về việc mất đồ hay mất tiền nữa", "-", F2D_LABEL),
           F2E_LABEL = paste("Bảo hiểm rất tốn kém", "-", F2E_LABEL),
           F2F_LABEL = paste("Thiếu những sản phẩm bảo hiểm cho người thu nhập thấp", "-", F2F_LABEL))

F2A <- count_response(fisf, F2A_LABEL, index = "F2")
F2B <- count_response(fisf, F2B_LABEL, index = "F2B")
F2C <- count_response(fisf, F2C_LABEL, index = "F2C")
F2D <- count_response(fisf, F2D_LABEL, index = "F2D")
F2E <- count_response(fisf, F2E_LABEL, index = "F2E")
F2F <- count_response(fisf, F2F_LABEL, index = "F2F")

F2 <- bind_cols(
    F2A,
    select(F2B, -STT, -`Phân loại ngừơi trả lời`),
    select(F2C, -STT, -`Phân loại ngừơi trả lời`),
    select(F2D, -STT, -`Phân loại ngừơi trả lời`),
    select(F2E, -STT, -`Phân loại ngừơi trả lời`),
    select(F2F, -STT, -`Phân loại ngừơi trả lời`)
)

F2 <- select(F2, -contains("Không biết"))

## CÁCH 2: TÍNH TRÊN MẪU ĐÃ LOẠI NHỮNG NGƯỜI TRẢ LỜI KHÔNG BIẾT

F2A_DROP8 <- count_response(filter(fisf, F2A != 8), F2A_LABEL, index = "F2")
F2B_DROP8 <- count_response(filter(fisf, F2B != 8), F2B_LABEL, index = "F2B")
F2C_DROP8 <- count_response(filter(fisf, F2C != 8), F2C_LABEL, index = "F2C")
F2D_DROP8 <- count_response(filter(fisf, F2D != 8), F2D_LABEL, index = "F2D")
F2E_DROP8 <- count_response(filter(fisf, F2E != 8), F2E_LABEL, index = "F2E")
F2F_DROP8 <- count_response(filter(fisf, F2F != 8), F2F_LABEL, index = "F2F")

F2_DROP8 <- bind_cols(
    F2A_DROP8,
    select(F2B_DROP8, -STT, -`Phân loại ngừơi trả lời`),
    select(F2C_DROP8, -STT, -`Phân loại ngừơi trả lời`),
    select(F2D_DROP8, -STT, -`Phân loại ngừơi trả lời`),
    select(F2E_DROP8, -STT, -`Phân loại ngừơi trả lời`),
    select(F2F_DROP8, -STT, -`Phân loại ngừơi trả lời`)
)

F2_DROP8 <- select(F2_DROP8, -contains("Không biết"))


## Export ------------------------------

TABLES <- tibble(
    `Bảng` = c("F1", "F1_DROP8", "F2", "F2_DROP8"),
    `Mô tả` = c("ÔNG/BÀ HIỆN CÓ TỰ MUA HOẶC LÀ ĐỐI TƯỢNG THỤ HƯỞNG CỦA CÁC LOẠI BẢO HIỂM SAU ĐÂY KHÔNG?",
                "ÔNG/BÀ HIỆN CÓ TỰ MUA HOẶC LÀ ĐỐI TƯỢNG THỤ HƯỞNG CỦA CÁC LOẠI BẢO HIỂM SAU ĐÂY KHÔNG? <Loại trường hợp trả lời không biết>",
                "ÔNG/BÀ ĐỒNG Ý VỚI NHỮNG KHẲNG ĐỊNH NÀO SAU ĐÂY VỀ SẢN PHẨM BẢO HIỂM?",
                "ÔNG/BÀ ĐỒNG Ý VỚI NHỮNG KHẲNG ĐỊNH NÀO SAU ĐÂY VỀ SẢN PHẨM BẢO HIỂM? <Loại trường hợp trả lời không biết>")
)

if (STRATIFIED_BY_REGION) {
    filename <- paste0("../outputs/SECTION-F ", WHICH_REGION, ".xlsx")
} else {
    filename <- paste0("../outputs/SECTION-F TOANQUOC.xlsx")
}

openxlsx::write.xlsx(list(F1 = F1,
                          F1_DROP8 = F1_DROP8,
                          F2 = F2,
                          F2_DROP8 = F2_DROP8,
                          TABLES = TABLES),
                     file = filename)
