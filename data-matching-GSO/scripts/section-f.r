## @ancv

## -----------------------------------------------------------------------------
## F - Bao Hiem

source("02-process.R")

## F1 ------------------------------

fisf <- fisf %>%
    mutate(F1A = fill_missval(F1A),
           F1B = fill_missval(F1B),
           F1C = fill_missval(F1C),
           F1D = fill_missval(F1D))

fisf <- fisf %>%
    mutate(F1A_LABEL = F1A,
           F1B_LABEL = F1B,
           F1C_LABEL = F1C,
           F1D_LABEL = F1D)

fisf <- fisf %>%
    mutate_at(c("F1A_LABEL", "F1B_LABEL", "F1C_LABEL", "F1D_LABEL"),
              function(x) case_when(
                              x == 1 ~ "Có",
                              x == 2 ~ "Không",
                              x == 8 ~ "Không biết"
                          )) %>%
    mutate_at(c("F1A_LABEL", "F1B_LABEL", "F1C_LABEL", "F1D_LABEL"),
              function(x) factor(x, levels = c("Có", "Không", "Không biết"),
                                 ordered = TRUE))


F1A <- count_response(fisf, F1A_LABEL, index = "F1")
F1A <- F1A %>%
    rename(`% người trên 18 tuổi có bảo hiểm sức khỏe tự nguyện` = `Có`,
           `% người trên 18 tuổi không có bảo hiểm sức khỏe tự nguyện` = `Không`) %>%
    select(-`Không biết`)

F1B <- count_response(fisf, F1B_LABEL, index = "F1B")
F1B <- F1B %>%
    rename(`% người trên 18 tuổi có bảo hiểm nhân thọ` = `Có`,
           `% người trên 18 tuổi không có bảo hiểm nhân thọ` = `Không`) %>%
    select(-`Không biết`)

F1C <- count_response(fisf, F1C_LABEL, index = "F1C")
F1C <- F1C %>%
    rename(`% người trên 18 tuổi có bảo hiểm phi nhân thọ` = `Có`,
           `% người trên 18 tuổi không có bảo hiểm phi nhân thọ` = `Không`) %>%
    select(-`Không biết`)

F1D <- count_response(fisf, F1D_LABEL, index = "F1D")
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

## F2 ------------------------------

fisf <- fisf %>%
    mutate(F2A = fill_missval(F2A),
           F2B = fill_missval(F2B),
           F2C = fill_missval(F2C),
           F2D = fill_missval(F2D),
           F2E = fill_missval(F2E),
           F2F = fill_missval(F2F))

fisf <- fisf %>%
    mutate(F2A_LABEL = F2A,
           F2B_LABEL = F2B,
           F2C_LABEL = F2C,
           F2D_LABEL = F2D,
           F2E_LABEL = F2E,
           F2F_LABEL = F2F)

fisf <- fisf %>%
    mutate_at(c("F2A_LABEL", "F2B_LABEL", "F2C_LABEL", "F2D_LABEL",
                "F2E_LABEL", "F2F_LABEL"),
              function(x) case_when(
                              x == 1 ~ "Đồng ý",
                              x == 2 ~ "Không đồng ý",
                              x == 8 ~ "Không biết",
                              )) %>%
    mutate_at(c("F2A_LABEL", "F2B_LABEL", "F2C_LABEL", "F2D_LABEL",
                "F2E_LABEL", "F2F_LABEL"),
              function(x) factor(x, levels = c("Đồng ý", "Không đồng ý", "Không biết"),
                                 ordered = TRUE))

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

## Export ------------------------------

openxlsx::write.xlsx(list(F1 = F1,
                          F2 = F2),
                     file = "../outputs/SECTION-F.xlsx")
