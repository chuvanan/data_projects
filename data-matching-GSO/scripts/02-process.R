## 2019-03-31
## @ancv

require(dplyr)
require(tidyr)

## -----------------------------------------------------------------------------
## Pre-process dimensions
## -----------------------------------------------------------------------------

fisf <- readxl::read_xlsx("../data/SoLieu_Gui An.xlsx")
source("helper-functions.r")

## Gender
fisf <- fisf %>%
    mutate(A4 = fill_missval(A4))
fisf <- fisf %>%
    mutate(A4_LABEL = case_when(
               A4 == 1 ~ "Nam",
               A4 == 2 ~ "Nữ",
               A4 == 3 ~ "Khác"
           )) %>%
    mutate(A4_LABEL = factor(A4_LABEL,
                             levels = c("Nam", "Nữ", "Khác"),
                             ordered = TRUE))

## Age
fisf <- fisf %>%
    mutate(A3 = fill_missval(A3))
fisf <- fisf %>%
    mutate(AGE = 2019 - A3)

## Age binning
fisf <- fisf %>%
    mutate(AGE_BINS = case_when(
    (AGE >= 18 & AGE <= 24) ~ "Người trẻ tuổi",
    (AGE >= 25 & AGE <= 55 & A4 == 2) ~ "Người trung niên",
    (AGE >= 25 & AGE <= 60 & A4 == 1) ~ "Người trung niên",
    (AGE > 55 & A4 == 2) ~ "Người già",
    (AGE > 60 & A4 == 1) ~ "Người già"
    )) %>%
    mutate(AGE_BINS = factor(AGE_BINS,
                             levels = c("Người trẻ tuổi", "Người trung niên", "Người già"),
                             ordered = TRUE))

## Region
fisf <- fisf %>%
    mutate(TTNT = fill_missval(TTNT))
fisf <- fisf %>%
    mutate(TTNT_LABEL = case_when(
               TTNT == 1 ~ "Thành thị",
               TTNT == 2 ~ "Nông thôn"
           )) %>%
    mutate(TTNT_LABEL = factor(TTNT_LABEL,
                               levels = c("Thành thị", "Nông thôn"),
                               ordered = TRUE))

## Education
fisf <- fisf %>%
    mutate(A6 = fill_missval(A6))
fisf <- fisf %>%
    mutate(A6_LABEL = case_when(
               A6 == 1 ~ "Chưa đi học hoặc tốt nghiệp tiểu học",
               A6 == 2 ~ "Tốt nghiệp tiểu học",
               A6 == 3 ~ "Tốt nghiệp trung học cơ sở",
               A6 == 4 ~ "Tốt nghiệp trung học phổ thông",
               A6 == 5 ~ "Tốt nghiệp trung cấp, cao đẳng hoặc học nghề có chứng chỉ",
               A6 == 6 ~ "Tốt nghiệp đại học trở lên"
           )) %>%
    mutate(A6_LABEL = factor(A6_LABEL,
                             levels = c("Chưa đi học hoặc tốt nghiệp tiểu học",
                                        "Tốt nghiệp tiểu học",
                                        "Tốt nghiệp trung học cơ sở",
                                        "Tốt nghiệp trung học phổ thông",
                                        "Tốt nghiệp trung cấp, cao đẳng hoặc học nghề có chứng chỉ",
                                        "Tốt nghiệp đại học trở lên"),
                             ordered = TRUE))

## Ethnicity
fisf <- fisf %>%
    mutate(A5 = fill_missval(A5))
fisf <- fisf %>%
    mutate(A5_LABEL = case_when(
               A5 == 1 ~ "Kinh",
               TRUE ~ "Khác"
           )) %>%
    mutate(A5_LABEL = factor(A5_LABEL,
                             levels = c("Kinh", "Khác"),
                             ordered = TRUE))

## Job
fisf <- fisf %>%
    mutate(A7MANGHE = fill_missval(A7MANGHE))
fisf <- fisf %>%
    mutate(A7_LVL1 = substr(A7MANGHE, 1L, 1L)) %>%
    mutate(A7_LVL1_LABEL = case_when(
               A7_LVL1 == "0" ~ "Lực lượng quân đội",
               A7_LVL1 == "1" ~ "Các nhà lãnh đạo",
               A7_LVL1 == "2" ~ "Chuyên môn kỹ thuật bậc cao",
               A7_LVL1 == "3" ~ "Nhà chuyên môn bậc trung",
               A7_LVL1 == "4" ~ "Nhân viên trợ lý văn phòng",
               A7_LVL1 == "5" ~ "Nhân viên dịch vụ bán hàng",
               A7_LVL1 == "6" ~ "Lao động có kỹ năng trong nông, lâm nghiệp và thủy sản",
               A7_LVL1 == "7" ~ "Lao động thủ công và các nghề nghiệp có liên quan khác",
               A7_LVL1 == "8" ~ "Thợ lắp ráp và vận hành máy móc thiết bị",
               A7_LVL1 == "9" ~ "Lao động giản đơn",
               (is.na(A7_LVL1) & A7 == 2) ~ "Không làm việc nhưng có thu nhập",
               (is.na(A7_LVL1) & A7 == 3) ~ "Không làm việc nhưng không có thu nhập"
           )) %>%
    mutate(A7_LVL1_LABEL = factor(A7_LVL1_LABEL,
                                  levels = c("Lực lượng quân đội",
                                             "Các nhà lãnh đạo",
                                             "Chuyên môn kỹ thuật bậc cao",
                                             "Nhà chuyên môn bậc trung",
                                             "Nhân viên trợ lý văn phòng",
                                             "Nhân viên dịch vụ bán hàng",
                                             "Lao động có kỹ năng trong nông, lâm nghiệp và thủy sản",
                                             "Lao động thủ công và các nghề nghiệp có liên quan khác",
                                             "Thợ lắp ráp và vận hành máy móc thiết bị",
                                             "Lao động giản đơn",
                                             "Không làm việc nhưng có thu nhập",
                                             "Không làm việc nhưng không có thu nhập"),
                                  ordered = TRUE))

## Median Income
fisf <- fisf %>%
    mutate(A8 = fill_missval(A8))
fisf <- fisf %>%
    mutate(A8_LABEL = case_when(
               A8 == 1 ~ "<=900,000 (Dưới chuẩn nghèo)",
               A8 == 2 ~ "900,001-1,300,001 (Dưới cận nghèo)",
               A8 == 3 ~ "1,300,001-2,000,000 (Mức trung bình)",
               A8 == 4 ~ "2,000,001-10,000,000 (Mức trung bình khá)",
               A8 == 5 ~ ">10,000,000 (Giàu)",
               A8 == 8 ~ "Không biết",
               A8 == 9 ~ "Từ chối trả lời",
               )) %>%
    mutate(A8_LABEL = factor(A8_LABEL,
                             levels = c("<=900,000 (Dưới chuẩn nghèo)",
                                        "900,001-1,300,001 (Dưới cận nghèo)",
                                        "1,300,001-2,000,000 (Mức trung bình)",
                                        "2,000,001-10,000,000 (Mức trung bình khá)",
                                        ">10,000,000 (Giàu)",
                                        "Không biết",
                                        "Từ chối trả lời"),
                             ordered = TRUE))

## Income Per Capita
fisf <- fisf %>%
    mutate(A9 = fill_missval(A9))
fisf <- fisf %>%
    mutate(A9_LABEL = case_when(
               A9 == 1 ~ "<=900,000 (Dưới chuẩn nghèo)",
               A9 == 2 ~ "900,001-1,300,001 (Dưới cận nghèo)",
               A9 == 3 ~ "1,300,001-2,000,000 (Mức trung bình)",
               A9 == 4 ~ "2,000,001-10,000,000 (Mức trung bình khá)",
               A9 == 5 ~ ">10,000,000 (Giàu)",
               A9 == 8 ~ "Không biết",
               A9 == 9 ~ "Từ chối trả lời",
               )) %>%
    mutate(A9_LABEL = factor(A9_LABEL,
                             levels = c("<=900,000 (Dưới chuẩn nghèo)",
                                        "900,001-1,300,001 (Dưới cận nghèo)",
                                        "1,300,001-2,000,000 (Mức trung bình)",
                                        "2,000,001-10,000,000 (Mức trung bình khá)",
                                        ">10,000,000 (Giàu)",
                                        "Không biết",
                                        "Từ chối trả lời"),
                             ordered = TRUE))

## -----------------------------------------------------------------------------
## Whether or not to stratify the data set by `region`
## -----------------------------------------------------------------------------

STRATIFIED_BY_REGION <- FALSE
WHICH_REGION <- "NONGTHON"

if (STRATIFIED_BY_REGION) {
    switch(WHICH_REGION,
           THANHTHI = {
               count_response <- count_response_stratified_by_region
               fisf <- filter(fisf, TTNT == 1)
           },
           NONGTHON = {
               count_response <- count_response_stratified_by_region
               fisf <- filter(fisf, TTNT == 2)
           })
}
