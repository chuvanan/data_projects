

library(stringr)

convert_to_day <- function(x) {
  require(stringr)
  
  if (str_detect(x, "[Nn]ăm")) {
    out <- str_extract(x, "[0-9]+")
    out <- as.integer(out) * 365L
  }

  if (str_detect(x, "[Tt]háng")) {
    out <- str_extract(x, "[0-9]+")
    out <- as.integer(out) * 30L
  }

  if (str_detect(x, "[Nn]gày")) {
    out <- str_extract(x, "[0-9]+")
    out <- as.integer(out)
  }
  
  if (x == "") out <- NA
  if (!exists("out")) out <- x
  out
}

tour_guides <- read.csv("~/Documents/data_projects/tour_guides/tour_guides_rawdb.csv",
                        stringsAsFactors = F)

## clean names
full_name <- str_sub(tour_guides$x, 1, str_locate(tour_guides$x, "Số thẻ:")[, 1] - 1)
full_name <- str_replace(full_name, "Họ và tên:", "")
full_name <- str_trim(full_name, "both")
full_name <- str_to_title(full_name)

## clean register number
regis_num <- str_sub(tour_guides$x,
                     str_locate(tour_guides$x, "Số thẻ:")[, 1],
                     str_locate(tour_guides$x, "Ngày hết hạn:")[, 1] - 1)
regis_num <- str_replace(regis_num, "Số thẻ:", "")
regis_num <- str_trim(regis_num, "both")
regis_num <- str_to_title(regis_num)

## clean regis date
regis_date <- str_sub(tour_guides$x,
                      str_locate(tour_guides$x, "Ngày hết hạn:")[, 1],
                      str_locate(tour_guides$x, "Nơi cấp thẻ:")[, 1] - 1)
regis_date <- str_replace(regis_date, "Ngày hết hạn:", "")
regis_date <- str_trim(regis_date, "both")
regis_date <- str_to_title(regis_date)
regis_date <- as.Date(regis_date, "%d/%m/%Y")

## clean registration agency
regis_agency <- str_sub(tour_guides$x,
                        str_locate(tour_guides$x, "Nơi cấp thẻ:")[, 1],
                        str_locate(tour_guides$x, "Loại thẻ:")[, 1] - 1)
regis_agency <- str_replace(regis_agency, "Nơi cấp thẻ:", "")
regis_agency <- str_trim(regis_agency, "both")
regis_agency <- str_to_title(regis_agency)

## clean regis type
regis_type <- str_sub(tour_guides$x,
                      str_locate(tour_guides$x, "Loại thẻ:")[, 1],
                      str_locate(tour_guides$x, "Trạng thái:")[, 1] - 1)
regis_type <- str_replace(regis_type, "Loại thẻ:", "")
regis_type <- str_trim(regis_type, "both")
regis_type <- str_to_title(regis_type)

## clean regis status
regis_status <- str_sub(tour_guides$x,
                        str_locate(tour_guides$x, "Trạng thái:")[, 1],
                        str_locate(tour_guides$x, "Ngoại ngữ:")[, 1] - 1)
regis_status <- str_replace(regis_status, "Trạng thái:", "")
regis_status <- str_trim(regis_status, "both")
regis_status <- str_to_title(regis_status)

## clean regis language
regis_lang <- str_sub(tour_guides$x,
                      str_locate(tour_guides$x, "Ngoại ngữ:")[, 1],
                      str_locate(tour_guides$x, "Kinh nghiệm đến ngày cấp thẻ")[, 1] - 1)
regis_lang <- str_replace(regis_lang, "Ngoại ngữ:", "")
regis_lang <- str_trim(regis_lang, "both")
regis_lang <- str_to_title(regis_lang)

## register experience
regis_expr <- str_sub(tour_guides$x,
                      str_locate(tour_guides$x, "Kinh nghiệm đến ngày cấp thẻ")[, 1],
                      length(tour_guides$x))
regis_expr <- str_replace(regis_expr, "Kinh nghiệm đến ngày cấp thẻ", "")
regis_expr <- str_trim(regis_expr, "both")
regis_expr <- str_to_title(regis_expr)

regis_expr <- str_split(regis_expr, ",")
regis_expr <- lapply(regis_expr, function(x) sapply(x, convert_to_day))
regis_expr <- sapply(regis_expr, sum, na.rm = T)

tourguides <- data.frame(full_name, regis_num, regis_date, regis_agency,
                         regis_status, regis_lang, regis_expr,
                         stringsAsFactors = F)

## write.csv(tourguides, "~/Documents/data_projects/tour_guides/tour_guides_cleaned.csv",
##           row.names = F)
