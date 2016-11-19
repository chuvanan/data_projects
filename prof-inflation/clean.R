


profs <- read.csv("~/ownCloud/data_projects/prof-inflation/raw-profs.csv",
                  stringsAsFactors = FALSE)

table <- function(..., useNA = "ifany") base::table(..., useNA = useNA)

## clean date of birth ---------------------------------------------------------

profs$ngaysinh[profs$ngaysinh == "Chưa rõ"] <- NA
profs$ngaysinh <- gsub(" ", "", profs$ngaysinh)
profs$ngaysinh[profs$ngaysinh == "1977"] <- "01/01/1977"
profs$ngaysinh[profs$ngaysinh == "11/02/191947"] <- "11/02/1947"
profs$ngaysinh[profs$ngaysinh == "10/101945"] <- "10/10/1945"
profs$ngaysinh[profs$ngaysinh == "08/021954"] <- "08/02/1954"
profs$ngaysinh[profs$ngaysinh == "(01/1959)"] <- "01/01/1959"

profs$ngaysinh <- as.Date(profs$ngaysinh, "%d/%m/%Y")

profs$ngaysinh[profs$ngaysinh == as.Date("1905-05-09") & profs$maso_gcn == "2342/PGS"] <- NA
profs$ngaysinh[profs$ngaysinh == as.Date("1981-11-30") & profs$hoten == "Nguyễn Thị Ngọc Quyên"] <- as.Date("1944-11-30")
profs$ngaysinh[profs$ngaysinh == as.Date("2026-10-28") & profs$hoten == "Bùi Phan  Kỳ"] <- as.Date("1926-10-28")
profs$ngaysinh[profs$ngaysinh == as.Date("4364-10-02") & profs$hoten == "Nguyễn Văn Qui"] <- as.Date("1964-10-02")

## clean gender  ---------------------------------------------------------------

profs$gioitinh[profs$gioitinh == ""] <- NA
profs$gioitinh[profs$gioitinh == "Chưa rõ"] <- NA
profs$gioitinh[profs$gioitinh == "NAM"] <- "Nam"
profs$gioitinh[profs$gioitinh == " Nữ"] <- "Nữ"

profs$gioitinh[profs$hoten == "Đỗ Hương Trà" & profs$ngaysinh == as.Date("1957-07-27")] <- "Nữ"
profs$gioitinh[profs$hoten == "Lưu Cẩm Lộc" & profs$ngaysinh == as.Date("1954-01-25")] <- "Nam"
profs$gioitinh[profs$hoten == "Thái Vĩnh Thắng" & profs$ngaysinh == as.Date("1954-10-25")] <- "Nam"

## clean field -----------------------------------------------------------------

profs$nganh[grepl("âm nhạc", profs$nganh, ignore.case = TRUE)] <- "Nghệ thuật"
profs$nganh[grepl("chăn nuôi|thú y|(thủy|thuỷ) sản", profs$nganh, ignore.case = TRUE)] <- "Chăn nuôi-Thú y-Thủy sản"
profs$nganh[grepl("cơ học", profs$nganh, ignore.case = TRUE)] <- "Cơ học"
profs$nganh[grepl("văn học", profs$nganh, ignore.case = TRUE)] <- "Văn học"
## profs$nganh[grepl("xây dựng", profs$nganh, ignore.case = TRUE)] <- "Xây dựng"
profs$nganh[grepl("vật lý", profs$nganh, ignore.case = TRUE)] <- "Vật lý học"
profs$nganh[grepl("y học", profs$nganh, ignore.case = TRUE)] <- "Y học"
profs$nganh[grepl("văn (hóa|hoá)", profs$nganh, ignore.case = TRUE)] <- "Văn hoá học"
profs$nganh[grepl("điện|tự động", profs$nganh, ignore.case = TRUE)] <- "Điện tử-Tự động hóa"
profs$nganh[grepl("(vận tải|giao thông)", profs$nganh, ignore.case = TRUE)] <- "Giao thông vận tải"
profs$nganh[grepl("triết", profs$nganh, ignore.case = TRUE)] <- "Triết học"
profs$nganh[grepl("toán", profs$nganh, ignore.case = TRUE)] <- "Toán học"
profs$nganh[grepl("tin học", profs$nganh, ignore.case = TRUE)] <- "Tin học"
## profs$nganh[grepl("(thủy|thuỷ) sản", profs$nganh, ignore.case = TRUE)] <- "Thủy sản"
profs$nganh[grepl("(thủy|thuỷ) lợi", profs$nganh, ignore.case = TRUE)] <- "Thủy lợi"
## profs$nganh[grepl("thú y", profs$nganh, ignore.case = TRUE)] <- "Thú y"
profs$nganh[grepl("thể dục", profs$nganh, ignore.case = TRUE)] <- "TDTT"
profs$nganh[grepl("tâm (lý|lí)", profs$nganh, ignore.case = TRUE)] <- "Tâm lý học"
profs$nganh[grepl("sử", profs$nganh, ignore.case = TRUE)] <- "Sử học"
profs$nganh[grepl("sinh", profs$nganh, ignore.case = TRUE)] <- "Sinh học"
profs$nganh[grepl("nông|lâm", profs$nganh, ignore.case = TRUE)] <- "Nông-Lâm nghiệp"
## profs$nganh[grepl("mỏ", profs$nganh, ignore.case = TRUE)] <- "Khoa học mỏ"
## profs$nganh[grepl("luyện", profs$nganh, ignore.case = TRUE)] <- "Luyện kim"
profs$nganh[grepl("luật", profs$nganh, ignore.case = TRUE)] <- "Luật học"
## profs$nganh[grepl("lâm", profs$nganh, ignore.case = TRUE)] <- "Lâm nghiệp"
profs$nganh[grepl("nhiệt|luyện", profs$nganh, ignore.case = TRUE)] <- "Kỹ thuật nhiệt-Luyện kim"
profs$nganh[grepl("kinh tế", profs$nganh, ignore.case = TRUE)] <- "Kinh tế học"
profs$nganh[grepl("xây dựng|kiến trúc", profs$nganh, ignore.case = TRUE)] <- "Xây dựng-Kiến trúc"
profs$nganh[grepl("mỏ|trái đất", profs$nganh, ignore.case = TRUE)] <- "KH trái đất-Mỏ"
profs$nganh[grepl("quân sự", profs$nganh, ignore.case = TRUE)] <- "KH quân sự"
profs$nganh[grepl("an ninh", profs$nganh, ignore.case = TRUE)] <- "KH an ninh"
profs$nganh[grepl("khảo cổ", profs$nganh, ignore.case = TRUE)] <- "Khảo cổ học"
## profs$nganh[grepl("^(hoá|hóa) học", profs$nganh, ignore.case = TRUE)] <- "Hóa học"
profs$nganh[grepl("dược", profs$nganh, ignore.case = TRUE)] <- "Dược học"
## profs$nganh[grepl("động lực", profs$nganh, ignore.case = TRUE)] <- "Động lực học"
## profs$nganh[grepl("điện", profs$nganh, ignore.case = TRUE)] <- "Điện tử"
profs$nganh[grepl("dân tộc", profs$nganh, ignore.case = TRUE)] <- "Dân tộc học"
profs$nganh[grepl("^(hoá|hóa) học|thực phẩm", profs$nganh, ignore.case = TRUE)] <- "Hóa học-CNTP"
profs$nganh[grepl("tin học|thông tin", profs$nganh, ignore.case = TRUE)] <- "CNTT"
profs$nganh[grepl("cơ khí|động lực", profs$nganh, ignore.case = TRUE)] <- "Cơ khí-Động lực học"

## clean que quan---------------------------------------------------------------

profs$quequan[grepl("an giang", profs$quequan, ignore.case = TRUE)] <- "An Giang"
profs$quequan[grepl("vũng (tàu|tầu)", profs$quequan, ignore.case = TRUE)] <- "Vũng Tàu"
profs$quequan[grepl("bắc.*giang", profs$quequan, ignore.case = TRUE)] <- "Bắc Giang"
profs$quequan[grepl("bắc (kạn|cạn)", profs$quequan, ignore.case = TRUE)] <- "Bắc Kạn"
profs$quequan[grepl("bạc liêu", profs$quequan, ignore.case = TRUE)] <- "Bạc Liêu"
profs$quequan[grepl("bắc ninh", profs$quequan, ignore.case = TRUE)] <- "Bắc Ninh"
profs$quequan[grepl("bến tre", profs$quequan, ignore.case = TRUE)] <- "Bến Tre"
profs$quequan[grepl("bình định|quy nhơn|hoài nhơn", profs$quequan, ignore.case = TRUE)] <- "Bình Định"
profs$quequan[grepl("quảng ngãi|q.ngãi", profs$quequan, ignore.case = TRUE)] <- "Quảng Ngãi"
profs$quequan[grepl("bình dương", profs$quequan, ignore.case = TRUE)] <- "Bình Dương"
profs$quequan[grepl("bình thuận", profs$quequan, ignore.case = TRUE)] <- "Bình Thuận"
profs$quequan[grepl("cà (mau|mâu)", profs$quequan, ignore.case = TRUE)] <- "Cà Mau"
profs$quequan[grepl("cao bằng", profs$quequan, ignore.case = TRUE)] <- "Cao Bằng"
profs$quequan[grepl("đắk lắk", profs$quequan, ignore.case = TRUE)] <- "Đắk Lắk"
profs$quequan[grepl("đồng nai", profs$quequan, ignore.case = TRUE)] <- "Đồng Nai"
profs$quequan[grepl("(đồng|đông) tháp", profs$quequan, ignore.case = TRUE)] <- "Đồng Tháp"
profs$quequan[grepl("thanh (hóa|hoá)", profs$quequan, ignore.case = TRUE)] <- "Thanh Hóa"
profs$quequan[grepl("hà giang", profs$quequan, ignore.case = TRUE)] <- "Hà Giang"
profs$quequan[grepl("hà nam|nam hà", profs$quequan, ignore.case = TRUE)] <- "Hà Nam"
profs$quequan[grepl("hà.*tĩnh", profs$quequan, ignore.case = TRUE)] <- "Hà Tĩnh"
profs$quequan[grepl("hải (dương|hưng)", profs$quequan, ignore.case = TRUE)] <- "Hải Dương"
profs$quequan[grepl("hậu giang", profs$quequan, ignore.case = TRUE)] <- "Hậu Giang"
profs$quequan[grepl("thái (bình|binh)", profs$quequan, ignore.case = TRUE)] <- "Thái Bình"
profs$quequan[grepl("(hòa|hoà) bình", profs$quequan, ignore.case = TRUE)] <- "Hòa Bình"
profs$quequan[grepl("hưng yên", profs$quequan, ignore.case = TRUE)] <- "Hưng Yên"
profs$quequan[grepl("khánh (hòa|hoà)", profs$quequan, ignore.case = TRUE)] <- "Khánh Hòa"
profs$quequan[grepl("kiên giang", profs$quequan, ignore.case = TRUE)] <- "Kiên Giang"
profs$quequan[grepl("lai châu", profs$quequan, ignore.case = TRUE)] <- "Lai Châu"
profs$quequan[grepl("lâm đồng", profs$quequan, ignore.case = TRUE)] <- "Lâm Đồng"
profs$quequan[grepl("nghệ (an|tĩnh)", profs$quequan, ignore.case = TRUE)] <- "Nghệ An"
profs$quequan[grepl("lạng sơn", profs$quequan, ignore.case = TRUE)] <- "Lạng Sơn"
profs$quequan[grepl("lào cai", profs$quequan, ignore.case = TRUE)] <- "Lào Cai"
profs$quequan[grepl("long an", profs$quequan, ignore.case = TRUE)] <- "Long An"
profs$quequan[grepl("nam.*định", profs$quequan, ignore.case = TRUE)] <- "Nam Định"
profs$quequan[grepl("ninh bình", profs$quequan, ignore.case = TRUE)] <- "Ninh Bình"
profs$quequan[grepl("ninh thuận", profs$quequan, ignore.case = TRUE)] <- "Ninh Thuận"
profs$quequan[grepl("phú thọ", profs$quequan, ignore.case = TRUE)] <- "Phú Thọ"
profs$quequan[grepl("quảng bình|bình trị", profs$quequan, ignore.case = TRUE)] <- "Quảng Bình"
profs$quequan[grepl("quảng.*nam|qảng", profs$quequan, ignore.case = TRUE)] <- "Quảng Nam"
profs$quequan[grepl("(quảng|quang) ninh", profs$quequan, ignore.case = TRUE)] <- "Quảng Ninh"
profs$quequan[grepl("quảng trị", profs$quequan, ignore.case = TRUE)] <- "Quảng Trị"
profs$quequan[grepl("sóc trăng", profs$quequan, ignore.case = TRUE)] <- "Sóc Trăng"
profs$quequan[grepl("sơn la", profs$quequan, ignore.case = TRUE)] <- "Sơn La"
profs$quequan[grepl("tây ninh", profs$quequan, ignore.case = TRUE)] <- "Tây Ninh"
profs$quequan[grepl("thái nguyên", profs$quequan, ignore.case = TRUE)] <- "Thái Nguyên"
profs$quequan[grepl("huế|thừa thiên", profs$quequan, ignore.case = TRUE)] <- "Huế"
profs$quequan[grepl("tiền giang|mỹ tho", profs$quequan, ignore.case = TRUE)] <- "Tiền Giang"
profs$quequan[grepl("trà vinh", profs$quequan, ignore.case = TRUE)] <- "Trà Vinh"
profs$quequan[grepl("tuyên quang", profs$quequan, ignore.case = TRUE)] <- "Tuyên Quang"
profs$quequan[grepl("vĩnh long", profs$quequan, ignore.case = TRUE)] <- "Vĩnh Long"
profs$quequan[grepl("vĩnh (phúc|phú)", profs$quequan, ignore.case = TRUE)] <- "Vĩnh Phúc"
profs$quequan[grepl("yên bái", profs$quequan, ignore.case = TRUE)] <- "Yên Bái"
profs$quequan[grepl("hà tây", profs$quequan, ignore.case = TRUE)] <- "Hà Tây"
profs$quequan[grepl("phú yên", profs$quequan, ignore.case = TRUE)] <- "Phú Yên"
profs$quequan[grepl("cần thơ", profs$quequan, ignore.case = TRUE)] <- "Cần Thơ"
profs$quequan[grepl("đà nẵng", profs$quequan, ignore.case = TRUE)] <- "Đà Nẵng"
profs$quequan[grepl("hải phòng", profs$quequan, ignore.case = TRUE)] <- "Hải Phòng"
profs$quequan[grepl("hà nội|HN", profs$quequan, ignore.case = TRUE)] <- "Hà Nội"
profs$quequan[grepl("(hcm|hồ chío{0,1} minh|sài gòn|gia định)", profs$quequan, ignore.case = TRUE)] <- "TP. Hồ Chí Minh"
profs$quequan[grepl("chưa rõ", profs$quequan, ignore.case = TRUE)] <- NA
profs$quequan[grepl(" ", profs$quequan, ignore.case = TRUE)] <- NA

## clean maso_gcn --------------------------------------------------------------

profs$title <- character(nrow(profs))
profs$title <- ifelse(grepl("/?PGS$", profs$maso_gcn), "PGS",
                      ifelse(grepl("/?GS", profs$maso_gcn), "GS", NA))

## clean name ------------------------------------------------------------------

profs$hoten <- gsub("\\s{2,}", " ", profs$hoten)
profs$hoten <- trimws(profs$hoten, which = "both")

## remove duplicates -----------------------------------------------------------

profs$uni_id <- paste0(profs$hoten, profs$ngaysinh, profs$gioitinh, profs$nam)
profs <- profs[!duplicated(profs$uni_id), ]
profs$uni_id <- NULL
profs <- profs[!(profs$hoten == "Đặng Hùng Thắng" & profs$nam == "1999"), ]

## export csv ------------------------------------------------------------------

write.csv(profs, na = "",
          file = "~/Documents/data_projects/prof-inflation/cleaned-profs.csv",
          row.names = FALSE)
