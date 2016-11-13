

library(XML)
library(readxl)

## helper functions ------------------------------------------------------------

table <- function(..., useNA = "ifany") base::table(..., useNA = useNA)

crawler <- function(url, year) {
  out <- readHTMLTable(url, header = TRUE, stringsAsFactors = FALSE)
  out <- out[sapply(out, ncol) == 8]
  out <- do.call("rbind", out)
  names(out) <- c("stt", "hoten", "ngaysinh", "gioitinh", "nganh",
                  "noi_lamviec", "quequan", "maso_gcn")
  out <- out[!grepl("[a-z]", out$stt), ]
  out$nam <- year
  out
}

## links -----------------------------------------------------------------------

url_2001 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-do-n-tru-c-2002"
url_2002 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-doan-2002-2007/2002"
url_2003 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-doan-2002-2007/2003"
url_2004 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-doan-2002-2007/2004"
url_2005 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-doan-2002-2007/2005"
url_2006 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-doan-2002-2007/2006"
url_2007 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-doan-2002-2007/2007"
url_2009 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-doan-2009-2014/2009"
url_2010 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-doan-2009-2014/2010"
url_2011 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-doan-2009-2014/2011"
url_2012 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-doan-2009-2014/2012"
url_2013 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-doan-2009-2014/2013"
url_2014 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-doan-2009-2014/2014"
url_2015 <- "http://www.hdcdgsnn.gov.vn/index.php/danh-sach-cac-gs-pgs-duoc-cong-nhan/giai-doan-2015-2019/2015"


## profs before 2002 -----------------------------------------------------------

prof_2001 <- readHTMLTable(url_2001, header = TRUE, stringsAsFactors = FALSE)
prof_2001 <- prof_2001[sapply(prof_2001, ncol) == 9]
prof_2001 <- do.call("rbind", prof_2001)
names(prof_2001) <- c("stt", "hoten", "ngaysinh", "gioitinh", "nganh",
                      "nam", "noi_lamviec", "quequan", "maso_gcn")

for (i in seq_along(prof_2001$stt)) {
  if (i <= length(prof_2001$stt)) {
    if (grepl("phó giáo sư", prof_2001$stt[i], ignore.case = TRUE)) {
      prof_2001$maso_gcn[i] <- "PGS"
    } else if (grepl("giáo sư", prof_2001$stt[i], ignore.case = TRUE)) {
      prof_2001$maso_gcn[i] <- "GS"
    } else {
      prof_2001$maso_gcn[i] <- prof_2001$maso_gcn[i - 1]
    }
  }
}

prof_2001 <- prof_2001[!grepl("[a-z]", prof_2001$stt), ]
prof_2001 <- prof_2001[, c("stt", "hoten", "ngaysinh", "gioitinh", "nganh",
                           "noi_lamviec", "quequan", "maso_gcn", "nam")]

## after 2001 ------------------------------------------------------------------

prof_2002 <- crawler(url_2002, year = 2002)
prof_2003 <- crawler(url_2003, year = 2003)
prof_2004 <- crawler(url_2004, year = 2004)
prof_2005 <- crawler(url_2005, year = 2005)
prof_2006 <- crawler(url_2006, year = 2006)
prof_2007 <- crawler(url_2007, year = 2007)
prof_2009 <- crawler(url_2009, year = 2009)
prof_2010 <- crawler(url_2010, year = 2010)
prof_2011 <- crawler(url_2011, year = 2011)
prof_2012 <- crawler(url_2012, year = 2012)
prof_2013 <- crawler(url_2013, year = 2013)
prof_2014 <- crawler(url_2014, year = 2014)
prof_2015 <- crawler(url_2015, year = 2015)

## 2016 ------------------------------------------------------------------------


prof_2016a <- read_excel("~/ownCloud/data_projects/prof-inflation/Danh_sach_cac_GS_PGS_cong_nhan_nam_2016_WEB.xls",
                         sheet = 1, skip = 9)
prof_2016b <- read_excel("~/ownCloud/data_projects/prof-inflation/Danh_sach_cac_GS_PGS_cong_nhan_nam_2016_WEB.xls",
                         sheet = 2, skip = 9)
prof_2016 <- rbind(prof_2016a, prof_2016b)
rm(prof_2016a, prof_2016b)

names(prof_2016) <- c("stt", "ho", "ten", "ngaysinh", "gioitinh",
                      "nganh", "noi_lamviec", "quequan", "maso", "gcn")

prof_2016$hoten <- paste(prof_2016$ho, prof_2016$ten)
prof_2016$hoten <- trimws(prof_2016$hoten, which = "both")
prof_2016$maso_gcn <- paste0(prof_2016$maso, prof_2016$gcn)
prof_2016$nam <- 2016
prof_2016 <- prof_2016[, c("stt", "hoten", "ngaysinh", "gioitinh",
                           "nganh", "noi_lamviec", "quequan", "maso_gcn", "nam")]


## combine ---------------------------------------------------------------------

profs <- do.call("rbind",
                 list(prof_2001, prof_2002, prof_2003, prof_2004,
                      prof_2005, prof_2006, prof_2007, prof_2009,
                      prof_2010, prof_2011, prof_2012, prof_2013,
                      prof_2014, prof_2015, prof_2016))
profs$stt <- NULL
rownames(profs) <- NULL

## export raw data
write.csv(profs,
          na = "",
          file = "~/Documents/data_projects/prof-inflation/raw-profs.csv",
          row.names = FALSE)
