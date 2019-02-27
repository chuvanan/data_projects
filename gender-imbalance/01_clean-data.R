

## load data, be noted that the separator is semi-colon
gender_stats <- read.table("V02.09.csv", sep = ";",
                           skip = 3, nrows = 71, stringsAsFactors = FALSE)


## initial look at data
head(gender_stats)
dim(gender_stats)
names(gender_stats)
sapply(gender_stats, class)

## fix column names
names(gender_stats) <- c("province",
                         paste0("y", c(2005, 2007:2017)))

## convert '..' to NA
gender_stats[] <- lapply(gender_stats,
                         function(x) {x[x == ".."] <- NA; x})


## fix correct column types
num_cols <- names(gender_stats)[grepl("^y", names(gender_stats))]
gender_stats[, num_cols] <- lapply(gender_stats[, num_cols], as.numeric)

## check again
sapply(gender_stats, class)


## new column: region to indicate region name of each province
unique(gender_stats$province)
regions <- c("CẢ NƯỚC", "Đồng bằng sông Hồng", "Trung du và miền núi phía Bắc",
             "Bắc Trung Bộ và duyên hải miền Trung", "Tây Nguyên",
             "Đông Nam Bộ", "Đồng bằng sông Cửu Long")

gender_stats$region <- NA
len <- nrow(gender_stats)

for (i in seq_len(len)) {
    if (gender_stats$province[i] %in% regions) {
        gender_stats$region[i] <- gender_stats$province[i]
    } else {
        gender_stats$region[i] <- gender_stats$region[i - 1]
    }
}
