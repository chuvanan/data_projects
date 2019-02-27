

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
