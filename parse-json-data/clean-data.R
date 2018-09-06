# Clean batdongsan data
# 20/08/2018


# Load packages and data --------------------------------------------------


library(data.table)
library(ggplot2)

dta <- fread("~/Documents/data_projects/parse-json-data/hcm-batdongsan.csv",
             colClasses = "c")

dta[, dim(.SD)]
dta[, head(.SD)]
dta[, names(.SD)]

dta[, submission_date := as.Date(submission_date, "%d-%m-%Y")]

dta[, month := lubridate::month(submission_date)]
dta[, year := lubridate::year(submission_date)]

by_month <- dta[, .N, by = .(year, month)][order(year, month)]
sample_dta <- dta[sample(1:nrow(dta), size = 10000)]


ggplot(by_month, aes(factor(month), N, fill = factor(year))) +
    geom_col(show.legend = FALSE) +
    facet_wrap( ~ factor(year), scales = "free_y") +
    labs(x = NULL) +
    scale_fill_manual(name = NULL, values = c("firebrick", "steelblue"))

## ggsave("~/Documents/data_projects/parse-json-data/by_month.png")
## fwrite(sample_dta, "~/Documents/data_projects/parse-json-data/sample-bds-data.csv")

fwrite(dta[year == 2017 & month == 1], "bds-201701.csv")
fwrite(dta[year == 2017 & month == 2], "bds-201702.csv")
fwrite(dta[year == 2017 & month == 3], "bds-201703.csv")
fwrite(dta[year == 2017 & month == 4], "bds-201704.csv")
fwrite(dta[year == 2017 & month == 5], "bds-201705.csv")
fwrite(dta[year == 2017 & month == 6], "bds-201706.csv")
fwrite(dta[year == 2017 & month == 7], "bds-201707.csv")
fwrite(dta[year == 2017 & month == 8], "bds-201708.csv")
fwrite(dta[year == 2017 & month == 9], "bds-201709.csv")
fwrite(dta[year == 2017 & month == 10], "bds-201710.csv")
fwrite(dta[year == 2017 & month == 11], "bds-201711.csv")
fwrite(dta[year == 2017 & month == 12], "bds-201712.csv")
fwrite(dta[year == 2018 & month == 1], "bds-201801.csv")
fwrite(dta[year == 2018 & month == 2], "bds-201802.csv")
fwrite(dta[year == 2018 & month == 3], "bds-201803.csv")
fwrite(dta[year == 2018 & month == 4], "bds-201804.csv")
fwrite(dta[year == 2018 & month == 5], "bds-201805.csv")
fwrite(dta[year == 2018 & month == 6], "bds-201806.csv")
