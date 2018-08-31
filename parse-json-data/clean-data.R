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
    geom_col() +
    facet_wrap( ~ factor(year), scales = "free_y") +
    labs(x = NULL) +
    scale_fill_manual(name = NULL, values = c("firebrick", "steelblue"))

## ggsave("~/Documents/data_projects/parse-json-data/by_month.png")
## fwrite(sample_dta, "~/Documents/data_projects/parse-json-data/sample-bds-data.csv")


fwrite(dta[year == 2017 & month == 1], "~/Documents/data_projects/parse-json-data/bds-201701.csv")
fwrite(dta[year == 2017 & month == 2], "~/Documents/data_projects/parse-json-data/bds-201702.csv")
fwrite(dta[year == 2017 & month == 3], "~/Documents/data_projects/parse-json-data/bds-201703.csv")
fwrite(dta[year == 2017 & month == 4], "~/Documents/data_projects/parse-json-data/bds-201704.csv")
fwrite(dta[year == 2017 & month == 5], "~/Documents/data_projects/parse-json-data/bds-201705.csv")
fwrite(dta[year == 2017 & month == 6], "~/Documents/data_projects/parse-json-data/bds-201706.csv")
fwrite(dta[year == 2017 & month == 7], "~/Documents/data_projects/parse-json-data/bds-201706.csv")
fwrite(dta[year == 2017 & month == 8], "~/Documents/data_projects/parse-json-data/bds-201708.csv")
fwrite(dta[year == 2017 & month == 9], "~/Documents/data_projects/parse-json-data/bds-201709.csv")
fwrite(dta[year == 2017 & month == 10], "~/Documents/data_projects/parse-json-data/bds-201710.csv")
fwrite(dta[year == 2017 & month == 11], "~/Documents/data_projects/parse-json-data/bds-201711.csv")
fwrite(dta[year == 2017 & month == 12], "~/Documents/data_projects/parse-json-data/bds-201712.csv")
fwrite(dta[year == 2018 & month == 1], "~/Documents/data_projects/parse-json-data/bds-201801.csv")
fwrite(dta[year == 2018 & month == 2], "~/Documents/data_projects/parse-json-data/bds-201802.csv")
fwrite(dta[year == 2018 & month == 3], "~/Documents/data_projects/parse-json-data/bds-201803.csv")
fwrite(dta[year == 2018 & month == 4], "~/Documents/data_projects/parse-json-data/bds-201804.csv")
fwrite(dta[year == 2018 & month == 5], "~/Documents/data_projects/parse-json-data/bds-201805.csv")
fwrite(dta[year == 2018 & month == 6], "~/Documents/data_projects/parse-json-data/bds-201806.csv")
