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
