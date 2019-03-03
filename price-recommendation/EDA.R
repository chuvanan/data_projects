

## -----------------------------------------------------------------------------
## Load packages and import data

library(data.table)
train <- fread("train.tsv")

count_na <- function(x) {
    sum(is.na(x)) / length(x)
}

## -----------------------------------------------------------------------------
## EDA

dim(train)
## [1] 1482535       8

head(train)
sapply(train, class)

train[, sapply(.SD, count_na)]

anyDuplicated(train$train_id) # id of the listing

train[, length(unique(name))] # title of the listing
## >> 1M uniques listings

train[, length(unique(item_condition_id))] # the conditions of the items provided by the sellers
## 05 unique conditions

train[, .N, by = .(item_condition_id)]
train[, item_condition_id := as.factor(item_condition_id)]

train[brand_name == "", brand_name := NA]
train[, length(unique(brand_name))]
## 4810 unqie brands

train[category_name == "", category_name := NA]
train[, length(unique(category_name))]
train[, .N, by = .(category_name)][order(N, decreasing = TRUE)]

hist(train$price, col = "steelblue", border = "white") # skewed distribution
quantile(train$price, probs = seq(0, 1, 0.01))
## 90% of items has price less than 17$
## 90% of items has price less than 51$
## 99% of items has price less than 170$

hist(train$price[train$price < 200],
     breaks = 30,
     col = "steelblue", border = "white",
     xlab = "price[<200]",
     main = "Price Distribution")

hist(log(train$price), breaks = 30,
     col = "steelblue", border = "white", xlab = "price[log]",
     main = "Price Distributioin")

train$price_log <- log(train$price)

train[, table(shipping) / .N]
## 55% of items shipping fee were paid by the buyers
train$shipping <- as.factor(train$shipping)

train[, mean(price, na.rm = TRUE), by = .(shipping)]
train[, median(price, na.rm = TRUE), by = .(shipping)]

train[, mean(price, na.rm = TRUE), by = .(item_condition_id)]
train[, median(price, na.rm = TRUE), by = .(item_condition_id)]

boxplot(price_log ~ item_condition_id, data = train,
        xlab = "Item Conditions", ylab = "price[log]")
