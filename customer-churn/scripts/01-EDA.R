

## https://towardsdatascience.com/predict-customer-churn-with-r-9e62357d47b4

churn <- readr::read_csv("../data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

dim(churn)
head(churn)
tail(churn)
names(churn)
sapply(churn, class)
