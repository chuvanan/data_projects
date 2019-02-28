

## -----------------------------------------------------------------------------
## Load packages and import data

## https://towardsdatascience.com/predict-customer-churn-with-r-9e62357d47b4

library(dplyr)
library(purrr)
churn <- readr::read_csv("../data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

dim(churn)
head(churn)
tail(churn)
names(churn)
sapply(churn, class)

## -----------------------------------------------------------------------------
## Helper functions

count_na <- function(x) {
    sum(is.na(x)) * 100 / length(x)
}

## -----------------------------------------------------------------------------
## Initial Exploration

map_dbl(churn, count_na) %>%
    keep( ~ .x > 0)

## any duplicated customer id?
anyDuplicated(churn$customerID)

## male or female?
churn %>% count(gender)
churn$gender <- as.factor(churn$gender)

## whether the customer is a senior citizen or not?
churn %>% count(SeniorCitizen)
churn$SeniorCitizen <- ifelse(churn$SeniorCitizen == 1, "Yes", "No")
churn$SeniorCitizen <- as.factor(churn$SeniorCitizen)

## whether the customer has a partner or not?
churn %>% count(Partner)
churn$Partner <- as.factor(churn$Partner)

## whether the customer has dependents or not?
churn %>% count(Dependents)
churn$Dependents <- as.factor(churn$Dependents)

## number of months the customer has stayed with the company
churn %>% count(tenure, sort = TRUE)
summary(churn$tenure)

churn <- churn %>%
    mutate(tenure_grp = case_when(
               tenure >= 0 & tenure <= 12 ~ "0-12 months",
               tenure > 12 & tenure <= 24 ~ "12-24 months",
               tenure > 24 & tenure <= 48 ~ "24-48 months",
               tenure > 48 & tenure <= 60 ~ "48-60 months",
               tenure > 60 ~ ">60 months"
           ))
churn %>% count(tenure_grp)
churn$tenure_grp <- as.factor(churn$tenure_grp)

## whether the customer has a phone service or not?
churn %>% count(PhoneService)
churn$PhoneService <- as.factor(churn$PhoneService)

## whether the customer has a multipleline or not?
churn %>% count(MultipleLines)
churn$MultipleLines[churn$MultipleLines == "No phone service"] <- "No"
churn$MultipleLines <- as.factor(churn$MultipleLines)

## whether the customer has internet service or not?
churn %>% count(InternetService)
churn$InternetService[churn$InternetService != "No"] <- "Yes"
churn$InternetService <- as.factor(churn$InternetService)

## whether the customer has online security or not?
churn %>% count(OnlineSecurity)
churn$OnlineSecurity[churn$OnlineSecurity == "No internet service"] <- "No"
churn$OnlineSecurity <- as.factor(churn$OnlineSecurity)

## whether the customer has online backup or not?
churn %>% count(OnlineBackup)
churn$OnlineBackup[churn$OnlineBackup == "No internet service"] <- "No"
churn$OnlineBackup <- as.factor(churn$OnlineBackup)

churn %>% count(DeviceProtection)
churn$DeviceProtection[churn$DeviceProtection == "No internet service"] <- "No"
churn$DeviceProtection <- as.factor(churn$DeviceProtection)

churn %>% count(TechSupport)
churn$TechSupport[churn$TechSupport == "No internet service"] <- "No"
churn$TechSupport <- as.factor(churn$TechSupport)

churn %>% count(StreamingTV)
churn$StreamingTV[churn$StreamingTV == "No internet service"] <- "No"
churn$StreamingTV <- as.factor(churn$StreamingTV)

churn %>% count(StreamingMovies)
churn$StreamingMovies[churn$StreamingMovies == "No internet service"] <- "No"
churn$StreamingMovies <- as.factor(churn$StreamingMovies)

churn %>% count(Contract)
churn$Contract <- as.factor(churn$Contract)

churn %>% count(PaperlessBilling)
churn$PaperlessBilling <- as.factor(churn$PaperlessBilling)


churn %>% count(PaymentMethod)
churn$PaymentMethod <- as.factor(churn$PaymentMethod)

churn %>% count(MonthlyCharges)
churn %>% count(TotalCharges)

churn %>% count(Churn)
churn$Churn <- as.factor(churn$Churn)

## remove columns
churn$customerID <- NULL

cor(churn$MonthlyCharges, churn$TotalCharges, use = "complete") # high correlation
count_na(churn$MonthlyCharges)
count_na(churn$TotalCharges)

churn$TotalCharges <- NULL

## -----------------------------------------------------------------------------
## Export cleaned data

save(churn, file = "../data/churn.RDA")
