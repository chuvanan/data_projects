

library(caret)

load("../data/churn.RDA")
sapply(churn, class)

churn$tenure <- NULL

## -----------------------------------------------------------------------------
## Data  splitting

set.seed(3456)
train_index <- createDataPartition(churn$Churn,
                                   p = 0.8,
                                   list = FALSE,
                                   times = 1)

churn_train <- churn[train_index, ]
churn_test <- churn[-train_index, ]

nrow(churn_train) / nrow(churn)
table(churn$Churn) / nrow(churn)
table(churn_train$Churn) / nrow(churn_train)
table(churn_test$Churn) / nrow(churn_test)

## -----------------------------------------------------------------------------
## Feature selection


glm(Churn ~ ., data = churn, family = binomial(link = "logit"))
