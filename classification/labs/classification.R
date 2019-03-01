

## -----------------------------------------------------------------------------
## Load data

data(Smarket, package = "ISLR")

dim(Smarket)
names(Smarket)
head(Smarket)
tail(Smarket)
## target: Direction

summary(Smarket)
pairs(Smarket)
cor(Smarket[, -9])
plot(Smarket$Volume)

## -----------------------------------------------------------------------------
## Logistic regression

## Use all predictors
glm_fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                data = Smarket,
                family = binomial)

summary(glm_fits)
coef(glm_fits)

glm_preds <- predict(glm_fits, type = "response")
head(glm_preds)
contrasts(Smarket$Direction)

glm_results <- rep("Down", length(glm_preds))
glm_results[glm_preds > 0.5] <- "Up"

table(glm_results, Smarket$Direction)
table(glm_results, Smarket$Direction) / nrow(Smarket)
sum(diag(table(glm_results, Smarket$Direction))) / nrow(Smarket) # accuracy: 52%

## Use only Lag1 and Lag2

smarket_train <- Smarket[Smarket$Year < 2005, ]
smarket_test <- Smarket[Smarket$Year == 2005, ]

glm_fits <- glm(Direction ~ Lag1 + Lag2,
                data = smarket_train,
                family = binomial)
glm_preds <- predict(glm_fits, newdata = smarket_test, type = "response")
glm_results <- rep("Down", length(glm_preds))
glm_results[glm_preds > 0.5] <- "Up"
sum(diag(table(glm_results, smarket_test$Direction))) / nrow(smarket_test) # accuracy: 56%
