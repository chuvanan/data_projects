



library(Information)
options(scipen = 10) # donot show scientific number

## load data
data(train, package = "Information")
data(valid, package = "Information")


## initial exploration
dim(train)
dim(valid)

head(train)
head(valid)

sapply(train, class) # all are numberic variables
sapply(valid, class)

## exclude control group
train <- train[train$TREATMENT == 1, ]
valid <- valid[valid$TREATMENT == 1, ]

## ranking variables using penalized IV

iv_tbl <- create_infotables(train, valid, y = "PURCHASE")

head(iv_tbl$Summary)

names(iv_tbl$Tables)

iv_tbl$Tables$AGE # default is 10 bins
iv_tbl$Tables$AGRGT_BAL_ALL_XCLD_MRTG

## visualize WoE patterns
vars_list <- names(iv_tbl$Tables)
plots <- list()
for (i in 1:length(vars_list)) {
    plots[[i]] <- plot_infotables(iv_tbl, vars_list[i])
}

plots[1]
