

library(dplyr)

y <- sample(c("G", "B"), size = 100, replace = TRUE)
x <- sample(18:60, size = 100, replace = TRUE)
x_bins <- cut(x, breaks = 5)

dta <- data.frame(y = y, x = x, x_bins)

dta %>%
    group_by(x_bins) %>%
    summarise(n = n(),
              tot_distr = n / nrow(dta),
              n_goods = sum(y == "G"),
              n_bads = sum(y == "B")) %>%
    mutate(distr_goods = n_goods / sum(n_goods),
           distr_bads = n_bads / sum(n_bads),
           bad_rate = n_bads / n) %>%
    mutate(WOE_good_over_bad = log(distr_goods / distr_bads),
           WOE_bad_over_good = log(distr_bads / distr_goods)) %>%
    mutate(IV_good_over_bad = sum(distr_goods - distr_bads) * WOE_good_over_bad,
           IV_bad_over_good = sum(distr_bads - distr_goods) * WOE_bad_over_good)
