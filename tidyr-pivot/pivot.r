
## Play around with new features from tidyr 1.0.0: pivot_longer() and
## pivot_wider()

require(tidyr)
require(ggplot2)

theme_set(theme_minimal(base_size = 16, base_family = "Lato"))

## long-format transformation --------------------------------------------------


data(relig_income)
str(relig_income)

relig_income %>%
    pivot_longer(-religion, names_to = "income", values_to = "count")

data(billboard)
str(billboard)

billboard %>%
    pivot_longer(cols = starts_with("wk"),
                 names_to = "week",
                 values_to = "rank",
                 values_drop_na = TRUE)

billboard %>%
    pivot_longer(cols = starts_with("wk"),
                 names_to = "week",
                 values_to = "rank",
                 names_prefix = "wk",
                 names_ptypes = list(week = integer()),
                 values_drop_na = TRUE)

data(who)
str(who)

who %>%
    pivot_longer(
        cols = new_sp_m014:newrel_f65,
        names_to = c("diagnosis", "gender", "age"),
        names_pattern = "new_?(.*)_(.)(.*)",
        names_ptypes = list(
            gender = factor(levels = c("f", "m")),
            age = factor(
                levels = c("014", "1524", "2534", "3544", "4554", "5564", "65"),
                ordered = TRUE
            )
        ), values_to = "count")

anscombe %>%
    pivot_longer(everything(),
                 names_to = c(".value", "set"),
                 names_pattern = "(.)(.)") %>%
    ggplot(aes(x, y, fill = set)) +
    geom_point(shape = 21, color = "white", size = 3) +
    scale_fill_discrete() +
    facet_wrap(~set)
