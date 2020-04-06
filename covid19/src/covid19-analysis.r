

## -----------------------------------------------------------------------------
##
## -----------------------------------------------------------------------------



conflictRules(pkg = "dplyr", mask.ok = TRUE)
require(here)
require(tidyr)
require(dplyr)
require(ggplot2)
require(forcats)


theme_set(
    theme_minimal(
        base_size = getOption("base_size"), base_family = "Roboto Condensed"
    ) +
    theme(
        panel.grid = element_line(size = 0.3, color = "gray95")
    )
)


confirmed <- readr::read_csv(here("data", "time_series_covid19_confirmed_global.csv"))
dead <- readr::read_csv(here("data", "time_series_covid19_deaths_global.csv"))
recovered <- readr::read_csv(here("data", "time_series_covid19_recovered_global.csv"))


## -----------------------------------------------------------------------------
##
## -----------------------------------------------------------------------------


pivot_cols <- setdiff(names(confirmed), c("Province/State", "Country/Region", "Lat", "Long"))

confirmed <- pivot_longer(confirmed, names_to = "Date", values_to = "Cases", all_of(pivot_cols))
## # A tibble: 6 x 6
## `Province/State` `Country/Region`   Lat  Long Date       Cases
## <chr>            <chr>            <dbl> <dbl> <date>     <dbl>
## 1 NA               Afghanistan         33    65 2020-01-22     0
## 2 NA               Afghanistan         33    65 2020-01-23     0
## 3 NA               Afghanistan         33    65 2020-01-24     0
## 4 NA               Afghanistan         33    65 2020-01-25     0
## 5 NA               Afghanistan         33    65 2020-01-26     0
## 6 NA               Afghanistan         33    65 2020-01-27     0

dead <- pivot_longer(dead, names_to = "Date", values_to = "Cases", all_of(pivot_cols))
recovered <- pivot_longer(recovered, names_to = "Date", values_to = "Cases", all_of(pivot_cols))

confirmed$Date <- lubridate::mdy(confirmed$Date)
dead$Date <- lubridate::mdy(dead$Date)
recovered$Date <- lubridate::mdy(recovered$Date)


confirmed %>%
    group_by(`Country/Region`) %>%
    summarise(TotalCases = max(Cases)) %>%
    filter(TotalCases > 0) %>%
    top_n(30) %>%
    mutate(`Country/Region` = fct_reorder(`Country/Region`, TotalCases)) %>%
    ggplot(aes(`Country/Region`, TotalCases)) +
    geom_col(fill = "orange") +
    geom_text(aes(y = TotalCases + 2e4, label = scales::comma(TotalCases)), color = "gray30", size = 3.5) +
    scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0, 350e3)) +
    coord_flip() +
    labs(x = NULL, title = "Confirmed Cases, Top 30 Countries")
## ggsave(filename = here("figures", "top30.png"))
