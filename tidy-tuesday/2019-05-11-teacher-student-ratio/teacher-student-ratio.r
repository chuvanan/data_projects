## @an.cv


## -----------------------------------------------------------------------------
## Load packages and import data
## -----------------------------------------------------------------------------

conflictRules("dplyr",
              mask.ok = c("filter"),
              exclude = c("setdiff", "setequal", "union", "intersect", "lag"))

require(readr)
require(dplyr)
require(ggplot2)
student_ratio <- read_csv("student_teacher_ratio.csv")

dim(student_ratio)
head(student_ratio)

## -----------------------------------------------------------------------------
## Explore data
## -----------------------------------------------------------------------------


## how many education levels?
count(student_ratio, edulit_ind)
count(student_ratio, indicator)

## how many countries?
count(student_ratio, country_code)
count(student_ratio, country) %>%
    filter(country == "Viet Nam")

count(student_ratio, year)
count(student_ratio, flag_codes)
count(student_ratio, flags)

## -----------------------------------------------------------------------------
## Helper functions
## -----------------------------------------------------------------------------

bigger_font <- function() {
    theme(text = element_text(size = 17))
}


## -----------------------------------------------------------------------------
## Analyse data
## -----------------------------------------------------------------------------

## overall, has the studio-to-teacher ratio been increasing or decreasing?

student_ratio %>%
    ggplot(aes(factor(year), student_ratio)) +
    geom_boxplot() +
    facet_wrap( ~ indicator, scales = "free_y") +
    bigger_font()

student_ratio %>%
    filter(year == 2017) %>%
    ggplot(aes(indicator, student_ratio)) +
    geom_boxplot() +
    bigger_font() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

student_ratio %>%
    filter(country == "Viet Nam") %>%
    ggplot(aes(year, student_ratio, group = indicator, color = indicator)) +
    geom_line(size = 1.5) +
    labs(title = "Viet Nam") +
    bigger_font()
## abnormal pattern?
