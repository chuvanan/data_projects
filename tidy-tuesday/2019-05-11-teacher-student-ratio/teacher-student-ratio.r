## @an.cv


## -----------------------------------------------------------------------------
## Load packages and import data
## -----------------------------------------------------------------------------

conflictRules("dplyr",
              mask.ok = c("filter"),
              exclude = c("setdiff", "setequal", "union", "intersect", "lag"))

require(readr)
require(dplyr)
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
## Analyse data
## -----------------------------------------------------------------------------
