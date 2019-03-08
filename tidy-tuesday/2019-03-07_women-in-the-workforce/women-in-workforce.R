
library(ggplot2)

## Get the data
jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")
earnings_female <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv")
employed_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv")

## Look at data

dim(jobs_gender)
names(jobs_gender)
sapply(jobs_gender, class)

dim(earnings_female)
head(earnings_female)
sapply(earnings_female, class)

dim(employed_gender)
head(employed_gender)
sapply(employed_gender, class)


## -----------------------------------------------------------------------------
## helper functions

bigger_font <- function() {
    theme(text = element_text(size = 15))
}


## -----------------------------------------------------------------------------
## explore employed_gender

## Are more people working full time or part time over the years?

employment_trend <- ggplot(employed_gender) +
    geom_line(aes(year, total_full_time, color = "fulltime"), size = 1.5) +
    geom_line(aes(year, total_part_time, color = "parttime"), size = 1.5) +
    scale_y_continuous(label = scales::percent_format(scale = 1)) +
    scale_color_manual(name = NULL,
                       values = c(fulltime = "red4", parttime = "blue")) +
    labs(x = NULL, y = "Percentage of employed people",
         title = "Employment Trend in the U.S.A") +
    bigger_font() +
    theme(legend.position = "bottom")

## ggsave(filename = "employment-trend.pdf", employment_trend)
## slight decrease in full-time workforce and a gradual increase of
## part-time workers

## Are there significant difference in employment trend between genders?

fulltime_employment_by_gender <- ggplot(employed_gender) +
    geom_line(aes(year, full_time_male, color = "male"), size = 1.5) +
    geom_line(aes(year, full_time_female, color = "female"), size = 1.5) +
    scale_color_manual(values = c(male = "orange", female = "blue")) +
    labs(x = NULL, y = "Full-time employed (%)",
         title = "After 4 decades, female employment back to its peak") +
    bigger_font()

## ggsave(filename = "fulltime-employment-by-gender.pdf", fulltime_employment_by_gender)

fulltime_employment_8290 <- ggplot(employed_gender) +
    geom_rect(aes(xmin = 1982, xmax = 1990, ymin = 70, ymax = 95),
              fill = "gray90", alpha = 0.3) +
    geom_line(aes(year, full_time_male, color = "male"), size = 1.5) +
    geom_line(aes(year, full_time_female, color = "female"), size = 1.5) +
    scale_color_manual(name = NULL,
                       values = c(male = "orange", female = "blue")) +
    labs(x = NULL, y = "Full-time employed (%)",
         title = "What happened form 1982 to 1990?") +
    theme_minimal() +
    bigger_font() +
    theme(legend.position = "bottom")

## ggsave(filename = "fulltime-employment-8290.pdf", fulltime_employment_8290)
## rate of increase in female employment from 1982 to 1990 is quite phenomenal

## how about part-time employment?

parttime_employment_by_gender <- ggplot(employed_gender) +
    geom_line(aes(year, part_time_male, color = "male"), size = 1.5) +
    geom_line(aes(year, part_time_female, color = "female"), size = 1.5) +
    scale_color_manual(name = NULL,
                       values = c(male = "orange", female = "blue")) +
    labs(x = NULL, y = "Part-time employed (%)",
         title = "More men were taking part-time jobs") +
    theme_minimal() +
    bigger_font() +
    theme(legend.position = "bottom")

## ggsave(filename = "parttime_employment_by_gender.pdf", parttime_employment_by_gender)

## -----------------------------------------------------------------------------
## explore earnings


earnings_female <- earnings_female[earnings_female$group != "Total, 16 years and older", ]

earnings_female_trend <- ggplot(earnings_female,
                                aes(Year, percent,
                                    group = group, color = group)) +
    geom_line(size = 1.5) +
    labs(x = NULL, y = "female salary percent of male salary") +
    scale_color_viridis_d(name = NULL) +
    bigger_font()

## ggsave(filename = "earnings-female-trend.pdf", earnings_female_trend)

## -----------------------------------------------------------------------------
## explore jobs_gender


table(jobs_gender$major_category)
table(jobs_gender$minor_category)
sapply(jobs_gender, class)

jobs_gender$occupation <- factor(jobs_gender$occupation)
jobs_gender$major_category <- factor(jobs_gender$major_category)
jobs_gender$minor_category <- factor(jobs_gender$minor_category)

## Which occupational has the highest earnings?

best_occp <- jobs_gender
best_occp$major_category <- forcats::fct_reorder(best_occp$major_category,
                                                 best_occp$total_earnings,
                                                 median)

best_earning_by_occp <- ggplot(best_occp,
                               aes(major_category, total_earnings)) +
    geom_boxplot() +
    labs(x = NULL, y = NULL) +
    coord_flip() +
    scale_y_continuous(label = scales::comma) +
    bigger_font()

## ggsave(filename = "best-earning-by-occp.pdf", best_earning_by_occp)
