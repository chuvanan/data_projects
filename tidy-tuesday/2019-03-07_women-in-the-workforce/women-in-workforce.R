
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
