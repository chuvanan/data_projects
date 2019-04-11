
## -----------------------------------------------------------------------------
## Load packages and data

require(dplyr)
require(purrr)

player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")
grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")
grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")


## -----------------------------------------------------------------------------
## Inspect data

glimpse(player_dob)
glimpse(grand_slams)
glimpse(grand_slam_timeline)

## -----------------------------------------------------------------------------
## EDA

## Who was the youngest to win a major title?
player_dob %>%
    filter(age == min(age, na.rm = TRUE)) # Martina Hingis, counting by days
difftime(as.Date("1997-01-25"), as.Date("1980-09-30"))

player_dob %>%
    mutate(age_in_year = as.numeric(format(date_of_first_title, "%Y")) -
               as.numeric(format(date_of_birth, "%Y"))) %>%
    filter(age_in_year == min(age_in_year, na.rm = TRUE)) %>%
    arrange(age) %>%
    select(name, grand_slam, age, age_in_year)
