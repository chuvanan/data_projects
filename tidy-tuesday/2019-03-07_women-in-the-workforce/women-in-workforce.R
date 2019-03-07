


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
## explore employed_gender
