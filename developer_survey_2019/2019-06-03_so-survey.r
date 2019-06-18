## @ancv
## This script is

## Load packages and import data -----------------------------------------------

require(ggplot2)
require(data.table)
so_survey <- fread("survey_results_public.csv")

## Expore data -----------------------------------------------------------------


## Who are survey respondents?
survey_respondents <- so_survey[, .N, by = .(MainBranch)][order(N, decreasing = TRUE)]
survey_respondents[, P := round(N * 100/ sum(N), 1)]
## => Most of respondents are professional developers (74%) or retired ones (8.5%)



## How do respondents feel about the quality of open source software (OSS)?
oss_quality <- so_survey[, .N, by = .(OpenSource)][order(N, decreasing = TRUE)]
oss_quality[, P := round(N * 100/ sum(N), 1)]
## => OSS either have equal (46.7%) as or higher (41.1%) quality than closed
## source softwares



## Where are respondents come from?
countries <- so_survey[, .N, by = .(Country)][order(N, decreasing = TRUE)]
dim(countries)                          # 180 nationals
countries[, P := round(N * 100/ sum(N), 1)]
## => one out of five respondents is American and one out of ten is Indian
countries[Country == "Viet Nam"]        # 231 guys coming from Viet Nam




## Respondent's level of formal education?
edu_levels <- so_survey[, .N, by = .(EdLevel)][order(N, decreasing = TRUE)]
edu_levels[, P := round(N * 100/ sum(N), 1)]
## => Around 2/3 of respondents had BS or MS degrees. Only 2.7% had Ph.D level.




## Non-degree education that respondents have used or participated in?
other_edu_levels <- so_survey[, .N, by = .(EduOther)][order(N, decreasing = TRUE)]
other_edu_levels[, P := round(N * 100/ sum(N), 1)]
## => There're many ways to become a 'someone who write codes', but MOOCs and
## self-taught seems to be the most popular choice.



## What do the respondents describe themselves?
dev_types <- so_survey[, .N, by = .(DevType)][order(N, decreasing = TRUE)]
dev_types[, P := round(N * 100/ sum(N), 1)]
dev_types[grepl(pattern = "Data", DevType, ignore.case = TRUE)][, sum(P)]
## => Only a small number of respondents described their word involving 'data'



## Which languages that respondents have used over the past year?
working_langs <- so_survey[, .N, by = .(LanguageWorkedWith)][order(N, decreasing = TRUE)]
working_langs[, P := round(N * 100/ sum(N), 1)]
## => Very diverse set of programming languages used by developers

most_used_langs <- strsplit(so_survey$LanguageWorkedWith, split = ";")
most_used_langs <- table(unlist(most_used_langs), useNA = "ifany")
sort(most_used_langs, decreasing = TRUE) * 100 / sum(most_used_langs)
## => But web technologies (JavaScript and HTML/CSS) still dominates the
## result. Others include: SQL, Python, Bash, Java, C++, C


## Respondent's development environment of choice?
dev_environ <- so_survey[, .N, by = .(DevEnviron)][order(N, decreasing = TRUE)]
dev_environ[, P := round(N * 100/ sum(N), 1)]

fav_dev_environ <- strsplit(so_survey$DevEnviron, split = ";")
fav_dev_environ <- table(unlist(fav_dev_environ), useNA = "ifany")
sort(fav_dev_environ, decreasing = TRUE) * 100 / sum(fav_dev_environ)
## => MS's products (Visual Studio Code and Visual Studio) are loved by
## developers. Vim is preferred by developers than its long-time competitor
## Emacs.
