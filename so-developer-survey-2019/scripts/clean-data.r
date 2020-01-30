

require(ggplot2)

theme_set(
    theme_minimal(base_size = 13, base_family = "Roboto Condensed") +
    theme(panel.grid = element_line(size = 0.3, color = "gray90"))
)

survey_raw <- read.csv("../data-raw/survey_results_public.csv", stringsAsFactors = FALSE)
names(survey_raw) <- tolower(names(survey_raw))
filtered_gender <- c("Man", "Woman", "Non-binary")


## US respondents only
survey_raw <- survey_raw[survey_raw$country %in% "United States" &
                         survey_raw$employment %in% "Employed full-time" &
                         survey_raw$convertedcomp > 3e4 & survey_raw$convertedcomp < 2e6, ]

## identify non-ICs, to remove
managers_ctos <- survey_raw[grepl(pattern = "Engineering manager|Product manager|Senior executive/VP", survey_raw$devtype), ]

## identify academics, to remove
academics <- survey_raw[grepl(pattern = "Academic researcher|Scientist|Educator", survey_raw$devtype), ]

## filter intended results
survey_results <- survey_raw[!survey_raw$respondent %in% c(managers_ctos$respondent, academics$respondent), ]

less_than_bachelor_levels <- c("I never completed any formal education",
                               "Primary/elementary school",
                               "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)",
                               "Some college/university study without earning a degree",
                               "Associate degree")
bachelor_levels <- c("Bachelor’s degree (BA, BS, B.Eng., etc.)")
graduate_levels <- c("Other doctoral degree (Ph.D, Ed.D., etc.)",
                     "Master’s degree (MA, MS, M.Eng., MBA, etc.)",
                     "Professional degree (JD, MD, etc.)")

survey_results$edlevel <- ifelse(survey_results$edlevel %in% less_than_bachelor_levels, "Less than bachelor's",
                          ifelse(survey_results$edlevel %in% bachelor_levels, "Bachelor's degree",
                          ifelse(survey_results$edlevel %in% graduate_levels, "Graduate degree", survey_results$edlevel)))
survey_results$edlevel <- factor(survey_results$edlevel,
                                 levels = c("Less than bachelor's", "Bachelor's degree", "Graduate degree"),
                                 ordered = TRUE)

table(survey_results$edlevel, useNA = "ifany")

survey_results$opensourcer <- ifelse(survey_results$opensourcer %in% c("Less than once per year"), "Sometimes",
                              ifelse(survey_results$opensourcer %in% c("Less than once a month but more than once per year",
                                                                       "Once a month or more often"), "Often",
                              ifelse(survey_results$opensourcer %in% c("Never"), "Never", survey_results$opensourcer)))

survey_results$opensourcer <- factor(survey_results$opensourcer,
                                     levels = c("Often", "Sometimes", "Never"),
                                     ordered = TRUE)

table(survey_results$opensourcer, useNA = "ifany")

survey_results$gender <- ifelse(grepl(pattern = "Non-binary", survey_results$gender), "Non-binary", survey_results$gender)
survey_results <- survey_results[survey_results$gender %in% filtered_gender, ]

table(survey_results$gender, useNA = "ifany")

survey_results <- survey_results[, c("respondent", "edlevel", "devtype", "opensourcer",
                                     "yearscodepro", "gender", "dependents", "convertedcomp")]


## Exploratory data analysis ---------------------------------------------------


## distribution of scale by gender
ggplot(survey_results, aes(convertedcomp, fill = gender)) +
    geom_density(alpha = 0.3, color = "white") +
    scale_x_log10(labels = scales::dollar_format()) +
    theme(legend.position = "top")

## median income by gender
tapply(survey_results$convertedcomp, survey_results$gender, median)

## Impact of developer role and experience -------------------------------------

filtered_devtype <- c("Other", "Student", "Marketing or Sales Professional")

devtypes <- survey_results$devtype
devtypes <- strsplit(devtypes, split = ";")

head(devtypes)

devtypes <- lapply(
    devtypes,
    function(x) {
        out <- tolower(x)
        out <- ifelse(grepl(pattern = "data scientist", out), "Data scientist",
               ifelse(grepl(pattern = "data or business", out), "Data analyst",
               ifelse(grepl(pattern = "desktop", out), "Desktop",
               ifelse(grepl(pattern = "embedded", out), "Embedded",
               ifelse(grepl(pattern = "devops", out), "DevOps",
               ifelse(grepl(pattern = "engineer, data", out), "Data engineer",
               ifelse(grepl(pattern = "site reliability", out), "DevOps", out)))))))
        out <- gsub(pattern = "developer, ", replacement = "", out)
        out <- tools::toTitleCase(out)
        out <- gsub(pattern = "Qa", replacement = "QA", out)
        out <- gsub(pattern = "Sre", replacement = "SRE", out)
        out <- gsub(pattern = "Devops", replacement = "DevOps", out)
        out
    }
)

devtypes <- setNames(devtypes, survey_results$respondent)
devtypes <- lapply(devtypes, function(x) data.frame(devtype = x))

for (i in seq_along(devtypes)) {
    devtypes[[i]]$respondent <- names(devtypes[i])
}

devtypes <- do.call("rbind", devtypes)
row.names(devtypes) <- NULL
devtypes <- devtypes[!is.na(devtypes$devtype) & !devtypes$devtype %in% filtered_devtype, ]

## merge salary
devtypes <- merge(devtypes, survey_results[, c("respondent", "convertedcomp", "gender")], by = "respondent", all.x = TRUE)
head(devtypes)

ggplot(devtypes, aes(gender, convertedcomp / 1e3, color = gender)) +
    geom_boxplot() +
    scale_y_log10(labels = scales::dollar_format()) +
    labs(x = NULL, y = NULL, title = "Salary and Gender in the 2019 Stack Overflow Developer Survey") +
    facet_wrap( ~ devtype) +
    coord_flip() +
    theme(legend.position = "none", axis.text.x.bottom = element_text(angle = 45, hjust = 0.5))

## Modeling --------------------------------------------------------------------
