


job_gender <- read.csv2("../data/V02.51.csv", skip = 2, stringsAsFactors = FALSE)

names(job_gender) <- c("year", "region", "unemploy_all", "unemploy_male",
                       "unemploy_female", "jobless_all", "jobless_male",
                       "jobless_female")

head(job_gender)
tail(job_gender)
dim(job_gender)
sapply(job_gender, class)

## -----------------------------------------------------------------------------
## tidy data

## remove empty row
job_gender <- job_gender[-1, ]


## year as integer
job_gender$year <- as.integer(gsub("(Sơ bộ)|\\s", "", job_gender$year))

for (i in 1:nrow(job_gender)) {
    if (is.na(job_gender$year[i])) {
        job_gender$year[i] <- job_gender$year[i - 1]
    }
}

table(job_gender$year, useNA = "ifany")

## remove rows having empty values
job_gender <- job_gender[job_gender$region != "", ]
table(job_gender$region, useNA = "ifany")


## recode region
job_gender$region <- stringi::stri_trans_general(job_gender$region, "latin-ascii")
job_gender$region[job_gender$region == "Bac Trung Bo va duyen hai mien Trung"] <- "BTB & DHMT"
job_gender$region[job_gender$region == "Trung du va mien nui phia Bac"] <- "TD & MNPB"
job_gender$region[job_gender$region == "Dong bang song Cuu Long"] <- "DB song Cuu Long"
job_gender$region[job_gender$region == "Dong bang song Hong"] <- "DB song Hong"
job_gender$region[job_gender$region == "CA NUOC"] <- "All Regions"
job_gender$region <- factor(job_gender$region)
table(job_gender$region, useNA = "ifany")

num_vars <- names(job_gender)[grepl("unemploy|jobless", names(job_gender))]
for (v in num_vars) {
    job_gender[[v]] <- as.numeric(job_gender[[v]])
}

## -----------------------------------------------------------------------------
## export cleaned data

## save(job_gender, file = "../data/job-gender.RData")
