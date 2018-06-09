


## -----------------------------------------------------------------------------
## helper functions

parseDates <- function(x) {

    if (nchar(x) == 10L) {
        d <- substr(x, 1, 6)
        d <- as.Date(d, format = "%y%m%d")
    }

    if (nchar(x) == 9L) {               # adhoc
        d <- gsub("22016", "2016", x)
        d <- as.Date(d, format = "%d%m%Y")
    }

    if (nchar(x) == 8L) {
        d <- as.Date(x, format = "%d%m%Y")
    }
    d
}

## -----------------------------------------------------------------------------
## get data

notes <- readLines("~/Documents/rta-app-releases/rtwork-releases.rtf")

## keep release dates and builds

notes <- notes[grepl("[0-9]{1}\\.[0-9]{1}\\.[0-9]{1,3}_", notes)]

notes <- gsub("\\\\b|\\\\c?f[0-9]{1,2}|\\s|\\\\", "", notes)

notes <- gsub("\\(mergedwith0.2.33\\)|\\(mergedwith28\\)", "", notes)

notes <- strsplit(notes, split = "_")

notes <- lapply(notes, function(x) x[1:2])

builds <- sapply(notes, "[", 1)

dates <- sapply(notes, "[", 2)

dates <- sapply(dates, parseDates, USE.NAMES = FALSE)
dates <- as.Date.numeric(dates, origin = "1970-01-01")

release.notes <- data.frame(dates, builds, stringsAsFactors = FALSE)

## export data

write.csv(release.notes, "~/Documents/rta-app-releases/rtwork-releases.csv", row.names = FALSE)



## -----------------------------------------------------------------------------
## rtsurvey


rtsurvey.notes <- readLines("~/Documents/rta-app-releases/rtsurvey-releases.rtf")

rtsurvey.notes <- rtsurvey.notes[grepl("[0-9]{1}\\.[0-9]{1}\\.[0-9]{1,3}_", rtsurvey.notes)]

rtsurvey.notes <- gsub("\\\\b|\\\\c?f[0-9]{1,2}|\\s|\\\\", "", rtsurvey.notes)

rtsurvey.notes <- gsub("\\(Fixmistakefrom2.1.9\\)", "", rtsurvey.notes)

rtsurvey.notes <- strsplit(rtsurvey.notes, split = "_")

rtsurvey.notes <- lapply(rtsurvey.notes, function(x) x[1:2])

rtsurvey.builds <- sapply(rtsurvey.notes, "[", 1)

rtsurvey.dates <- sapply(rtsurvey.notes, "[", 2)

rtsurvey.dates <- sapply(rtsurvey.dates, parseDates, USE.NAMES = FALSE)
rtsurvey.dates <- as.Date.numeric(rtsurvey.dates, origin = "1970-01-01")

rtsurvey.notes <- data.frame(rtsurvey.dates, rtsurvey.builds, stringsAsFactors = FALSE)

write.csv(rtsurvey.notes, "~/Documents/rta-app-releases/rtsurvey-releases.csv", row.names = FALSE)
