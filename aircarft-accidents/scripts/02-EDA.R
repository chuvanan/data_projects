## 2019-03-14
## @ancv

## -----------------------------------------------------------------------------
## Load packages and import data

accidents <- read.csv("../data/baaa-aircraft-accidents.csv",
                      stringsAsFactors = FALSE)

dim(accidents)
sapply(accidents, class)
head(accidents)
tail(accidents)
summary(accidents)

## -----------------------------------------------------------------------------
## Clean data

## check duplicated records
anyDuplicated(accidents)
dup_records <- accidents[duplicated(accidents), ] # 56 cases

## see them all
merge(accidents, dup_records, by = c("Date", "AC_Type", "Fatalities",
                                     "Location", "Registration", "Operator"))

## remove duplicated records
accidents <- accidents[!duplicated(accidents), ]

accidents$Date <- as.Date(accidents$Date, format = "%b %d, %Y")
summary(accidents$Date)
## from 1918 to 2019

length(unique(accidents$Operator))
## 4154 unique operators
table(accidents$Operator == "") * 100 / nrow(accidents)
## 74% are empty character
accidents$Operator[accidents$Operator == ""] <- NA
table(is.na(accidents$Operator)) * 100 / nrow(accidents)
## 80% are NAs


length(unique(accidents$AC_Type))
## 1080 types of AC
anyNA(accidents$AC_Type)
any(accidents$AC_Type == "")
accidents$AC_Type[accidents$AC_Type == ""] <- NA

sum(is.na(accidents$AC_Type)) * 100 / nrow(accidents)

## top 10 types of AC
sort(table(forcats::fct_lump(accidents$AC_Type, n = 10)))
sort(table(forcats::fct_lump(accidents$AC_Type, n = 20)))

boeing <- accidents$AC_Type[grepl("Boeing", accidents$AC_Type)]
length(boeing)                          # 1271
sort(table(boeing))                     # both commercial and military AC

airbus <- accidents$AC_Type[grepl("Airbus", accidents$AC_Type)]
length(airbus)                          # 75
sort(table(airbus))

length(accidents$AC_Type[grepl("Boeing 737 MAX", accidents$AC_Type)])
## only 2 case of B737 MAX

anyNA(accidents$Fatalities)
hist(accidents$Fatalities[accidents$Fatalities < 30],
     breaks = 30, col = "steelblue", border = "white",
     xlab = "fatalities", ylab = "frequency",
     main = "Distribution of accidents fatalities")

anyNA(accidents$Registration)
length(unique(accidents$Registration)) / nrow(accidents)
## 95% are unique registrations

## -----------------------------------------------------------------------------
## Export

## save(accidents, file = "../data/cleaned-baaa-aircraft-accidents.RData")
