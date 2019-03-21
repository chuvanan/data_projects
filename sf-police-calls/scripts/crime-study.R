

## load required packages
library(data.table)
options(scipen = 999)

## read in incidents dataset
incidents <- fread("../data/police-department-incidents.csv")

## read in calls dataset
calls <- fread("../data/police-department-calls-for-service.csv")

## -----------------------------------------------------------------------------
## first poke and prod

str(incidents)
str(calls)

dim(incidents)
dim(calls)

incidents[, sapply(.SD, class)]
calls[, sapply(.SD, class)]

incidents[, anyDuplicated(.SD)]         # no
calls[, anyDuplicated(.SD)]
calls[, sum(duplicated(.SD))]           # 50

## count unique values
incidents[, sapply(.SD, function(x) length(unique(x)))]
calls[, sapply(.SD, function(x) length(unique(x)))]

## perct of unique values
incidents[, sapply(.SD, function(x) length(unique(x)) * 100 / .N)]
calls[, sapply(.SD, function(x) length(unique(x)) * 100 / .N)]


## aggregate number of reported incidents by date
incidents[, Date := as.Date(Date)]
daily_incidents <- incidents[, .(n_incidents = .N), by = .(Date)]
daily_incidents <- daily_incidents[order(Date)]
daily_incidents[, range(Date)]          # form 2003-01-01 to 2018-05-15

## trend of reported incidents
png(file = "../figures/incidents-trend.png",
    width = 962, height = 713)
par(cex = 1.7)
daily_incidents[
  , plot(Date, n_incidents, type = "l", col = "gray", las = 1,
         ylab = "# reported incidents", xlab = "date")
]
grid()
dev.off()


## aggregate number of calls for polices
setnames(calls, names(calls), gsub(" ", "", names(calls)))
calls[, CallDate := as.Date(CallDate)]
daily_calls <- calls[, .(n_calls = .N), by = .(CallDate)]
daily_calls <- daily_calls[order(CallDate)]
daily_calls[, range(CallDate)]          # from 2016-03-30 to 2018-12-31


## trend of calls
png(filename = "../figures/calls-trend.png",
    width = 962, height = 713)
par(cex = 1.7)
daily_calls[
  , plot(CallDate, n_calls, type = "l", col = "blue", las = 1,
         xlab = "date", ylab = "# calls")
]
grid()
dev.off()

## -----------------------------------------------------------------------------
## join

shared_dates <- merge(daily_calls, daily_incidents,
                      by.x = c("CallDate"), by.y = c("Date"))

png(filename = "../figures/calls-vs-incidents-trend.png",
    width = 962, height = 713)
par(cex = 1.8)
shared_dates[
  , plot(CallDate, n_calls, col = "blue", xlab = "date",
         las = 1, ylab = "frequency")
]
shared_dates[
  , points(CallDate, n_incidents, col = "red4")
]
legend(as.Date("2017-10-01"), 1300, legend = c("# calls", "# incidents"),
       col = c("blue", "red4"), pch = 1)
dev.off()

par(cex = 1.3)
shared_dates[
  , plot(n_calls, n_incidents, col = "blue", las = 1,
         xlab = "# calls", ylab = "# incidents")
]

daily_cor <- shared_dates[, cor(n_calls, n_incidents)]

shared_dates[, Month := month(CallDate)]

agg_by_month <- shared_dates[, .(monthly_incidents = sum(n_incidents),
                                 monthly_calls = sum(n_calls)),
                             by = .(Month)]
correlation_by_month <- agg_by_month[, cor(monthly_calls, monthly_incidents)]

## -----------------------------------------------------------------------------
## filtering join
