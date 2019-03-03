
## Use grouping sets with data.table
library(data.table) # 1.12.0


## sample data
data(flights, package = "nycflights13")

flights <- as.data.table(flights)

dim(flights)
names(flights)
sapply(flights, class)
flights[, summary(.SD)]

## -----------------------------------------------------------------------------
## basic group-by summary

flights[, sum(distance), by = .(month, origin)]
flights[, sum(distance), by = .(origin, dest)]
flights[, sum(distance), by = .(month)]
flights[, sum(distance), by = .(origin)]

## -----------------------------------------------------------------------------
## aggregating sub-totals and grand totals

## https://stackoverflow.com/questions/9315258/aggregating-sub-totals-and-grand-totals-with-data-table
## https://github.com/Rdatatable/data.table/issues/1377

cubed <- cube(flights, sum(distance), by = c("month", "origin"))
cubed[order(month)]

dcast(cubed, origin ~ month, value.var = "V1")
dcast(cubed, month ~ origin, value.var = "V1")

## using more dimensions

cubed2 <- cube(flights, sum(distance), by = c("month", "origin", "carrier"))
cubed2[order(month)]

dcast(cubed2, month + carrier ~ origin, value.var = "V1")
dcast(cubed2, month + origin ~ carrier, value.var = "V1")
dcast(cubed2, carrier + origin ~ month, value.var = "V1")

groupingsets(flights, sum(distance),
             by = c("month", "origin", "carrier"),
             sets = list(c("month")))

groupingsets(flights, sum(distance),
             by = c("month", "origin", "carrier"),
             sets = list(c("origin")))

groupingsets(flights, sum(distance),
             by = c("month", "origin", "carrier"),
             sets = list(c("carrier")))

groupingsets(flights, sum(distance),
             by = c("month", "origin", "carrier"),
             sets = list(c("month", "carrier")))

groupingsets(flights, sum(distance),
             by = c("month", "origin", "carrier"),
             sets = list(c("origin", "carrier")))

groupingsets(flights, sum(distance),
             by = c("month", "origin", "carrier"),
             sets = list(c("origin", "carrier"),
                         c("month", "origin")))

groupingsets(flights, sum(distance),
             by = c("month", "origin", "carrier"),
             sets = list(character(0)))

rolledup <- rollup(flights, sum(distance), by = c("month", "origin", "carrier"))
rolledup[order(month)]
