

library(lattice)

library(ggplot2)

hanoi_weather <- read.csv("~/Dropbox/freemeteo/hanoi_weather_history.csv",
                          stringsAsFactors = F)

hanoi_weather$date <- as.Date(hanoi_weather$date)

hanoi_weather <- subset(hanoi_weather, date >= as.Date("1974-09-20"))

ggplot(hanoi_weather, aes(date, pressure)) +
  geom_line()

with(hanoi_weather,
     plot(date, pressure, type = "l"))
