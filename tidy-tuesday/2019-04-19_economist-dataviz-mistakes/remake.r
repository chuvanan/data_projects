

download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/corbyn.csv",
              destfile = "data/corbyn.csv")
download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/brexit.csv",
              destfile = "data/brexit.csv")
download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/dogs.csv",
              destfile = "data/dogs.csv")
download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/eu_balance.csv",
              destfile = "data/eu_balance.csv")
download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/pensions.csv",
              destfile = "data/pensions.csv")
download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/trade.csv",
              destfile = "data/trade.csv")
download.file(url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv",
              destfile = "data/women_research.csv")

corbyn <- readr::read_csv("data/corbyn.csv")
brexit <- readr::read_csv("data/brexit.csv")
dogs <- readr::read_csv("data/dogs.csv")
eu_balance <- readr::read_csv("data/eu_balance.csv")
pensions <- readr::read_csv("data/pensions.csv")
trade <- readr::read_csv("data/trade.csv")
women_research <- readr::read_csv("data/women_research.csv")


## ------------------------------

corbyn <- corbyn[order(corbyn$avg_facebook_likes), ]

par(cex = 1.3, oma = c(0, 3, 0, 0), family = "Lato")
barplot(corbyn$avg_facebook_likes,
        names.arg = corbyn$political_group,
        horiz = TRUE, las = 1, col = "steelblue", border = "white",
        main = "Left-click", xlab = "Average number of likes per FB post",
        cex.main = 1.5)
box()
## R.devices::toSVG(name = "economist-corbyn", width = 8.125000, height = 6.545276)

## ------------------------------

brexit$date <- as.Date(brexit$date, format = "%d/%m/%y")

res_right <- loess(percent_responding_right ~ as.numeric(date), data = brexit)
res_wrong <- loess(percent_responding_wrong ~ as.numeric(date), data = brexit)

par(cex = 1.3, mai = c(0.8, 1.2, 1, 0.5), family = "Lato")
plot(brexit$date, brexit$percent_responding_right,
     col = scales::alpha("blue", 0.3), pch = 19, ylim = c(38, 49), las = 1,
     ylab = "% responding", xlab = "",
     main = "Bremorse", cex.main = 1.4, xaxt = "n")
lines(predict(res_right), x = brexit$date, col = "blue", lwd = 5)
points(brexit$date, brexit$percent_responding_wrong,
       col = scales::alpha("red", 0.3), pch = 19)
lines(predict(res_wrong), x = brexit$date, col = "red", lwd = 5)
axis(1, at = seq(17000, 17800, 200),
     labels = format(as.Date.numeric(seq(17000, 17800, 200),
                                     origin = "1970-01-01"),
                     "%d/%m/%y"))

## ------------------------------


trade$manufacture_employment <- round(trade$manufacture_employment / 1e6, 1)
trade$trade_deficit <- round(trade$trade_deficit / 1e9, 1)

par(family = "Lato", mai = c(1, 0.5, 1, 1),
    mfrow = c(1, 2), cex.axis = 1.2)
barplot(trade$trade_deficit, yaxt = "n", names.arg = trade$year,
        col = "steelblue", border = "white")
axis(4, las = 1)
title(main = "Trade deficit with China in goods, $bn",
      cex.main = 1.5)
plot(trade$year, trade$manufacture_employment,
     type = "l", lwd = 4, col = "steelblue", ylim = c(5, 20),
     las = 1, ann = FALSE, yaxt = "n")
title(main = "Manufacturing employment, mil", cex.main = 1.5)
axis(4, las = 1)
## R.devices::toSVG("trade", width = 12.614583, height = 6.711505)

## ------------------------------

unique(pensions$country)

par(family = "Lato", cex = 1.3, mai = c(1.2, 1.5, 0.8, 0.5),
    bg = "gray95")
plot(pensions$pop_65_percent,
     pensions$gov_spend_percent_gdp,
     xlim = c(0, 30), ylim = c(0, 20), las = 1,
     pch = 21, bg = scales::alpha("steelblue", 0.5), col = "white", cex = 2,
     xlab = "Population aged 65 and over, % of total",
     ylab = "Government spending on\npension benefit, % of GDP",
     main = "Brazil's golden oldie blowout", cex.main = 1.5)
points(pensions$pop_65_percent[pensions$country == "Brazil"],
       pensions$gov_spend_percent_gdp[pensions$country == "Brazil"],
       cex = 2, col = "steelblue", pch = 19)
text(pensions$pop_65_percent[pensions$country == "Brazil"] + 1.1,
     pensions$gov_spend_percent_gdp[pensions$country == "Brazil"] + 1.1,
     labels = "Brazil", cex = 1.1)
## R.devices::toSVG("pension-brazil", width = 8.062500, height = 6.618001)

## ------------------------------

health <- subset(women_research, field == "Health sciences")
health <- health[order(health$percent_women), ]
health$percent_women <- health$percent_women * 100

par(family = "Lato", mai = c(1, 2, 0.7, 0.5), cex = 1.3)
barplot(health$percent_women,
        names.arg = health$country, horiz = TRUE, las = 1,
        col = "steelblue", border = "white",
        main = "Women among researchers with papers published 2011-15")
mtext(side = 1, "% of total, in health science", line = 2.2, cex = 1.3)
## R.devices::toSVG("women-search-health-science", width = 8.614583, height = 5.215442)

## ------------------------------


par(cex = 1.3, family = "Lato", mai = c(0.7, 1.2, 0.8, 1.2), bg = "gray96")
plot(dogs$year, dogs$avg_neck, type = "l", col = "blue", las = 1,
     ylim = c(38, 45), lwd = 5, ylab = "", xlab = "")
mtext(side = 2, "Neck size, cm", line = 2.5, cex = 1.4, col = "blue")
title(main = "Fit as a butcher's dog", cex.main = 1.4)
par(new = TRUE)
plot(dogs$year, dogs$avg_weight, xaxt = "n",
     yaxt = "n", xlab = "", ylab = "", ylim = c(18, 23),
     type = "l", lwd = 5, col = "red4")
axis(4, las = 1)
mtext(side = 4, "Weight, kg", line = 2.5, cex = 1.4, col = "red4")
## R.devices::toSVG("dog", width = 7.958333, height = 7.043963)
