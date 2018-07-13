

library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)
source("helper-functions.R")

wc_matches <- read_csv("../data/WorldCupMatches.csv")
wc_players <- read_csv("../data/WorldCupPlayers.csv")
wc <- read_csv("../data/WorldCups.csv")

## prettify column names

wc_matches <- normalize_colnames(wc_matches)
wc_players <- normalize_colnames(wc_players)
wc <- normalize_colnames(wc)

## sanity check

str(wc_matches, give.attr = FALSE)
str(wc_players, give.attr = FALSE)
str(wc, give.attr = FALSE)

## home teams are more likely to win
wc_matches %>%
    group_by(HomeTeamGoals, AwayTeamGoals) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    ggplot(aes(HomeTeamGoals, AwayTeamGoals, size = sqrt(n / pi))) +
    geom_point() +
    scale_x_continuous(breaks = 0:10, limits = c(0, 10)) +
    scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
    geom_abline(slope = 1) +
    theme_minimal()

wc_matches %>%
    group_by(HomeTeamName) %>%
    count(HomeTeamGoals) %>%
    ungroup()

count(wc_matches, HomeTeamName) %>%
    arrange(n) %>%
    View()

wc_matches %>%
    group_by(AwayTeamName) %>%
    count(AwayTeamGoals) %>%
    ungroup()

HomeTeamPerf <- wc_matches %>%
    mutate(HomeTeamResult = case_when(
               HomeTeamGoals > AwayTeamGoals ~ "Won",
               HomeTeamGoals == AwayTeamGoals ~ "Tied",
               HomeTeamGoals < AwayTeamGoals ~ "Lost"
           )) %>%
    group_by(HomeTeamName) %>%
    summarise(HomeTeamWonRate = mean(HomeTeamResult == "Won"),
              HomeTeamTiedRate = mean(HomeTeamResult == "Tied"),
              HomeTeamLostRate = mean(HomeTeamResult == "Lost")) %>%
    ungroup()

AwayTeamPerf <- wc_matches %>%
    mutate(AwayTeamResult = case_when(
               HomeTeamGoals < AwayTeamGoals ~ "Won",
               HomeTeamGoals == AwayTeamGoals ~ "Tied",
               HomeTeamGoals > AwayTeamGoals ~ "Lost"
           )) %>%
    group_by(AwayTeamName) %>%
    summarise(AwayTeamWonRate = mean(AwayTeamResult == "Won"),
              AwayTeamTiedRate = mean(AwayTeamResult == "Tied"),
              AwayTeamLostRate = mean(AwayTeamResult == "Lost")) %>%
    ungroup()

full_join(HomeTeamPerf, AwayTeamPerf,
          by = c("HomeTeamName" = "AwayTeamName")) %>%
    ggplot(aes(HomeTeamLostRate, AwayTeamLostRate)) +
    geom_point()


full_join(HomeTeamPerf, AwayTeamPerf,
          by = c("HomeTeamName" = "AwayTeamName")) %>%
    ggplot(aes(HomeTeamTiedRate, AwayTeamTiedRate)) +
    geom_point()

out <- full_join(HomeTeamPerf, AwayTeamPerf,
                 by = c("HomeTeamName" = "AwayTeamName"))

out %>%
    hchart(type = "point",
           hcaes(HomeTeamWonRate * 100, AwayTeamWonRate * 100)) %>%
    hc_plotOptions(point = list(dataLabels = list(enabled = TRUE))) %>%
    hc_title(text = "Home Teams Are More Likely To Win?",
             style = list(fontSize = "24px")) %>%
    hc_xAxis(labels = list(style = list(fontSize = "13px"),
                           format = "{value}%"),
             title = list(text = "Home Team's Win Rate")) %>%
    hc_yAxis(labels = list(style = list(fontSize = "13px"),
                           format = "{value}%"),
             title = list(text = "Away Team's Win Rate"),
             showLastLabel = FALSE) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "",
               formatter = JS("function(){
                              return ('<b>'+ this.point.HomeTeamName + '</b><br>'+
                              'Home Team Win Rate: '+ Highcharts.numberFormat(this.x, 0)+'%' + '<br>' +
                              'Away Team Win Rate: '+ Highcharts.numberFormat(this.y, 0)+'%')}")) %>%
    hc_add_theme(hc_theme_smpl(
        chart = list(style = list(fontSize = "16px")))) %>%
    hc_size(width = 800, height = 700)
