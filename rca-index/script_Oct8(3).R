# Caculate Revealed Competitive Advantage Index of Vietnam's export commodities 

# Remove all objects ------------------------------------------------------
rm(list=ls())

# Set working directory ---------------------------------------------------
setwd("D:/WORKS/1. My Projects/25.RCA Proj/Input/Exported by country/")  

# Load packages -----------------------------------------------------------
library(dplyr)
library(stringr)
library(xlsx)
library(gdata)
library(tidyr)
library(magrittr)

# Read export datasets --------------------------------------------------------

setwd("~/Documents/data_projects/rca-index/Exported by country/")

path_export <- dir()

for (i in 1:length(path_export)){
  print(paste0(path_export[i]))
  file <- read.xlsx(paste0(path_export[i]), header = TRUE, sheetIndex = 1)
  file$country <- gsub(".xlsx", "", path_export[i])
  assign(paste0("data", i), file)
}

export <- rbind(data1, data2, data3, data4, data6, data7, 
                data8, data9, data10, data11, data12, data13) # Data5 was removed: china excluded
keep(export, sure = TRUE)

# Do some cleaning --------------------------------------------------------

export.long <- export %>% 
  gather(year, export.value, Exported.value.in.2001:Exported.value.in.2014)

export.long$year <- as.factor(gsub("Exported.value.in.", "", export.long$year))

colnames(export.long)[1] <- "code"
colnames(export.long)[2] <- "product"

## -----------------------------------------------------------------------------
           
tpp_1 <- export.long %>%
  filter(year == "2014") %>%
  group_by(code, product) %>% 
  summarise(value1 = sum(export.value)/1000)
tpp_1$rca_element1 <- tpp_1$value1/sum(tpp_1$value1)

tpp_vn <- export.long %>%
  filter(year == "2014" & country == "Vietnam") %>%
  group_by(code, product) %>% 
  summarise(value2 = sum(export.value)/1000) 
tpp_vn$rca_element2 <- tpp_vn$value2/sum(tpp_vn$value2)

ghep  <- merge(tpp_vn, tpp_1, by = c("code","product"))
ghep$rca <- ghep$rca_element2/ghep$rca_element1

table1 <- ghep %>%
  mutate(log.rca = log(rca)) %>%
  select(code, value2, rca, log.rca, product) %>%
  arrange(desc(rca))

write.xlsx(table1, "../../Output/result2014.xlsx")



rm(list = ls())


x <- letters

## integer subsetting
x[1]
x[1:3]
x[c(1, 1)]

## character subsetting
x[x == "a"]
x[x %in% c("b", "f", "g")]
x[!x %in% c("b", "f", "g")]

## logical subsetting
x == "a"
x[c(TRUE, FALSE)]

merge(mtcars,
      aggregate(mpg ~ cyl, data = mtcars, sum),
      by = "cyl",
      all.x = TRUE)

library(data.table)


## mtcars[i, j, by]

str(mtcars)
mtcars <- as.data.table(mtcars)
mtcars[, c("avg_mpg") := mean(mpg), by = list(cyl)]
mtcars


