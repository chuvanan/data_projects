## calculate RCA index of Vietnam's export commodities
## anchu@rta.vn
## 09/12/2016

library(readxl)
library(data.table)

## import data
xls_files <- dir("~/Documents/data_projects/rca-index/Exported by country/",
                 full.names = TRUE)
countries <- lapply(xls_files, read_excel)

## make a column of country label
country_lb <- substring(xls_files, 68)
country_lb <- gsub(".xlsx", "", country_lb)
countries <- Map(function(dtf, x) {dtf$country <- x; dtf}, countries, country_lb)

## combine 'em all
countries <- do.call("rbind", countries)
countries <- as.data.table(countries)
setnames(countries, c("code", "product_lb", "exval_2001", "exval_2002",
                      "exval_2003", "exval_2004", "exval_2005", "exval_2006",
                      "exval_2007", "exval_2008", "exval_2009", "exval_2010",
                      "exval_2011", "exval_2012", "exval_2013", "exval_2014",
                      "country_lb"))

## reshape data by year
countries <- melt(countries,
                  measure.vars = grep("^exval", names(countries), value = TRUE),
                  variable.name = "year",
                  value.name = "exported_value")
countries[, c("year") := list(gsub("exval_", "", year))]

## calculating rca_index for vietnam
tpp_1 <- countries[year == "2014",
                   list(value1 = sum(exported_value)/1000),
                  by = list(code, product_lb)]
tpp_1[, c("rca_element1") := list(value1/sum(value1))]

tpp_vn <- countries[year == "2014" & country_lb == "Vietnam",
                    list(value2 = sum(exported_value)/1000),
                    by = list(code, product_lb)]
tpp_vn[, c("rca_element2") := list(value2/sum(value2))]

ghep <- merge(tpp_vn,
              tpp_1,
              by = c("code", "product_lb"))
ghep[, c("rca") := list(rca_element2/rca_element1)]
ghep[, c("log_rca") := list(log(rca))]
ghep <- ghep[, list(code, value2, rca, log_rca, product_lb)][order(-rca)]

fwrite(ghep, "~/Documents/data_projects/rca-index/rca_index_2014.csv")

