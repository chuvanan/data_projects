

require(rvest)
options(scipen = 999)

public_debt_wiki <- "https://en.wikipedia.org/wiki/List_of_countries_by_public_debt#Public_debt_as_%_of_GDP"
gdp_wiki <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)"

## -----------------------------------------------------------------------------
## Extract public debt data

public_debt_page <- read_html(public_debt_wiki)
public_debt_table <- html_table(public_debt_page, fill = TRUE)[[3]]
names(public_debt_table) <- c("country", "debt_gdp_ratio", "date1",
                              "total_gov_debt_gdp_ratio",
                              "net_gov_debt_gdp_ratio", "date2", "region")

str(public_debt_table)
range(public_debt_table$debt_gdp_ratio, na.rm = TRUE)
range(public_debt_table$total_gov_debt_gdp_ratio, na.rm = TRUE)
range(public_debt_table$net_gov_debt_gdp_ratio, na.rm = TRUE)

## how updated the data?
table(public_debt_table$date1, useNA = "ifany")

## keep 2017 only
public_debt_table <- public_debt_table[which(public_debt_table$date1 == 2017), ]
public_debt_table <- public_debt_table[, c("country", "debt_gdp_ratio", "region")]
public_debt_table <- public_debt_table[public_debt_table$country != "World", ]


## -----------------------------------------------------------------------------
## Extract GDP data

gdp_page <- read_html(gdp_wiki)
gdp_tables <- html_table(gdp_page, fill = TRUE)
gdp_un_table <- gdp_tables[[6]] # because UN has data in 2017
names(gdp_un_table) <- c("rank", "country", "gdp")

gdp_un_table <- gdp_un_table[!grepl(pattern = "^World", gdp_un_table$country), ] # remove world stats
gdp_un_table$country <- gsub(pattern = "\\[.*\\]", replacement = "", gdp_un_table$country) # remove ref links
gdp_un_table$gdp <- gsub(pattern = ",", replacement = "", gdp_un_table$gdp) # clean gdp numbers
gdp_un_table$gdp <- as.numeric(gdp_un_table$gdp)


## -----------------------------------------------------------------------------
## Combine public debt and gdp data

## resolve difference in country names
public_debt_table$country[public_debt_table$country == "Macedonia"] <- "North Macedonia"
gdp_un_table$country[gdp_un_table$country == "Côte d'Ivoire"] <- "Cote d'Ivoire"
public_debt_table$country[public_debt_table$country == "Sao Tome and Principe"] <- "São Tomé and Príncipe"
public_debt_table$country[public_debt_table$country == "Burma"] <- "Brunei"


## keep countries that have full data
public_debt_table <- merge(public_debt_table, gdp_un_table[, c("country", "gdp")], by = "country")

## calculate debt per country
public_debt_table$debt <- public_debt_table$gdp * (public_debt_table$debt_gdp_ratio / 100)

## share of debt globally
public_debt_table$share_of_debt <- vapply(public_debt_table$debt, function(x) x * 100 / sum(public_debt_table$debt), numeric(1))

## export data
write.table(public_debt_table, file = "./public-debt-ratio.csv", sep = ",", row.names = FALSE)
