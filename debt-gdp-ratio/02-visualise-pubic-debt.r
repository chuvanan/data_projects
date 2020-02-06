
public_debt <- read.csv("./public-debt-ratio.csv", stringsAsFactors = FALSE)
public_debt$region <- factor(public_debt$region)

high_risk_conds <- public_debt$debt_gdp_ratio > 100 | public_debt$share_of_debt > 10
top_risk <- c("United States", "Japan", "China", "Italy", "Greece")
public_debt$high_risk <- ifelse(high_risk_conds, "red", "darkgray")

par(family = "Roboto Condensed", cex = 1.4, mar = c(4.1, 4.1, 4, 2), xpd = TRUE)
plot(public_debt$debt_gdp_ratio,
     public_debt$share_of_debt, las = 1,
     pch = 21, bg = public_debt$high_risk, col = "white",
     xlab = "debt/gdp ratio (%)", ylab = "share of debt (%)",
     main = "Percentage of debt vs. gdp/debt by country")
text(public_debt$debt_gdp_ratio[public_debt$country %in% top_risk],
     public_debt$share_of_debt[public_debt$country %in% top_risk],
     labels = public_debt$country[public_debt$country %in% top_risk], pos = 4)
