

## library(rvest)

## -----------------------------------------------------------------------------
## scrap data from site

## opm_url <- "http://manga24h.me/Onepunch-Man.htm"

## opm <- read_html(opm_url)

## chap_names <- opm %>%
##   html_nodes(xpath = "//div[@class='col-md-5 col-xs-8 chap_name']") %>%
##   html_text()

## chap_names <- trimws(chap_names, which = "both")

## chap_dates <- opm %>%
##   html_nodes(xpath = "//span[@class='chap-date']") %>%
##   html_text()

## chap_dates <- chap_dates[chap_dates != "" & !grepl("[a-z]", chap_dates)]

## chap_views <- opm %>%
##   html_nodes(xpath = "//span[@class='chap-views']") %>%
##   html_text()

## chap_views <- trimws(chap_views)

## onepunch <- data.frame(chap_names, chap_dates, chap_views, stringsAsFactors = FALSE)

## onepunch$chap_dates <- as.Date(onepunch$chap_dates, "%d/%m/%Y")
## onepunch <- onepunch[order(onepunch$chap_dates), ]
## onepunch$chap_views <- as.numeric(onepunch$chap_views)

## write.csv(onepunch, "~/Dropbox/onepunchman.csv", row.names = FALSE)

onepunch <- read.csv("~/Dropbox/onepunchman.csv", stringsAsFactors = FALSE)
onepunch$chap_dates <- as.Date(onepunch$chap_dates)

## -----------------------------------------------------------------------------
## visualise data

## cairo_pdf("~/Dropbox/onepunchman.pdf", width=11.197917, height=6.160871)

png("~/Dropbox/onepunchman.png", width=11.197917, height=6.160871, units = "in", res = 100)

par(bg = "gray95",
    mar = c(4, 4, 4, 2) + 0.2,
    family = "Lato Light")
plot.new()
plot.window(xlim = range(onepunch$chap_dates),
            ylim = range(onepunch$chap_views))
grid(NA, NULL)
abline(v = seq.Date(as.Date("2012-12-31"), length.out = 5, by = "years"),
       col = "lightgray", lty = "dotted")
abline(h = 0, col = "gray")
points(onepunch$chap_dates, onepunch$chap_views, type = "b",
       col = rgb(139, 0, 0, maxColorValue = 255))
axis(side = 1,
     at = seq.Date(as.Date("2012-12-31"), length.out = 5, by = "years"),
     labels = format(seq.Date(as.Date("2012-12-31"), length.out = 5, by = "years"), "%Y"),
     cex.axis = 0.8, col.axis = "gray40",
     lwd = 0)
axis(side = 2,
     at = seq(0, 4e5, by = 1e5),
     labels = c("0", "100k", "200k", "300k", "400k"),
     cex.axis = 0.8, col.axis = "gray40",
     lwd = 0, las = 1)
mtext(side = 2, line = 3,
      text = "Chapter views", cex = 0.9, col = "gray40")
mtext(side = 1, line = 3, font = 3, adj = 1,
      text = "Source: manga24h.com", cex = 0.8, col = "gray40")
mtext(side = 3, line = 3, font = 2, adj = 0, cex = 1.2,
      text = "No Superhero Lure: Number of One-punch Man's readers decreasing")
mtext(side = 3, line = 1.5, font = 3, adj = 0,  cex = 0.8, col = "gray20",
      text = "Measured by pageviews on manga24h.com")
text(x = as.Date("2012-10-26"), y = 258000, xpd = TRUE,
     labels = "Chap 1 released", cex = 0.8, col = "gray20")
text(x = as.Date("2013-02-03"), y = 412000, xpd = TRUE,
     labels = "Chap Special", cex = 0.8, col = "gray20")
text(x = as.Date("2013-12-02"), y = 185000, xpd = TRUE,
     labels = "Chap Bonus", cex = 0.8, col = "gray20")
text(x = as.Date("2017-04-10"), y = 32000, xpd = TRUE,
     labels = "Chap 109", cex = 0.8, col = "gray20")

dev.off()
