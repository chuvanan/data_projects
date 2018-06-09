
library(rvest)

spider <- function(u, i) {
    require(rvest)
    urls <- paste0(u, i)
    docs <- data.frame(titles = character(0), dates = character(0),
                       stringsAsFactors = FALSE)
    for (l in urls) {
        site <- read_html(l)
        titles <- html_text(html_nodes(site, xpath = "//p[@class='title']"))
        dates <- html_text(html_nodes(site, xpath = "//p[@class='green']"))
        temp <- data.frame(titles, dates, stringsAsFactors = FALSE)
        docs <- rbind(docs, temp)
    }
    docs
}

base_url <- "http://csdl.thutuchanhchinh.vn/Pages/vbqppl.aspx?&SearchIn=Title,TrichYeu&Page="
pages <- 1:319

## out <- spider(u = base_url, i = pages)

out$titles <- gsub("[\r\n]", "", out$titles)
out$titles <- trimws(out$titles, which = "both")

out$dates <- gsub("[\r\n]", "", out$dates)
out$dates <- trimws(out$dates, which = "both")
out$dates <- gsub("Ban hành: \\s+", "", out$dates)

out <- out[out$dates != "...", ]

write.csv(out, "csdl_thutuchanhchinh.csv", row.names = FALSE)


## -----------------------------------------------------------------------------

out <- read.csv("csdl_thutuchanhchinh.csv", stringsAsFactors = FALSE)

out$dates <- as.Date(out$dates, "%d/%m/%Y")
out$years <- substr(out$dates, 1, 4)
out$years <- as.integer(out$years)
out$n <- 1

count_by_date <- aggregate(n ~ years, data = out, sum)
count_by_date <- subset(count_by_date, years >= 1991)
count_by_date$cols <- ifelse(count_by_date$years == 2016, "red4", "ivory4")

cairo_pdf("thutuc_hanhchinh.pdf", width = 6.989583, height = 6.992017)

par(bg = "gray98", mar = c(5, 5, 5, 2),
    family = "Lato Light")
x_labels <- paste0("'", substr(count_by_date$years, 3, 4))
bp <- barplot(count_by_date$n, names.arg = FALSE,
              col = count_by_date$cols, border = FALSE, axes = FALSE, xpd = TRUE)
text(30.7, 190, labels = "175", col = "red4", xpd = TRUE, cex = 0.7, font = 2)
grid(NA, NULL, col = "gray98", lty = "solid")
axis(side = 2, cex.axis = 0.8, col.axis = "gray40",
     las = 1, col = "gray98", col.ticks = "gray40", tck = -0.01)
axis(side = 1, at = bp, labels = x_labels, line = -1,
     lwd = 0, col.axis = "gray40", cex.axis = 0.8)
mtext(side = 1, line = 1.5, text = "Năm ban hành", adj = 0,
      cex = 0.9, col = "gray40")
mtext(side = 2, line = 3, text = "Số văn bản hành chính",
      adj = 1, las = 3, cex = 0.9, col = "gray40")
mtext(side = 3, line = 3, text = "Dấu hiệu của cải cách thủ tục hành chính?",
      adj = 0, cex = 1.2, font = 2)
mtext(side = 3, line = 1.5, text = "Số lượng văn bản ban hành năm 2016 thấp nhất trong 10 năm gần đây",
      adj = 0, cex = 1, font = 3, col = "gray20")
mtext(side = 1, line = 3, text = "Nguồn: Cơ Sở Dữ Liệu Quốc Gia Về Thủ Tục Hành Chính",
      cex = 0.8, col = "gray40", adj = 1, font = 3)

dev.off()
