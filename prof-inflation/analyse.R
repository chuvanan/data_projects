
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(svglite)

profs <- read.csv("~/ownCloud/data_projects/prof-inflation/cleaned-profs.csv",
                  stringsAsFactors = FALSE)

profs$age <- profs$nam - as.numeric(substr(profs$ngaysinh, 1, 4))

sort(prop.table((table(profs$nganh))))

ordered <- profs %>%
  group_by(nam, nganh) %>%
  summarise(n = n()) %>%
  group_by(nganh) %>%
  mutate(cum_n = cumsum(n)) %>%
  group_by(nganh) %>%
  arrange(desc(cum_n)) %>%
  slice(1) %>%
  arrange(desc(cum_n))

ordered1 <- profs %>%
  group_by(nam, nganh) %>%
  summarise(n = n()) %>%
  group_by(nganh) %>%
  mutate(cum_n = cumsum(n)) %>%
  group_by(nganh) %>%
  arrange(desc(cum_n)) %>%
  slice(c(1, length(n)))

profs %>%
  group_by(nam, nganh) %>%
  summarise(n = n()) %>%
  group_by(nganh) %>%
  mutate(cum_n = cumsum(n)) %>%
  mutate(nganh_ft = factor(nganh, levels = ordered$nganh)) %>%
  ggplot(aes(nam, cum_n)) +
  geom_line(color = "orange", size = 0.8) +
  geom_hline(yintercept = 0, color = "gray90", size = 0.2) +
  geom_hline(yintercept = 1000, color = "gray90", size = 0.2) +
  geom_hline(yintercept = 2000, color = "gray90", size = 0.2) +
  facet_wrap(~ nganh_ft) +
  scale_x_continuous(breaks = c(1984, 2012), labels = c(1980, 2016)) +
  scale_y_continuous(breaks = c(0, 1000, 2000), labels = c(0, 1000, 2000)) +
  labs(x = NULL, y = NULL,
       title = "Sự thống trị của y học trong bảng phong hàm", subtitle = "Biểu đồ số lượng GS/PGS được phong tặng từ 1980 tới 2016.\nNăm 2016 có gần 2000 GS/PGS y học. Bình quân 6 người thì có 1 người trong ngành y.",
       caption = "Dữ liệu từ hdcdgsnn.gov.vn") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text.x = element_text(face = "bold", color = "gray20", hjust = 0),
        axis.text = element_text(color = "gray45", size = 8),
        plot.title = element_text(face = "bold"))

## ggsave("~/Documents/data_projects/prof-inflation/figures/profs-growth.svg")

## thong ke so luong GS/PGS theo nam
## profs %>%
##   group_by(nam, title) %>%
##   summarise(n = n()) %>%
##   ggplot(aes(nam, n, group = title)) +
##   geom_line(aes(linetype = title)) +
##   geom_point() +
##   labs(x = NULL, y = NULL, title = "Số lượng GS/PGS được phong hàm") +
##   theme(legend.position = c(0.8, 0.9),
##         legend.direction = "horizontal",
##         legend.title = element_blank(),
##         legend.background = element_blank(),
##         plot.title = element_text(face = "bold"))

## ggsave("~/Documents/data_projects/prof-inflation/figures/profs-quant.svg",
##        width = 8, height = 5, dpi = 300)

## thong ke do tuoi trung binh -------------------------------------------------

## profs %>%
##   group_by(nam) %>%
##   summarise(avg_age = median(age, na.rm = TRUE),
##             min_age = min(age, na.rm = TRUE),
##             max_age = max(age, na.rm = TRUE)) %>%
##   ggplot() +
##   geom_line(aes(nam, max_age), color = "gray80") +
##   geom_line(aes(nam, min_age), color = "gray80") +
##   geom_point(aes(nam, min_age), color = "gray60") +
##   geom_point(aes(nam, max_age), color = "gray60") +
##   geom_point(aes(nam, avg_age), size = 2, color = "red4") +
##   geom_label(aes(1984, 35), label = "trẻ tuổi nhất", color = "gray60") +
##   geom_label(aes(1988, 80), label = "lớn tuổi nhất", color = "gray60") +
##   geom_label(aes(2005, 55), label = "trung bình", color = "red4") +
##   labs(x = NULL, y = "độ tuổi khi được phong hàm", title = "Trẻ hóa đội ngũ giáo sư") +
##   theme_bw()

## ggsave("~/Documents/data_projects/prof-inflation/figures/profs-age.png",
##        width = 6.5, height = 4, dpi = 100)

## thong ke theo nganh ---------------------------------------------------------

## profs %>%
##   group_by(nam, nganh) %>%
##   summarise(n = n()) %>%
##   group_by(nganh) %>%
##   mutate(cum_n = cumsum(n)) %>%
##   arrange(nganh) %>%
##   ggplot(aes(nam, cum_n, group = nganh)) +
##   geom_line() +
##   labs(x = NULL, y = NULL) +
##   theme_bw()

## profs$uni_id <- paste0(profs$hoten, profs$ngaysinh)
## full_profs <- filter(profs, title == "GS")
## asoc_profs <- filter(profs, title == "PGS")
## profs <- filter(profs, uni_id %in% intersect(full_profs$uni_id, asoc_profs$uni_id))
## profs <- arrange(profs, uni_id, nam)

## split_profs <- split(profs, profs$uni_id)

## split_profs <- lapply(
##   split_profs,
##   function(df) {
##     if (length(unique(df$nganh)) == 2) {
##       df$nganh <- df$nganh[df$title == "GS"]
##       df
##     } else {
##       df
##     }
##   }
## )

## profs <- do.call("rbind", split_profs)

## profs <- profs %>%
##   group_by(hoten, ngaysinh) %>%
##   arrange(nam) %>%
##   mutate(gap_year = diff(nam)) %>%
##   arrange(uni_id)

## asoc_profs <- profs %>%
##   filter(title == "PGS") %>%
##   group_by(nganh) %>%
##   summarise(asoc_age = median(age, na.rm = TRUE)) %>%
##   arrange(nganh)

## full_profs <- profs %>%
##   filter(title == "GS") %>%
##   group_by(nganh) %>%
##   summarise(full_age = median(age, na.rm = TRUE)) %>%
##   arrange(nganh)

## profs <- left_join(asoc_profs, full_profs)

## ggplot(data = profs, aes(asoc_age, full_age)) +
##   geom_point(color = "red") +
##   geom_text_repel(aes(label = nganh),
##                   max.iter = 4e3) +
##   labs(x = "Độ tuổi khi được phong PGS",
##        y = "Độ tuổi khi được phong GS",
##        title = "Độ tuổi bình quân khi được phong hàm",
##        caption = "Dữ liệu từ http://www.hdcdgsnn.gov.vn/") +
##   scale_x_continuous(breaks = unique(profs$asoc_age), label = unique(profs$asoc_age)) +
##   scale_y_continuous(breaks = unique(profs$full_age), label = unique(profs$full_age)) +
##   theme(panel.grid.minor.x = element_blank(),
##         panel.grid.minor.y = element_blank(),
##         plot.title = element_text(face = "bold"))

## ggsave("~/Documents/data_projects/prof-inflation/figures/profs-avg-age.svg",
##        width = 8, height = 5.5, dpi = 300)
