

library(ggplot2)
library(scales)

car_tax <- data.frame(
  gia_co_so = rep(100, 8),
  dung_tich = c("<1.5", "1,5-2", "2-2,5", "2,5-3", "3-4", "4-5", "5-6", ">6"),
  thue_nhapkhau = rep(0.7, 8),
  thue_ttdb = c(0.4, 0.45, 0.5, 0.55, 0.9, 1.1, 1.3, 1.5),
  thue_gtgt = rep(0.1, 8),
  stringsAsFactors = FALSE
)

car_tax$gia_B <- car_tax$gia_co_so * car_tax$thue_nhapkhau
car_tax$gia_C <- (car_tax$gia_co_so + car_tax$gia_B) * car_tax$thue_ttdb
car_tax$gia_D <- (car_tax$gia_co_so + car_tax$gia_B + car_tax$gia_C) * car_tax$thue_gtgt

car_tax$gia_tong <- apply(car_tax[, c("gia_co_so", "gia_B", "gia_C", "gia_D")], 1, sum)

car_tax$tyle_giaA <- round(car_tax$gia_co_so * 100 / car_tax$gia_tong, 1)
car_tax$tyle_giaB <- round(car_tax$gia_B * 100 / car_tax$gia_tong, 1)
car_tax$tyle_giaC <- round(car_tax$gia_C * 100 / car_tax$gia_tong, 1)
car_tax$tyle_giaD <- round(car_tax$gia_D * 100 / car_tax$gia_tong, 1)


car_tax <- car_tax[, c("dung_tich", "tyle_giaA", "tyle_giaB", "tyle_giaC", "tyle_giaD")]

car_tax <- reshape(car_tax,
                   varying = c("tyle_giaA", "tyle_giaB", "tyle_giaC", "tyle_giaD"),
                   v.names = "tyle",
                   timevar = "thanhphan",
                   times = c("Giá xe nhập cảng", "Thuế nhập khẩu",
                             "Thuế TTĐB", "Thuế GTGT"),
                   direction = "long")

rownames(car_tax) <- NULL
car_tax$id <- NULL

## remake1

ggplot(car_tax, aes(dung_tich, tyle, fill = thanhphan)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = tyle),
            position = "stack",
            hjust = 1.8,
            size = 3.5,
            color = "gray30") +
  labs(x = "Dung tích động cơ (lít)", y = NULL,
       title = "Cấu trúc thuế trong giá xe bán ra tại Việt Nam") +
  scale_fill_brewer(breaks = c("Giá xe nhập cảng", "Thuế nhập khẩu",
                               "Thuế TTĐB", "Thuế GTGT"),
                    palette = "Paired", type = "qual",
                    name = "% giá bán") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  theme_minimal(base_family = "Calibri", base_size = 11) +
  theme(legend.title = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(r = -8)),
        plot.title = element_text(hjust = 0),
        legend.position = "bottom",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## remake2

ggplot(car_tax, aes(dung_tich, tyle, color = thanhphan)) +
  geom_point(size = 2.5) +
  labs(x = "Dung tích động cơ (lít)", y = NULL,
       title = "Cấu trúc thuế trong giá xe bán ra tại Việt Nam") +
  scale_color_brewer(breaks = c("Thuế GTGT", "Thuế nhập khẩu",
                                "Giá xe nhập cảng", "Thuế TTĐB"),
                     palette = "Set1", type = "qual") +
  scale_y_continuous(labels = c("", "10%", "20%", "30%", "40%", "50%", "")) +
  coord_flip() +
  theme_minimal(base_family = "Calibri", base_size = 11) +
  theme(legend.title = element_blank(),
        axis.text.y = element_text(margin = margin(r = -8), color = "gray30"),
        axis.text.x = element_text(color = "gray30", margin = margin(t = -20)),
        axis.title.y = element_text(color = "gray30"),
        plot.title = element_text(hjust = 0),
        legend.position = c(0.39, 0.98),
        legend.direction = "horizontal",
        legend.margin = unit(0, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank())
