## A remake of bad charts
## 24/11/2016
## Source: http://vneconomy.vn/thi-truong/9-nhom-hang-xuat-khau-lon-nhat-da-dem-ve-hon-100-ty-usd-20161123014544186.htm


## packages
library(ggplot2)
library(svglite)


## data
vn_exports <- data.frame(
  goods = c("Điện thoại và LK", "Hàng dệt may", "Máy vi tính và LK",
            "Giày dép", "Máy móc, thiết bị", "Hàng thủy sản",
             "Gỗ và sản phẩm gỗ", "PT vận tải, phụ tùng",
            "Cà phê", "Túi xách, ví, vali, mũ"),
  value = c(19.7, 13.7, 10.3, 7.3, 5.8, 4.0, 3.9, 3.4, 1.9, 1.8),
  stringsAsFactors = FALSE
)

vn_exports$perct <- paste0(vn_exports$value, "%")
vn_exports$abs_value <- vn_exports$value * 144 / 100
vn_exports$gt10 <- vn_exports$abs_value > 10
vn_exports$gt10_color <- ifelse(vn_exports$gt10 == FALSE, "ivory4", "steelblue")

png("~/Documents/data_projects/remake/export.png",
    width = 619, height = 401, res = 95)

par(mar = c(1, 7.5, 5, 2), bg = "gray95")
barplot(height = rev(vn_exports$value),
        names.arg = rev(vn_exports$goods),
        horiz = TRUE, las = 1,
        border = FALSE, cex.names = 0.8,
        col.axis = "gray20", axes = FALSE,
        col = rev(vn_exports$gt10_color))
abline(v = seq(3, 18, by = 3), col = "gray95", lty = "dotted")
axis(side = 3, line = -2,
     at = seq(0, 18, by = 3),
     labels = paste0(seq(0, 18, by = 3), "%"),
     lwd = 0, cex.axis = 0.8, col.axis = "gray20", padj = -2)
mtext(side = 3,
      text = "Các nhóm hàng xuất khẩu chủ lực của Việt Nam",
      line = 3.8, adj = 0, font = 2)
mtext(side = 3,
      text = "10 nhóm hàng lớn đóng góp 100 trên 144 tỷ USD kim ngạch xuất khẩu",
      line = 2.8, cex = 0.8, col = "gray20", adj = 0, font = 3)
mtext(side = 3,
      text = "% tổng giá trị xuất khẩu",
      line = 1, col = "gray20", cex = 0.8)
mtext(side = 1,
      text = "Dữ liệu tổng hợp 10 tháng đầu năm 2016, Tổng cục Hải quan",
      line = 0, cex = 0.8, col = "gray20", adj = 1)
text(x = 13.5, y = 7.5,
     labels = "Các nhóm hàng >10 tỷ USD, vẫn là các ngành\nlắp ráp và thâm dụng lao động",
     cex = 0.8, col = "steelblue")

dev.off()


## ggplot(data = vn_exports, aes(reorder(goods, value), value, fill = gt10)) +
##   geom_bar(stat = "identity", width = 0.75) +
##   geom_text(aes(label = perct), hjust = 1.3, size = 3.5, color = "white") +
##   annotate("text", x = "Túi xách, ví, vali, mũ", y = 4.8, size = 3.5,
##            color = "gray35",
##            label = "(% tổng giá trị xuất khẩu)") +
##   annotate("text", x = "Giày dép", y = 14, size = 3.5,
##            color = "steelblue", fontface = 2, 
##            label = "Các nhóm hàng >10 tỷ USD, vẫn là các ngành\nlắp ráp và thâm dụng lao động") +
##   scale_fill_manual(values = c("gray55", "steelblue")) +
##   labs(x = NULL, y = NULL,
##        title = "Các nhóm hàng xuất khẩu chủ lực của Việt Nam",
##        subtitle = "10 nhóm hàng lớn đóng góp 100/144 tỷ USD kim ngạch xuất khẩu",
##        caption = "Dữ liệu tổng hợp 10 tháng đầu năm 2016, Tổng cục Hải quan") +
##   coord_flip() +
##   theme_minimal() +
##   theme(panel.grid = element_blank(),
##         axis.title.x = element_text(color = "gray35"),
##         axis.text.x = element_blank(),
##         axis.text.y = element_text(face = "bold", size = 10,
##                                    color = rev(vn_exports$gt10_color)),
##         plot.title = element_text(hjust = 0.1),
##         plot.subtitle = element_text(hjust = 0.1, color = "gray35"),
##         plot.caption = element_text(color = "gray35"),
##         legend.position = "none")

## ggsave("~/Documents/data_projects/remake/vietname-export.png",
##        width = 8, height = 6, dpi = 120)
