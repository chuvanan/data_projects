

library(data.table)
library(ggplot2)
library(ggrepel)



gender <- fread("~/Documents/data_projects/world_gender/Data.csv", header = TRUE)

setnames(gender,
         old = c("Country.Name", "Country.Code", "Indicator.Name",
                 "Indicator.Code", 1960:2016),
         new = c("country_lb", "country_id", "indicator_lb", "indicator_id",
                 paste0("y", 1960:2016)))

vietnam <- gender[country_id == "VNM"]

vietnam[indicator_id %in% c("SH.STA.OB18.FE.ZS", "SH.STA.OB18.MA.ZS")]

obesity <- vietnam[indicator_id %in% c("SH.STA.OB18.FE.ZS", "SH.STA.OB18.MA.ZS"),
                   list(indicator_lb, y2010, y2014)]

obesity <- melt(obesity,
                id.vars = c("indicator_lb"),
                measure.var = c("y2010", "y2014"))

ggplot(obesity, aes(variable, value, group = indicator_lb, color = indicator_lb)) +
  geom_point(size = 1.8) +
  geom_line(size = 1) +
  geom_text(aes(label = paste0(value, "%")), vjust = 1.9) +
  ## geom_hline(yintercept = 0, color = "gray90", size = 0.5) +
  annotate("text", x = "y2010", y = 1.7, label = "Nam", color = "red") +
  annotate("text", x = "y2010", y = 3.8, label = "Nữ", color = "blue") +
  labs(x = NULL, y = NULL,
       title = "Người Việt đang béo lên",
       subtitle = "Tỷ lệ béo phì được tính cho dân số trên 18 tuổi",
       caption = "Nguồn dữ liệu: WB") +
  scale_y_continuous(limits = c(0, 5)) +
  scale_x_discrete(labels = c("2010", "2014")) +
  scale_color_manual(values = c("blue", "red")) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.3),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(color = "gray45", size = 11),
        plot.caption = element_text(color = "gray45", hjust = 0),
        panel.border = element_rect(color = "gray60"))

ggsave("viet_obesity.png", width=4, height=4)

overweight <- gender[indicator_id %in% c("SH.STA.OB18.FE.ZS", "SH.STA.OB18.MA.ZS"),
                     list(country_id, country_lb, indicator_lb, y2010, y2014)]

overweight <- overweight[country_id %in% c("VNM", "CHN", "SGP", "THA",
                                           "IDN", "LAO", "PHL", "MYS",
                                           "BRN", "MMR")]

overweight[grepl("\\sfemale\\s", indicator_lb), c("indicator_lb") := "Nữ"]
overweight[grepl("\\smale\\s", indicator_lb), c("indicator_lb") := "Nam"]


ggplot(overweight, aes(y2010, y2014, group = indicator_lb)) +
  geom_point(aes(color = indicator_lb)) +
  geom_abline(slope = 1, intercept = 0, color = "gray90") +
  geom_text_repel(aes(label = country_lb)) +
  labs(x = "2010", y = "2014") +
  scale_color_manual(values = c("blue", "red")) +
  facet_grid(. ~ indicator_lb) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_equal()

ggsave("asis_obesity.png", width=6, height=4)
