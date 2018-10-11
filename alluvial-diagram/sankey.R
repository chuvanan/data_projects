

library(dplyr)
library(plotly)
library(ggalluvial)
library(ggrepel)
library(patchwork)

dta <- readr::read_csv("ex_im_type.csv")

dta <- dta %>%
    mutate(country = case_when(
               country == "Hàn quốc" ~ "Hàn Quốc",
               country == "Khac" ~ "Khác",
               country == "Nhật bản" ~ "Nhật Bản",
               TRUE ~ country
           ))

dta <- dta %>%
    mutate(type = RTA::capwords(type, strict = TRUE)) %>%
    filter(type == "Nhập Khẩu" & country %in% c("Trung Quốc", "Hàn Quốc", "Nhật Bản", "Đài Loan", "ASEAN") |
           type == "Xuất Khẩu" & country %in% c("Hoa Kỳ", "EU", "Trung Quốc", "Hàn Quốc", "ASEAN"))

dta <- dta %>%
    mutate(commodity = if_else(commodity == "Máy vi tính, sản phẩm điện tử và linh kiện",
                               "Máy vi tính,\nsản phẩm điện tử và linh kiện", commodity)) %>%
    mutate(commodity = if_else(commodity == "Giày dep",
                               "Giày dép", commodity))

dta <- dta %>%
    group_by(type) %>%
    mutate(tot_amount_by_type = sum(amount)) %>%
    mutate(type_lb = paste0(type, "\n", "$", scales::comma(tot_amount_by_type))) %>%
    ungroup()

dta <- dta %>%
    group_by(type, commodity) %>%
    mutate(tot_amount_by_commodity = sum(amount)) %>%
    mutate(commodity_lb = paste0(commodity, "\n", "$", scales::comma(tot_amount_by_commodity))) %>%
    ungroup()

dta <- dta %>%
    group_by(type, country) %>%
    mutate(tot_amount_by_country = sum(amount)) %>%
    mutate(country_lb = paste0(country, "\n", "$", scales::comma(tot_amount_by_country))) %>%
    ungroup()


ggplot(dta, aes(axis1 = country_lb, axis2 = commodity_lb, axis3 = type_lb, y = amount)) +
    geom_flow(stat = "alluvium", lode.guidance = "rightleft",
              aes(fill = commodity), width = 1/12, show.legend = FALSE,
              color = "white", alpha = 0.6) +
    geom_stratum(width = 1/12, color = "gray80", alpha = 0.01, fill = "white") +
    geom_text(stat = "stratum", label.strata = TRUE, nudge_x = -0.1, size = 4) +
    scale_x_discrete(expand = c(0.03, 0.1)) +
    scale_fill_manual(values = c("#6c4d0d",
                                 "#00b4f8",
                                 "#f8751f",
                                 "#962a3f",
                                 "#f1bd7a",
                                 "#ca0720",
                                 "#ff8ba3")) +
    labs(x = NULL, y = NULL,
         title = "Kim Ngạch Xuất Nhập Khẩu Của Việt Nam Với Nhóm 5 Đối Tác Chính",
         subtitle = "Dữ liệu Niên Giám Thống Kê Hải Quan, năm 2017, đơn vị: triệu USD") +
    theme_void(base_family = "Roboto") +
    facet_wrap( ~ type) +
    theme(panel.grid = element_blank(),
          strip.text = element_blank(),
          plot.title = element_text(size = 18, hjust = 0.16, face = "bold", vjust = 1),
          plot.subtitle = element_text(size = 13, hjust = 0.12))

ggsave(filename = "~/Downloads/bigdata-r/trade-chart_1.png")
## Saving 13.4 x 8.69 in image


p1 <- ggplot(data = filter(dta, type == "Nhập Khẩu"),
             aes(axis1 = country_lb, axis2 = commodity_lb, axis3 = type_lb, y = amount)) +
    geom_flow(stat = "alluvium", lode.guidance = "rightleft",
              aes(fill = commodity), width = 1/12, show.legend = FALSE,
              color = "white", alpha = 0.6) +
    geom_stratum(width = 1/12, color = "gray80", alpha = 0.01, fill = "white") +
    geom_text(stat = "stratum", label.strata = TRUE, nudge_x = -0.1, size = 4) +
    scale_x_discrete(expand = c(0.03, 0.1)) +
    scale_fill_manual(values = c("#6c4d0d",
                                 "#00b4f8",
                                 "#f8751f",
                                 "#962a3f",
                                 "#f1bd7a",
                                 "#ca0720",
                                 "#ff8ba3")) +
    labs(x = NULL, y = NULL,
         title = "Kim Ngạch XNK Của Việt Nam Với Nhóm 5 Đối Tác Chính",
         subtitle = "Dữ liệu Niên Giám Thống Kê Hải Quan, năm 2017, đơn vị: triệu USD") +
    theme_void(base_family = "Roboto") +
    theme(panel.grid = element_blank(),
          strip.text = element_blank(),
          plot.title = element_text(size = 18, hjust = 0.3, face = "bold", vjust = 1),
          plot.subtitle = element_text(size = 13, hjust = 0.2))

p2 <- ggplot(data = filter(dta, type == "Xuất Khẩu"),
             aes(axis1 = type_lb, axis2 = commodity_lb, axis3 = country_lb, y = amount)) +
    geom_flow(stat = "alluvium", lode.guidance = "rightleft",
              aes(fill = commodity), width = 1/12, show.legend = FALSE,
              color = "white", alpha = 0.6) +
    geom_stratum(width = 1/12, color = "gray80", alpha = 0.01, fill = "white") +
    geom_text(stat = "stratum", label.strata = TRUE, nudge_x = -0.1, size = 4) +
    scale_x_discrete(expand = c(0.03, 0.1)) +
    scale_fill_manual(values = c("#ff8ba3",
                                 "#6c4d0d",
                                 "#ca0720",
                                 "#00b4f8",
                                 "#f8751f",
                                 "#962a3f",
                                 "#f1bd7a")) +
    labs(x = NULL, y = NULL) +
    theme_void(base_family = "Roboto") +
    theme(panel.grid = element_blank(),
          strip.text = element_blank(),
          plot.title = element_text(size = 18, hjust = 0.16, face = "bold", vjust = 1),
          plot.subtitle = element_text(size = 13, hjust = 0.12))

p1 | p2

ggsave(filename = "~/Downloads/bigdata-r/trade-chart_2.png")

## -----------------------------------------------------------------------------
## demo

wesanderson::wes_palette("BottleRocket1")

library(networkD3)

## Load energy projection data
## Load energy projection data
URL <- paste0(
    "https://cdn.rawgit.com/christophergandrud/networkD3/",
    "master/JSONdata/energy.json")
Energy <- jsonlite::fromJSON(URL)
                                        # Plot
sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)
