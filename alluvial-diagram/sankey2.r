


## -----------------------------------------------------------------------------
## Load packages and import data
## -----------------------------------------------------------------------------

require(dplyr)
require(tidyr)
require(colorspace)

vanban <- readxl::read_xlsx(path = "slvb.xlsx", n_max = 12) # keep the first 12 rows
vanban$`Số lượng` <- NULL

## -----------------------------------------------------------------------------
## Data transformation
## -----------------------------------------------------------------------------


## data in long format is suitable for plotting
vanban <- vanban %>%
    pivot_longer(cols = `Văn phòng chính phủ`:`Bộ Y tế`,
                 names_to = "ministry",
                 values_to = "count") %>%
    rename(type = `Loại văn bản`) %>%
    filter(!is.na(count))

## proper labels
vanban$ministry[vanban$ministry != "Bộ KHĐT"] <- tools::toTitleCase(vanban$ministry[vanban$ministry != "Bộ KHĐT"])

vanban <- vanban %>%
    mutate(ministry = case_when(
               ministry == "Văn Phòng Chính Phủ" ~ "VP Chính Phủ",
               TRUE ~ ministry
           )) %>%
    mutate(type = case_when(
               type == "Sản xuất - Kinh doanh - Xuất nhập khẩu" ~ "Sản xuất - Kinh doanh -\nXuất nhập khẩu",
               type == "Giáo dục - Văn hóa - Xã hội - Du lịch" ~ "Giáo dục - Văn hóa &\nXã hội - Du lịch",
               type == "Đất đai - Xây dựng - Bất động sản" ~ "Đất đai - Xây dựng -\nBất động sản",
               type == "Doanh nghiệp - Đầu tư" ~ "Doanh nghiệp -\nĐầu tư",
               type == "Hành chính - Tư pháp - Hình sự" ~ "Hành chính - Tư pháp",
               TRUE ~ type
           ))



vanban <- vanban %>%
    group_by(type) %>%
    mutate(sum_type = sum(count)) %>%
    ungroup()

vanban <- vanban %>%
    group_by(ministry) %>%
    mutate(sum_ministry = sum(count)) %>%
    ungroup()

vanban <- vanban %>%
    mutate(ministry = paste0(ministry, " (", sum_ministry, ")"),
           type = paste0(type, " (", sum_type, ")"))

## -----------------------------------------------------------------------------
## Plotting
## -----------------------------------------------------------------------------

alluvial_colors <- qualitative_hcl(n = NROW(unique(vanban$ministry)))

png(filename = "sankey2.png", width = 11.114583, height = 9.765967, units = "in", res = 210)
par(
    family = "Lato", # pretty font
    cex = 1.15,      # bigger font
    cex.axis = 1.15,
    xpd = TRUE # no clipping
)
alluvial::alluvial(
              select(vanban, ministry, type),
              freq = vanban$count,
              gap.width = 0.15,
              blocks = FALSE,
              axis_labels = c("Cơ quan ban hành", "Loại văn bản"),
              border = "white",
              cw = 0.2,
              xw = 0.1,
              col = ifelse(vanban$ministry == "VP Chính Phủ (23)", alluvial_colors[1],
                    ifelse(vanban$ministry == "Bộ KHĐT (22)", alluvial_colors[2],
                    ifelse(vanban$ministry == "Bộ Lao Động (10)", alluvial_colors[3],
                    ifelse(vanban$ministry == "Bộ Tài Chính (19)", alluvial_colors[4],
                    ifelse(vanban$ministry == "Bộ Tài Nguyên (20)", alluvial_colors[5],
                    ifelse(vanban$ministry == "Bộ Y Tế (18)", alluvial_colors[6],
                    ifelse(vanban$ministry == "Bộ Giáo Dục (20)", alluvial_colors[7],
                    ifelse(vanban$ministry == "Bộ Giao Thông (5)", alluvial_colors[8],
                    ifelse(vanban$ministry == "Bộ Tư Pháp (3)", alluvial_colors[9],
                    ifelse(vanban$ministry == "Bộ Quốc Phòng (2)", alluvial_colors[10], "gray90"))))))))))
          )
dev.off()

## dev.size()
## [1] 11.114583  9.765967
