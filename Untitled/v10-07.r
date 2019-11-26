
## Load packages and import data -----------------------------------------------

require(dplyr)
require(tidyr)
require(ggplot2)
require(colorspace)
require(hrbrthemes)

dat <- readr::read_csv2("V10.07.csv", skip = 2)
dat <- filter(dat, !is.na(X2))

dat$X1 <- NULL
names(dat)[names(dat) == "X2"] <- "school_level"

dat$indicators <- c(
    rep("Number of female teachers", 3),
    rep("Number of female students", 3),
    rep("Growth rate of female teachers", 3),
    rep("Growth rate of female students", 3)
)

names(dat)[names(dat) == "Sơ bộ 2017-2018"] <- "2017-2018"

dat <- gather(dat, key = "year", value = "value", -c(school_level, indicators))
dat$value <- dat$value / 10

dat$year <- gsub(x = dat$year, pattern = "[0-9]{4}-", replacement = "")
dat$year <- as.numeric(dat$year)

## Helper functions ------------------------------------------------------------

bigger_font <- function() {
    theme_minimal(base_family = "Lato") +
        theme(text = element_text(size = 17))
}

## EDA -------------------------------------------------------------------------

dat %>%
    filter(grepl("Growth.*teachers", indicators)) %>%
    ggplot(aes(year, value - 100, color = school_level)) +
    geom_point(size = 3, alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(x = NULL, y = "Growth Rate(%)") +
    scale_color_discrete_qualitative(name = NULL, palette = "Dark 3") +
    theme_ipsum_rc(base_size = 16, axis_title_size = 16) +
    theme(legend.position = "top")
## ggsave(filename = "growth-rate.png")
