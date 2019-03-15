

library(ggplot2)
library(ggrepel)
library(hrbrthemes)
load("../data/cleaned-baaa-aircraft-accidents.RData")

big_font <- function() theme(text = element_text(size = 18))


## In term of number of accidents, how Boeing 737 MAX compare to other types?
boeing <- accidents[grepl("Boeing", accidents$AC_Type), ]

names(boeing)
nrow(boeing)                            # 1269
length(unique(boeing$AC_Type))          # 44
unique(boeing$AC_Type)

## categorize Boeing's Ac types into two major groups: commercial vs other
## (military, mail, ...)
## https://en.wikipedia.org/wiki/Boeing_Commercial_Airplanes
## https://airfactsjournal.com/2019/03/can-boeing-trust-pilots/

boeing$AC_Type[!startsWith(boeing$AC_Type, "Boeing 7")] <- "Other"
boeing <- boeing[boeing$AC_Type != "Other", ]

accidents_by_ac_type <- data.frame(table(boeing$AC_Type, dnn = c("AC_Type")))
accidents_by_ac_type$AC_Type <- forcats::fct_reorder(accidents_by_ac_type$AC_Type,
                                                     accidents_by_ac_type$Freq)
accidents_by_ac_type$is_MAX <- FALSE
accidents_by_ac_type$is_MAX[accidents_by_ac_type$AC_Type == "Boeing 737 MAX 8"] <- TRUE

accidents_by_ac_type_chart <- ggplot(accidents_by_ac_type,
                                     aes(AC_Type, Freq, fill = is_MAX)) +
    geom_col() +
    labs(x = NULL, y = "Number of Accidents",
         title = "Count of Boeing's Commercial Aircraft Accidents") +
    scale_fill_manual(values = c("gray", "red4"), guide = FALSE) +
    coord_flip() +
    theme_ipsum_rc(base_size = 13, axis_title_size = 14)

## ggsave(filename = "../figures/accidents_by_ac_type.png", accidents_by_ac_type_chart)

## Did more accidents happen recently?
range(boeing$Date)                      # from 1959
boeing <- transform(boeing, Year = as.integer(format(Date, "%Y")))
accidents_by_year <- data.frame(table(boeing$Year, dnn = "Year"))
accidents_by_year$Year <- as.numeric(as.character(accidents_by_year$Year))

ggplot(accidents_by_year,
       aes(Year, Freq)) +
    geom_point() +
    geom_smooth() +
    big_font()

## More fatalities in recent years?
fatalities_by_year <- aggregate(Fatalities ~ Year, data = boeing, sum)

ggplot(fatalities_by_year,
       aes(Year, Fatalities)) +
    geom_point() +
    geom_smooth() +
    big_font()

accidents_vs_fatalities <- merge(accidents_by_year, fatalities_by_year)

accidents_vs_fatalities$Decade <- ifelse(accidents_vs_fatalities$Year < 1970, "60s",
                                  ifelse(accidents_vs_fatalities$Year >= 1970 &
                                         accidents_vs_fatalities$Year < 1980, "70s",
                                  ifelse(accidents_vs_fatalities$Year >= 1980 &
                                         accidents_vs_fatalities$Year < 1990, "80s",
                                  ifelse(accidents_vs_fatalities$Year >= 1990 &
                                         accidents_vs_fatalities$Year < 2000, "90s",
                                  ifelse(accidents_vs_fatalities$Year >= 2000 &
                                         accidents_vs_fatalities$Year < 2010, "00s",
                                  ifelse(accidents_vs_fatalities$Year >= 2010, "10s", NA))))))

accidents_vs_fatalities$Decade <- factor(accidents_vs_fatalities$Decade,
                                         levels = c("60s", "70s", "80s", "90s", "00s", "10s"),
                                         ordered = TRUE)

accidents_vs_fatalities_chart <- ggplot(accidents_vs_fatalities,
                                        aes(Freq, Fatalities, label = Year)) +
    geom_smooth(method = "lm", se = FALSE, color = "gray", linetype = 2) +
    geom_point() +
    geom_text_repel(aes(color = Decade), size = 5.5) +
    labs(x = "Number of accidents",
         title = "Boeing's Aircraft Accidents/Fatalities") +
    scale_color_ipsum(guide = FALSE) +
    theme_ipsum_rc(axis_text_size = 13,
                   axis_title_size = 14,
                   strip_text_face = "bold") +
    facet_wrap( ~ Decade)

## ggsave(filename = "../figures/accidents-vs-fatalities.png", accidents_vs_fatalities_chart)
