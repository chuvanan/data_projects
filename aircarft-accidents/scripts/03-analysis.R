

library(ggplot2)
library(ggrepel)
load("../data/cleaned-baaa-aircraft-accidents.RData")

big_font <- function() theme(text = element_text(size = 16))


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

ggplot(accidents_by_ac_type,
       aes(AC_Type, Freq)) +
    geom_col() +
    labs(x = NULL, y = "Number of Accidents") +
    coord_flip() +
    big_font()


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

ggplot(accidents_vs_fatalities,
       aes(Freq, Fatalities, label = Year)) +
    ## geom_smooth(method = "lm", se = FALSE, linetype = 2) +
    geom_point() +
    geom_line(aes(group = Decade, color = Decade)) +
    geom_text_repel(aes(color = Decade), size = 5) +
    scale_color_brewer(type = "qual") +
    big_font() +
    facet_wrap( ~ Decade)
