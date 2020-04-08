

require(here)
require(data.table)
require(ggplot2)

theme_set(
    theme_minimal(
        base_size = getOption("base_size"),
        base_family = "Roboto Condensed"
    ) +
    theme(
        panel.grid = element_line(size = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    )
)


mobility <- fread(here("data", "google_mobility.tsv"))
mobility[, `:=`(report_date = as.Date(report_date),
                date = as.Date(date))]
mobility[category == "Workplace", `:=`(category = "Workplaces")]

countries <- mobility[type == "country"]
vietnam <- countries[country_code == "VN"]

ggplot(vietnam) +
    geom_line(aes(date, trend), color = "goldenrod") +
    geom_ribbon(aes(date, ymin =0, ymax = trend), fill = "goldenrod", alpha = 0.3) +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(limits = c(-0.5, 0.5), labels = scales::percent) +
    facet_wrap( ~ category)
ggsave(filename = here("figures", "mobility.png"))
