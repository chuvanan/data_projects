
library(tidyr)
library(dplyr)
library(ggplot2)

profs <- read.csv("~/ownCloud/data_projects/prof-inflation/cleaned-profs.csv",
                  stringsAsFactors = FALSE)

profs <- mutate(profs, age = nam - as.numeric(substr(ngaysinh, 1, 4)))

profs %>%
  group_by(nam) %>%
  summarise(n = n()) %>%
  ggplot(aes(nam, n)) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw()

profs %>%
  group_by(nam) %>%
  summarise(avg_age = median(age, na.rm = TRUE),
            min_age = min(age, na.rm = TRUE),
            max_age = max(age, na.rm = TRUE)) %>%
  ggplot(aes(nam, avg_age)) +
  geom_line(aes(nam, max_age), color = "gray90") +
  geom_line(aes(nam, min_age), color = "gray90") +
  geom_point(aes(nam, min_age), color = "gray60") +
  geom_point(aes(nam, max_age), color = "gray60") +
  geom_point(size = 2, color = "red4") +
  geom_label(aes(1984, 35), label = "youngest line", color = "gray60") +
  geom_label(aes(1987, 80), label = "oldest line", color = "gray60") +
  geom_label(aes(2005, 55), label = "average line", color = "red4") +
  scale_y_continuous(limits = c(min(profs$age, na.rm = TRUE),
                                max(profs$age, na.rm = TRUE))) +
  labs(x = NULL, y = "age when recieved title", title = "3X professors") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0, face = "bold"))

## ggsave("~/ownCloud/data_projects/prof-inflation/figures/3x-professors.png",
##        width = 8, height = 5, dpi = 100)

by_nganh <- profs %>%
  group_by(nam, nganh) %>%
  summarise(n = n()) %>%
  group_by(nganh) %>%
  mutate(cum_n = cumsum(n))


ggplot(aes(nam, cum_n, group = nganh), data = by_nganh) +
  geom_line(color = "gray60") +
  geom_line(aes(nam, cum_n), data = filter(by_nganh, nganh == "Khoa học quân sự"), color = "red") +
  geom_line(aes(nam, cum_n), data = filter(by_nganh, nganh == "Khoa học an ninh"), color = "green") +
  theme_bw()
