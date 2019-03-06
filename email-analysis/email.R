

library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)

email <- readxl::read_xlsx("email khcn_thudta_190228.xlsx")
names(email) <- tolower(names(email))

dim(email)
head(email)
tail(email)

## all emails are in valid format?
all(str_detect(email$email, "\\@"))

## get the domain
email$domain <- character(nrow(email))
email$domain <- unlist(str_extract_all(email$email, pattern = "\\@.*"))

unique(email$domain)
## over 2400 unique domains

bigger_font <- function() {
    theme(text = element_text(size = 15))
}

## what are the top 5 domains ------------------------------

top5_domains <- email %>%
    mutate(domain_fct = fct_lump(domain, n = 5)) %>%
    count(domain_fct, sort = TRUE) %>%
    mutate(pct = n / sum(n))

top5_domains %>%
    mutate(domain_fct = fct_reorder(domain_fct, n)) %>%
    ggplot(aes(domain_fct, pct)) +
    geom_col() +
    labs(x = NULL, y = NULL, title = "Top domains") +
    scale_y_continuous(label = scales::percent) +
    coord_flip() +
    bigger_font()

## ggsave(filename = "top5-domains.pdf")

## email length analysis ------------------------------
email$len <- numeric(nrow(email))
email$len <- nchar(str_replace_all(email$email, "\\@.*", ""))

email %>%
    mutate(len = as.factor(len)) %>%
    ggplot(aes(len)) +
    geom_bar() +
    labs(x = "email length in characters", y = "frequency") +
    bigger_font()
## ggsave(filename = "email-length.pdf")

## most used charactes ------------------------------

email$ename <- str_replace_all(email$email, "\\@.*", "")
email$has_number <- str_detect(email$ename, pattern = "[0-9]{1,}")

has_numbers <- email %>%
    count(has_number) %>%
    mutate(pct = n / sum(n))

ggplot(has_numbers, aes(has_number, pct)) +
    geom_col() +
    labs(x = "Has numeric characters in email?",
         y = "percentage") +
    scale_y_continuous(label = scales::percent) +
    coord_flip() +
    bigger_font()

## ggsave(filename = "has-numbers-in-email.pdf")

## Do emails that contain numbers often have more characters?
email %>%
    ggplot(aes(has_number, len)) +
    geom_boxplot() +
    labs(x = "Use numbers in email",
         y = "Email length in character") +
    bigger_font()

## ggsave(filename = "email-length-with-and-without-numbers.pdf")

## which vowel sounds are the most used?

email$sound <- strsplit(email$email, "")
email$sound <- lapply(email$sound, tolower)
email$sound <- lapply(email$sound, function(x) Filter(function(y) grepl("[a-z]", y), x))

count_sounds <- as.data.frame(table(unlist(email$sound)))
names(count_sounds)[1] <- c("Sounds")

count_sounds %>%
    mutate(Sounds = fct_rev(Sounds)) %>%
    ggplot(aes(Sounds, Freq)) +
    geom_col() +
    labs(title = "Not all characters are equal") +
    coord_flip() +
    bigger_font()

## ggsave(filename = "character-distribution.pdf")
