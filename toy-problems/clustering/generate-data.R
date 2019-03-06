

## https://smorbieu.gitlab.io/generate-datasets-to-understand-some-clustering-algorithms-behavior/

library(mvtnorm)
library(ggplot2)
library(dplyr)

bigger_font <- function() {
    theme(text = element_text(size = 15))
}

gen_gauss_data <- function(n, center, sigma, label) {
    dta <- mvtnorm::rmvnorm(n, mean = center, sigma = sigma)
    dta <- data.frame(dta)
    names(dta) <- c("x", "y")
    dta$class <- factor(label)
    dta
}

dataset1 <- rbind(
    gen_gauss_data(500, c(5, 5), sigma = matrix(c(1, 0, 0, 1), nrow = 2), 1),
    gen_gauss_data(500, c(1, 1), sigma = matrix(c(1, 0, 0, 1), nrow = 2), 2)
)

dataset1$name <- "1 - Mixture of Gaussians"

ggplot(dataset1, aes(x)) + geom_histogram(color = "white")
ggplot(dataset1, aes(y)) + geom_histogram(color = "white")

mixture_gaussian_equal_size <- dataset1 %>%
    ggplot(aes(x, y, shape = class, color = class)) +
    geom_point(size = 3) +
    coord_fixed() +
    labs(title = "Mixture of Gaussians Equal Size") +
    scale_shape_manual(values = c(2, 3)) +
    bigger_font()

## ggsave(filename = "mixture_gaussian_equal_size.pdf", mixture_gaussian_equal_size)

dataset2 <- rbind(
    gen_gauss_data(2000, c(5, 5), sigma = matrix(c(3, 0, 0, 3), nrow = 2), 1),
    gen_gauss_data(50, c(1, 1), sigma = matrix(c(0.1, 0, 0, 0.1), nrow = 2), 2)
)

dataset2$name <- "2 - Different sizes"

mixture_gaussian_diff_size <- dataset2 %>%
    ggplot(aes(x, y, shape = class, color = class)) +
    geom_point(size = 3) +
    coord_fixed() +
    labs(title = "Mixture of Gaussians Different Size") +
    scale_shape_manual(values = c(2, 3)) +
    bigger_font()

## ggsave(filename = "mixture_gaussian_diff_size.pdf", mixture_gaussian_diff_size)
