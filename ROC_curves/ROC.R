

library(tidyr)
library(dplyr)
library(ggplot2)

library(gganimate)  # for animation
library(magick)     # to put animations sicde by side

## Simulate ROC curve


s_mean <- 2  # signal mean
s_sd <- 1.1   # signal standard deviation

x <- seq(-5,5,by=0.01) # range of signal
signal <- rnorm(100000,s_mean,s_sd)
noise <- rnorm(100000,0,1)


PX_n <- 1 - pnorm(x, mean = 0, sd = 1) # P(X > c | noise only) = False alarm rate
PX_sn <- 1 - pnorm(x, mean = s_mean, sd = s_sd) # P(X > c | signal plus noise) = Hit rate


threshold <- data.frame(val = seq(from = .5, to = s_mean, by = .2))


dist <- data.frame(signal = signal, noise = noise) %>%
    gather(data, value) %>%
    ggplot(aes(x = value, fill = data)) +
    geom_density(trim = TRUE, alpha = .5) +
    ggtitle("Conditional Distributions") +
    xlab("observed signal")  +
    scale_fill_manual(values = c("pink", "blue"))


p1 <- dist +
    geom_vline(data = threshold, xintercept = threshold$val, color = "red") +
    transition_manual(threshold$val)

p1 <- animate(p1)

df2 <- data.frame(x, PX_n, PX_sn)
roc <- ggplot(df2) +
    xlab("P(X | n)") + ylab("P(X | sn)") +
    geom_line(aes(PX_n, PX_sn)) +
    geom_abline(slope = 1) +
    ggtitle("ROC Curve") +
    coord_equal()

q1 <- roc +
    geom_point(data = threshold, aes(1-pnorm(val),
                                     1- pnorm(val, mean = s_mean, sd = s_sd)),
               color = "red") +
    transition_manual(val)

q1 <- animate(q1)
