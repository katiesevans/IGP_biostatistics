# lecture 04 - binomial distributions (and normal)
library(tidyverse)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# plot mutant cat distribution
cats <- data.frame(mutants = c(0,1,2,3,4,5)) %>%
    dplyr::mutate(prob = dbinom(mutants, 5, 0.37))

cats %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = mutants, y = prob) +
    ggplot2::geom_bar(stat = "identity", fill = "palevioletred3") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Number of mutants", y = "Probability")
ggsave("mutant_cat_binom.png", height = 3, width = 5)


# create a pretend "unlimited binomial distribution" to approx normal distribution
cats2 <- data.frame(mutants = seq(0,200)) %>%
    dplyr::mutate(prob = dbinom(mutants, 200, 0.37))
cats2 %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = mutants, y = prob) +
    ggplot2::geom_bar(stat = "identity", fill = "palevioletred3", color = "palevioletred3") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Number of mutants", y = "Probability")
ggsave("mutant_cat_binom2.png", height = 3, width = 5)


###################
# The normal distribution

# plot normal distribution of interspike times in nerve cells
p1 <- data.frame(x = c(14.3,17)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 101, 
                           args = list(mean = 15.6, sd = 0.4)) + 
    ggplot2::labs(y = "", x = "Interspike-time intervals (ms)") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24)
ggsave(p1, filename = "interspike_nerves1.png", height = 5, width = 10)

# add line for mean and sd
p2 <- p1 +
    ggplot2::geom_vline(xintercept = 15.6, color = "red", size = 2) +
    ggplot2::geom_vline(xintercept = c(15.6+0.4, 15.6-0.4), color = "blue", size = 1.5, linetype = 2)
ggsave(p2, filename = "interspike_nerves2.png", height = 5, width = 10)

# try multiple different functions
data.frame(x = c(10,20)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 15.6, sd = 0.4),
                           color = "purple", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 15.6, sd = 1.5),
                           color = "forestgreen", size = 1.5) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 12.9, sd = 0.8),
                           color = "blue", size = 1.5) +
    ggplot2::labs(y = "", x = "Interspike-time intervals (ms)") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24)
ggsave("interspike_nerves3.png", height = 5, width = 10)

# standardized normal distribution
data.frame(x = c(-3,3)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
    ggplot2::theme_bw(24)
ggsave("standard_normal.png", height = 5, width = 10)


# plot example area under the curve
data.frame(x = c(-3,3)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(-3, 3),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
    ggplot2::theme_bw(24)
ggsave("standard_normal3.png", height = 5, width = 10)

# and again - with z = 1.53
data.frame(x = c(-3,3)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(-3, 1.53),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
    ggplot2::theme_bw(24)
ggsave("standard_normal2.png", height = 5, width = 10)

# highlight other side of the curve
data.frame(x = c(-3,3)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(1.53, 3),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
    ggplot2::theme_bw(24)
ggsave("standard_normal4.png", height = 5, width = 10)

data.frame(x = c(-3,3)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(-1.53, 0.86),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
    ggplot2::theme_bw(24)
ggsave("standard_normal5.png", height = 5, width = 10)
pnorm(0.86) - pnorm(-1.53)
