# lecture06 script
library(tidyverse)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# butterfly fake population distribution example
data.frame(x = c(15,50)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 32, sd = 2.5),
                           color = "orange", size = 1.5) + 
    ggplot2::geom_vline(xintercept = 32, color = "orange", linetype = 2, size = 1.5) +
    ggplot2::labs(y = "", x = "Wing area") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) 
ggsave("butterfly_distribution.png", height = 5, width = 7)

data.frame(x = c(15,50)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           args = list(mean = 28, sd = 3.4),
                           color = "grey50", size = 1.5) +
    ggplot2::geom_vline(xintercept = 28, color = "grey50", linetype = 2, size = 1.5) +
    ggplot2::labs(y = "", x = "Wing area") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24)
ggsave("butterfly_distribution2.png", height = 5, width = 7)

# calculate the SE between means 
s1=2.5
s2=3.4
n1=14
n2=12
sqrt((s1**2/n1) + (s2**2/n2))

p1 <- rnorm(14, 32, 2.5)
p2 <- rnorm(12, 28, 3.4)
t.test(p1, p2)


# show t on distribution
data.frame(x = c(-4,4)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    # ggplot2::stat_function(fun = dnorm,
    #                        n = 100,
    #                        geom = "area",
    #                        args = list(mean = 0, sd = 1),
    #                        xlim = c(-3, 3),
    #                        alpha = 0.7,
    #                        fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(320,380,440,500,560,620,680)) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank())
ggsave("t_dist1.png", height = 5, width = 7)

# with p values
data.frame(x = c(-4,4)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(-4, -1),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(1, 4),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank())
ggsave("t_dist2.png", height = 5, width = 7)

# with p values
data.frame(x = c(-4,4)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(-4, -2),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(2, 4),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank())
ggsave("t_dist3.png", height = 5, width = 7)

# show effect sizes very small between two populations
data.frame(x = c(-4,4)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0.5, sd = 1),
                           color = "violetred3", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank())
ggsave("effect_size1.png", height = 5, width = 7)


# show larger effect size
data.frame(x = c(-4,4)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = -0.5, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 1, sd = 1),
                           color = "violetred3", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank())
ggsave("effect_size2.png", height = 5, width = 7)

