# lecture08 script
library(tidyverse)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##############
# t test in R
##############
# set random seed
set.seed(76)

# make populations for butterfly
y1 <- rnorm(14, 32, 2.5)
y2 <- rnorm(12, 28, 3.4)

# calculate t test with t.test
t.test(y1, y2)

##############
# Power calculations
##############

data.frame(x = c(-5,4.5)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = -1.5, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 1, sd = 1),
                           color = "violetred3", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = -1.5, sd = 1),
                           xlim = c(0.25, 4),
                           alpha = 0.5,
                           fill = "grey50", size = 1.5) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 1, sd = 1),
                           xlim = c(-4, 0.25),
                           alpha = 0.3,
                           fill = "violetred3", size = 1.5)
ggsave("power_plot1.png", height = 4, width = 7)

data.frame(x = c(-5,4.5)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = -1.5, sd = 1),
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
    # ggplot2::stat_function(fun = dnorm,
    #                        n = 100,
    #                        geom = "area",
    #                        args = list(mean = -1.5, sd = 1),
    #                        xlim = c(0.25, 4),
    #                        alpha = 0.5,
    #                        fill = "grey50", size = 1.5) +
    # ggplot2::stat_function(fun = dnorm,
    #                        n = 100,
    #                        geom = "area",
    #                        args = list(mean = 1, sd = 1),
    #                        xlim = c(-4, 0.25),
    #                        alpha = 0.3,
    #                        fill = "violetred3", size = 1.5)
ggsave("power_plot2.png", height = 4, width = 7)

data.frame(x = c(-5,4.5)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = -1.5, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 1, sd = 1),
                           color = "violetred3", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
ggplot2::stat_function(fun = dnorm,
                       n = 100,
                       geom = "area",
                       args = list(mean = -1.5, sd = 1),
                       xlim = c(0.25, 4),
                       alpha = 0.5,
                       fill = "grey50", size = 1.5)
# ggplot2::stat_function(fun = dnorm,
#                        n = 100,
#                        geom = "area",
#                        args = list(mean = 1, sd = 1),
#                        xlim = c(-4, 0.25),
#                        alpha = 0.3,
#                        fill = "violetred3", size = 1.5)
ggsave("power_plot3.png", height = 4, width = 7)

# plot power
data.frame(x = c(-5,4.5)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = -1.5, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 1, sd = 1),
                           color = "violetred3", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = -1.5, sd = 1),
                           xlim = c(0.25, 4),
                           alpha = 0.5,
                           fill = "grey50", size = 1.5) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 1, sd = 1),
                           xlim = c(0.25, 4),
                           alpha = 0.3,
                           fill = "skyblue", size = 1.5)
ggsave("power_plot7.png", height = 4, width = 7)

# plot power, alpha, beta
data.frame(x = c(-5,4.5)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = -1.5, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 1, sd = 1),
                           color = "violetred3", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = -1.5, sd = 1),
                           xlim = c(0.25, 4),
                           alpha = 0.5,
                           fill = "grey50", size = 1.5) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 1, sd = 1),
                           xlim = c(0.25, 4),
                           alpha = 0.3,
                           fill = "skyblue", size = 1.5) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 1, sd = 1),
                           xlim = c(-4, 0.25),
                           alpha = 0.3,
                           fill = "violetred3", size = 1.5)
ggsave("power_plot8.png", height = 4, width = 7)

# change alpha

data.frame(x = c(-5,4.5)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = -1.5, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 1, sd = 1),
                           color = "violetred3", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = -1.5, sd = 1),
                           xlim = c(0.75, 4),
                           alpha = 0.5,
                           fill = "grey50", size = 1.5)+
    ggplot2::stat_function(fun = dnorm,
                       n = 100,
                       geom = "area",
                       args = list(mean = 1, sd = 1),
                       xlim = c(-4, 0.75),
                       alpha = 0.3,
                       fill = "violetred3", size = 1.5)
ggsave("power_plot4.png", height = 4, width = 7)

# increase sd
data.frame(x = c(-7,7)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = -1.5, sd = 2),
                           color = "grey50", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 1, sd = 2),
                           color = "violetred3", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = -1.5, sd = 2),
                           xlim = c(0.75, 7),
                           alpha = 0.5,
                           fill = "grey50", size = 1.5)+
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 1, sd = 2),
                           xlim = c(-7, 0.75),
                           alpha = 0.3,
                           fill = "violetred3", size = 1.5)
ggsave("power_plot5.png", height = 4, width = 7)

# increase diff in means
data.frame(x = c(-5,4.5)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = -2.5, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 2, sd = 1),
                           color = "violetred3", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = -2.5, sd = 1),
                           xlim = c(0.25, 4),
                           alpha = 0.5,
                           fill = "grey50", size = 1.5) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 2, sd = 1),
                           xlim = c(-4, 0.25),
                           alpha = 0.3,
                           fill = "violetred3", size = 1.5)
ggsave("power_plot6.png", height = 4, width = 7)

# power t test - has 5 params, you supply 4 and it gives the 5th
power.t.test(n = 20, delta = 2, sd = 1, sig.level = 0.05)

# lets test power across a variety of n with fixed delta and sd
power <- data.frame(delta = 2, sd = 1, alpha = 0.05, n = c(3,5,10,15,20)) %>%
    dplyr::mutate(power = power.t.test(n = n, delta = delta, sd = sd, sig.level = alpha)[[5]])

power %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = n, y = power) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Sample size, n", y = "Power") +
    ggplot2::geom_hline(yintercept = 0.8, color = "red", linetype = 2, size = 1.5)
ggsave("power-by-n.png", height = 4, width = 7)

# lets test power across a variety of n and eff size with fixed delta and sd
power <- data.frame(delta = "0.25, 0.5, 1, 1.5, 2", sd = 1, alpha = 0.05, n = c(3,5,10,15,20)) %>%
    tidyr::separate_rows(delta, sep = ",", convert = T) %>%
    dplyr::mutate(power = power.t.test(n = n, delta = delta, sd = sd, sig.level = alpha)[[5]])

power %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = delta, y = power, color = factor(n), group = n) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_bw(24) +
    ggplot2::scale_color_manual(values = c("3" = "yellow", "5" = "orange", "10" = "darkgreen",
                                           "15" = "blue", "20" = "purple"),
                                name = "Sample size, n") +
    ggplot2::labs(x = "Difference in means", y = "Power") +
    ggplot2::geom_hline(yintercept = 0.8, color = "red", linetype = 2, size = 1.5)
ggsave("power-by-n2.png", height = 4, width = 10)

# paired test - cAMP
df <- data.frame(control = c(6.01, 2.28, 1.51, 2.12),
                 progesterone = c(5.23, 1.21, 1.40, 1.38)) %>%
    dplyr::mutate(diff = control - progesterone)
t.test(df$control, df$progesterone, paired = T)
t.test(df$diff)
t.test(df$control, df$progesterone, mu = 1, paired = T)

