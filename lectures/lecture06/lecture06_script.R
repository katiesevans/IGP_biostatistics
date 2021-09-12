# lecture06 script
library(tidyverse)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# butterfly area example
butterfly <- c(33.9, 33, 30.6, 36.6, 36.5, 34, 36.1, 32, 28, 32, 32.2, 32.2, 32.3, 30)

# plot distribution
data.frame(butterfly) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = butterfly) +
    ggplot2::geom_histogram(fill = "darkorchid4") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Wing area (cm^2)", y = "Frequency")
ggsave("butterfly_distribution.png", height = 5, width = 7)

# plot distribution with SE and SD
sample_mean <- mean(butterfly)
sample_s <- sd(butterfly)
se <- sd(butterfly)/sqrt(14)

data.frame(butterfly) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = butterfly) +
    ggplot2::geom_histogram(fill = "darkorchid4") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Wing area (cm^2)", y = "Frequency") +
    ggplot2::geom_vline(xintercept = sample_mean, color = "red", size = 2) +
    ggplot2::geom_vline(xintercept = c(sample_mean + sample_s, sample_mean - sample_s), color = "blue", size = 1) +
    ggplot2::geom_vline(xintercept = c(sample_mean + se, sample_mean -se), color = "black", linetype = 2)
ggsave("butterfly_distribution2.png", height = 5, width = 7)

# just mean
data.frame(butterfly) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = butterfly) +
    ggplot2::geom_histogram(fill = "darkorchid4") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Wing area (cm^2)", y = "Frequency") +
    ggplot2::geom_vline(xintercept = sample_mean, color = "red", size = 2)
ggsave("butterfly_distribution3.png", height = 5, width = 7)

# plot bar graph showing mean +/- SE and +/- SD
mao <- data.frame(group = c("I", "II", "III", "IV", "V"),
                  n = c(18, 16, 8, 348, 332),
                  mean = c(9.81, 6.28, 5.97, 11.04, 13.29),
                  SE = c(0.85, 0.72, 1.13, 0.30, 0.30),
                  SD = c(3.62, 2.88, 3.19, 5.59, 5.50))

# plot sd
p1 <- mao %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = group, y = mean) +
    ggplot2::geom_bar(stat = "identity", fill = "thistle3", color = "thistle4") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Group", y = "MAO activity") +
    ggplot2::geom_errorbar(aes(ymin = mean-SD, ymax = mean + SD), width = 0.2) +
    ggplot2::ylim(0,20)

# plot se
p2 <- mao %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = group, y = mean) +
    ggplot2::geom_bar(stat = "identity", fill = "thistle3", color = "thistle4") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Group", y = "MAO activity") +
    ggplot2::geom_errorbar(aes(ymin = mean-SE, ymax = mean + SE), width = 0.2) +
    ggplot2::ylim(0,20)

cowplot::plot_grid(p1, p2, nrow = 1)
ggsave("mao_sd_v_se.png", height = 5, width = 10)

# the confidence interval - mean +/- 2SE
data.frame(x = c(-3,3)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(-1.96, 1.96),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +

    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3)) +
    ggplot2::theme_bw(24)
ggsave("95perc_dist.png", height = 4, width = 7)

# plotting student t distribution v. normal distribution
t.frame <- data.frame(rep = seq(-3,3,0.01),
                     df3 = dt(seq(-3,3,0.01),3),
                     df10 = dt(seq(-3,3,0.01),10),
                     std_normal = dnorm(seq(-3,3,0.01))) %>%
    tidyr::pivot_longer(cols = df3:std_normal, names_to = "variable", values_to = "values")

t.frame %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = rep, y = values, color = variable) +
    ggplot2::geom_line(size = 2) +
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   legend.position = "none") +    
    ggplot2::scale_color_manual(values = c("std_normal" = "grey50",
                                           "df3" = "lightpink",
                                           "df10" = "royalblue3"))+
    ggplot2::scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3))
ggsave("student_t_distribution.png", height = 4, width = 7)


# t 0.025
data.frame(x = c(-3,3)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(-3, -1.96),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(1.96, 3),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3)) +
    ggplot2::theme_bw(24)
ggsave("95perc_dist2.png", height = 4, width = 7)

# t 0.05
data.frame(x = c(-3,3)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(-3, -1.645),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(1.645, 3),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3)) +
    ggplot2::theme_bw(24)
ggsave("95perc_dist3.png", height = 4, width = 7)

# t 0.05 one sided
data.frame(x = c(-3,3)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(1.645, 3),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3)) +
    ggplot2::theme_bw(24)
ggsave("95perc_dist4.png", height = 4, width = 7)

# butterflies CI
data.frame(butterfly) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = butterfly) +
    ggplot2::geom_histogram(fill = "darkorchid4") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Wing area (cm^2)", y = "Frequency") +
    ggplot2::geom_vline(xintercept = sample_mean + 1.43, color = "red", size = 2)
ggsave("butterfly_distribution4.png", height = 5, width = 7)



