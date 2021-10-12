# lecture06 script
library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/figures"))

# butterfly area example
butterfly <- c(33.9, 33, 30.6, 36.6, 36.5, 34, 36.1, 32, 28, 32, 32.2, 32.2, 32.3, 30)

# plot distribution
data.frame(butterfly) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = butterfly) +
    ggplot2::geom_histogram(fill = "darkorchid4", bins = 10) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = expression(paste("Wing area (", cm^2, ")")), y = "Frequency")
ggsave("butterfly_distribution.png", height = 5, width = 7)

# plot distribution with SE and SD
sample_mean <- mean(butterfly)
sample_s <- sd(butterfly)
se <- sd(butterfly)/sqrt(14)

data.frame(butterfly) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = butterfly) +
    ggplot2::geom_histogram(fill = "darkorchid4", bins = 10) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = expression(paste("Wing area (", cm^2, ")")), y = "Frequency") +
    ggplot2::geom_vline(xintercept = sample_mean, color = "red", size = 2) +
    ggplot2::geom_vline(xintercept = c(sample_mean + sample_s, sample_mean - sample_s), color = "blue", size = 1) +
    ggplot2::geom_vline(xintercept = c(sample_mean + se, sample_mean -se), color = "black", linetype = 2)
ggsave("butterfly_distribution2.png", height = 5, width = 7)

# just mean
data.frame(butterfly) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = butterfly) +
    ggplot2::geom_histogram(fill = "darkorchid4", bins = 10) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = expression(paste("Wing area (", cm^2, ")")), y = "Frequency") +
    ggplot2::geom_vline(xintercept = sample_mean, color = "red", size = 2)
ggsave("butterfly_distribution3.png", height = 5, width = 7)

# plot SD, SE, mean as n increases
set.seed(9)
pop <- rnorm(10000, 32, 2.5)
pop_mean <- mean(pop)
pop_sd <- sd(pop)

# n = 14
s14 <- sample(pop, 14, replace = F)
mean(s14)
sd(s14)
sd(s14)/sqrt(14)

# n = 140
s140 <- sample(pop, 140, replace = F)
mean(s140)
sd(s140)
sd(s140)/sqrt(140)

# n = 1400
s1400 <- sample(pop, 1400, replace = F)
mean(s1400)
sd(s1400)
sd(s1400)/sqrt(1400)

# plots
all <- data.frame(val = s14) %>%
    dplyr::mutate(n = 14) %>%
    dplyr::bind_rows(data.frame(val = s140) %>% dplyr::mutate(n = 140)) %>%
    dplyr::bind_rows(data.frame(val = s1400) %>% dplyr::mutate(n = 1400)) %>%
    dplyr::bind_rows(data.frame(val = pop) %>% dplyr::mutate(n = Inf)) %>%
    dplyr::mutate(n = paste0("n = ", n))

ggplot2::ggplot(all) +
    ggplot2::aes(x = val) +
    ggplot2::geom_histogram(fill = "grey70") +
    ggplot2::facet_wrap(~n, scales = "free_y", nrow = 1) +
    ggplot2::theme_minimal(20) +
    ggplot2::theme(axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(), 
                   panel.grid = element_blank())
ggsave("se_v_sd_over_n.png", height = 2, width = 10)




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


t.frame %>%
    dplyr::filter(variable == "std_normal") %>%
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
ggsave("normal_distribution.png", height = 4, width = 7)


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


# plot one sided v two sided
data.frame(x = c(-3,3)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(qt(0.025, Inf), qt(0.975, Inf)),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 0, sd = 1),
                           xlim = c(-3, qt(0.95, Inf)),
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
ggsave("95perc_dist5.png", height = 4, width = 7)

# plot no area
data.frame(x = c(-3,3)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3)) +
    ggplot2::theme_bw(24)
ggsave("95perc_dist6.png", height = 4, width = 7)

# butterflies CI
data.frame(butterfly) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = butterfly) +
    ggplot2::geom_histogram(fill = "darkorchid4") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Wing area (cm^2)", y = "Frequency") +
    ggplot2::geom_vline(xintercept = sample_mean + 1.43, color = "red", size = 2)
ggsave("butterfly_distribution4.png", height = 5, width = 7)

# butterfly - t = 4.23
data.frame(x = c(20,45)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 32.81, sd = 2.48),
                           xlim = c(40.4904, 45),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 32.81, sd = 2.48),
                           xlim = c(20,25.13),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 32.81, sd = 2.48),
                           color = "grey50", size = 1.5) + 
    ggplot2::scale_y_continuous(breaks = NULL) +
    # ggplot2::scale_x_continuous(breaks = c(320,380,440,500,560,620,680)) +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::geom_vline(xintercept = c(40.4904, 25.13), size = 1.5, linetype = 2, color = "blue")
ggsave("butterfly_t1.png", height = 5, width = 7)

# find p value for t = 4.23
pt(4.23, 13, lower.tail = F)*2

