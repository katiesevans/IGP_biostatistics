# lecture 11 - chi square (categorical)
library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/figures"))

# chi square distribution - different df
data.frame(x = c(0,12)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dchisq, n = 100, args = list(df = 1), color = "red", size = 1) + 
    ggplot2::stat_function(fun = dchisq, n = 100, args = list(df = 2), color = "orange", size = 1) + 
    ggplot2::stat_function(fun = dchisq, n = 100, args = list(df = 3), color = "gold", size = 1) + 
    ggplot2::stat_function(fun = dchisq, n = 100, args = list(df = 4), color = "darkgreen", size = 1) + 
    ggplot2::stat_function(fun = dchisq, n = 100, args = list(df = 5), color = "blue", size = 1) + 
    ggplot2::stat_function(fun = dchisq, n = 100, args = list(df = 6), color = "purple", size = 1) + 
    ggplot2::stat_function(fun = dchisq, n = 100, args = list(df = 7), color = "pink", size = 1) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme_bw(24) +
    # ggplot2::ylim(0, 0.5) +
    # ggplot2::xlim(0,12) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks.y = element_blank())
ggsave("chi_square_dfs.png", height = 5, width = 10)


# chi square distribution
data.frame(x = c(-1,10)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dchisq,
                           n = 100,
                           geom = "area",
                           args = list(df = 2),
                           xlim = c(qchisq(0.95, 2), 10),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dchisq, 
                           n = 100, 
                           args = list(df = 2),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks.y = element_blank()) +
    ggplot2::geom_vline(xintercept = 0.69, color = "red", linetype = 2, size = 1)
ggsave("chi_square_df2_2.png", height = 5, width = 10)

# birth example
data.frame(x = c(0,20)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dchisq,
                           n = 100,
                           geom = "area",
                           args = list(df = 1),
                           xlim = c(13.3, 20),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dchisq, 
                           n = 100, 
                           args = list(df = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks.y = element_blank()) +
    ggplot2::geom_vline(xintercept = 13.3, color = "red", linetype = 2, size = 1)
ggsave("chi_square_births.png", height = 3, width = 4)
1-pchisq(13.3, 1)

# chi square distribution df = 1
data.frame(x = c(5,10)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dchisq,
                           n = 100,
                           geom = "area",
                           args = list(df = 1),
                           xlim = c(qchisq(0.95, 2), 10),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dchisq, 
                           n = 100, 
                           args = list(df = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks.y = element_blank()) +
    ggplot2::geom_vline(xintercept = 6.06, color = "red", linetype = 2, size = 1)


data <- data.frame(real = c(35, 14),
                   sham = c(19, 5))
chisq.test(data, correct = F)

# test phone tumors
tumor <- data.frame(left = c(14,19), right = c(28,27))
chisq.test(tumor, correct = F)
((14-15.75)^2)/15.75 + (28-26.25)^2/26.25 + (19-17.25)^2/17.25 + (27-28.75)^2/28.75

###########################
# r x k - alzheimers example
alz <- data.frame(group1 = c(22, 10), group2 = c(18, 11), group3 = c(12, 19), 
                  group4 = c(7,11), group5 = c(16,24), treatment = c("EGb", "Placebo"))

alz %>%
    tidyr::pivot_longer(group1:group5) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = name, y = value, fill = treatment) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_bw(20) +
    ggplot2::scale_fill_manual(values = c("EGb" = "skyblue", "Placebo" = "skyblue4")) +
    ggplot2::labs(x = "Change in ADAS-Cog scores", y = "Number of patients") +
    ggplot2::scale_x_discrete(labels = c("< -4", "-3 to -2", "-1 to +1", "+2 to +3", "> +4"))
ggsave("alzheimers.png", width = 10, height = 3)    
