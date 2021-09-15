# lecture 04 - binomial distributions (and normal)
library(tidyverse)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# demonstrate relative frequency histogram and density curve
# create a fake dataset - blood glucose
glucose <- rnorm(500, mean = 20, sd = 10)

data.frame(glucose) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = glucose) +
    ggplot2::geom_histogram(bins = 25, fill = "steelblue", color = "black") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Variable", y = "Frequency")
ggsave("frequency_histogram.png", height = 4, width = 7)

# now plot as relative frequency
data.frame(glucose) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = glucose, y = ..density..) +
    ggplot2::geom_histogram(bins = 25, fill = "steelblue", color = "black") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Variable", y = "Relative frequency")
ggsave("rel_freq_histogram.png", height = 4, width = 7)

# verify that sum of all relative freq = 1
rel_freq <- data.frame(glucose) %>%
    dplyr::mutate(rel_freq = glucose / sum(glucose))
sum(rel_freq$rel_freq)

# now plot as relative frequency with density curve overlaid
data.frame(glucose) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = glucose, y = ..density..) +
    ggplot2::geom_histogram(bins = 25, fill = "steelblue", color = "black") +
    ggplot2::geom_density(size = 2, color = "red") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Variable", y = "Relative frequency")
ggsave("rel_freq_density.png", height = 4, width = 7)


# plot only density curve this time
data.frame(glucose) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = glucose, y = ..density..) +
    ggplot2::geom_density(size = 2, color = "navy", fill = "steelblue", alpha = 0.5) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Variable", y = "Relative frequency")
ggsave("rel_freq_density2.png", height = 4, width = 7)

# new example:
parasite <- rlnorm(500)
data.frame(parasite) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = parasite, y = ..density..) +
    ggplot2::geom_density(size = 2, color = "navy", fill = "steelblue", alpha = 0.5) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Variable", y = "Relative frequency") +
    ggplot2::xlim(0, 5)
ggsave("rel_freq_density3.png", height = 4, width = 7)


test <- data.frame(parasite) %>%
    dplyr::mutate(bin = dplyr::case_when(parasite < 1 ~ "1",
                                         parasite < 2 ~ "2",
                                         parasite < 3 ~ "3",
                                         parasite < 4 ~ "4",
                                         TRUE ~ "5")) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarize(rel = n()/500)
