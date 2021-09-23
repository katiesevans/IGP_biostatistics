# Lecture 02 - probability (part 1)
library(tidyverse)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# probability of a coin flip
sample(c("Heads", "Tails"), 1, prob = c(0.5, 0.5))

set.seed(2173591)

# probability - frequency
# create a dataframe of 1000 coin flips
flips <- data.frame(rep = seq(0,1000), outcome = NA, outcome_numeric = 0) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(outcome = sample(c("Heads", "Tails"), 1, prob = c(0.5, 0.5))) %>%
    dplyr::mutate(outcome_numeric = ifelse(outcome == "Heads", 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cumulative_heads = cumsum(outcome_numeric)) %>%
    dplyr::mutate(cumulative_freq = cumulative_heads / rep)

# plot
flips %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = rep, y = cumulative_freq) +
    ggplot2::geom_line() +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Number of coin tosses", y = "Relative frequency of heads") +
    ggplot2::geom_hline(yintercept = 0.5, color = "red", linetype = 2, size = 1) +
    ggplot2::expand_limits(x = 0, y = 0)

# save plot
ggsave("plot01_probability_frequency.png", height = 5, width = 5)

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
