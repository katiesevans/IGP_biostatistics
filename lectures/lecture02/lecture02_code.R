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
