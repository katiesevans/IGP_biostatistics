# Lecture 01 code - Intro, sampling, and summary statistics
library(tidyverse)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Visualizing frequency distributions
poinsettias <- data.frame(color = c("red", "pink", "white"),
                          frequency = c(108, 34, 40)) %>%
    dplyr::mutate(total = sum(frequency)) %>%
    dplyr::mutate(relative_frequency = frequency / total) %>%
    dplyr::mutate(percent_frequency = relative_frequency * 100)

# check that relative frequency adds to 1
sum(poinsettias$relative_frequency)
sum(poinsettias$percent_frequency)

# plot as bar chart
poinsettias %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = factor(color, levels = c("red", "pink", "white")), 
                 y = frequency,
                 fill = color) +
    ggplot2::geom_bar(stat = "identity", color = "black") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Color", y = "Frequency") +
    ggplot2::scale_fill_manual(values = c("red" = "red3", "pink" = "pink", "white" = "grey90")) +
    ggplot2::theme(legend.position = "none")

# save
ggsave("poinsettas_frequency.png", height = 5, width = 5)
