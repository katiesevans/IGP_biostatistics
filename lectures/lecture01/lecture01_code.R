# Lecture 01 code - Intro, sampling, and summary statistics
library(tidyverse)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# showing random variation
data.frame(x = seq(1:20), y = rnorm(20, mean = 30, sd = 2)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = y) +
    ggplot2::geom_bar(stat = "identity", fill = "grey70", color = "grey10") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Replicate", y = "Result") +
    ggplot2::geom_hline(yintercept = 30, color = "red", linetype = 2, size = 1.5)
ggsave("random_variation.png", height = 4, width = 7)

## Visualizing frequency distributions

## Categorical distributions - bar plot
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

# plot as relative frequency bar chart
poinsettias %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = factor(color, levels = c("red", "pink", "white")), 
                 y = relative_frequency,
                 fill = color) +
    ggplot2::geom_bar(stat = "identity", color = "black") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Color", y = "Relative frequency") +
    ggplot2::scale_fill_manual(values = c("red" = "red3", "pink" = "pink", "white" = "grey90")) +
    ggplot2::theme(legend.position = "none")

# save
ggsave("poinsettas_relative_frequency.png", height = 5, width = 5)

## Numeric distributions - histogram
# create data for human height
set.seed(30)
height_df <- data.frame(person = seq(1,100,1),
                     height = rnorm(100, mean = 162.7, sd = 7.5))

# plot histogram of height
p1 <- height_df %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = height) +
    ggplot2::geom_histogram(fill = "dodgerblue3", color = "navy") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "", y = "Frequency")
p1
ggsave(p1, filename = "height_distribution.png", height = 5, width = 7)

# plot with different bin sizes
p2 <- height_df %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = height) +
    ggplot2::geom_histogram(fill = "dodgerblue3", color = "navy", bins = 10) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Height (cm)", y = "")

p3 <- height_df %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = height) +
    ggplot2::geom_histogram(fill = "dodgerblue3", color = "navy", bins = 100) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "", y = "")
cowplot::plot_grid(p1, p2, p3, rows = 1, cols = 3)
ggsave("height_distribution2.png", height = 5, width = 12)

# add density curve to p2
p4 <- height_df %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = height) +
    ggplot2::geom_histogram(fill = "dodgerblue3", color = "navy", bins = 10,
                            aes(y = ..density..)) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Height (cm)", y = "") +
    ggplot2::geom_density(col = "red", size = 2) +
    ggplot2::scale_y_continuous(labels = c(0,5,10,15,20,25))
p4
cowplot::plot_grid(p1, p4, p3, rows = 1, cols = 3)
ggsave("height_distribution3.png", height = 5, width = 12)

# boxplots for height
height_df %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = "Height", y = height) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "", y = "Height (cm)") +
    ggplot2::theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank())
ggsave("boxplot_height1.png", height = 5, width = 5)

# add points too
height_df %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = "Height", y = height) +
    ggplot2::geom_boxplot(outlier.color = NA) +
    ggplot2::geom_jitter(width = 0.2)+ 
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "", y = "Height (cm)") +
    ggplot2::theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank())
ggsave("boxplot_height2.png", height = 5, width = 5)


# plot mean and median
height_df %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = height) +
    ggplot2::geom_histogram(fill = "dodgerblue3", color = "navy", bins = 10) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Height (cm)", y = "Frequency") +
    ggplot2::geom_vline(aes(xintercept = mean(height, na.rm = T)), color = "red", size = 2) +
    ggplot2::geom_vline(aes(xintercept = median(height, na.rm = T)), color = "skyblue", size = 2)

#### Calcualte variance and sd
group1 <- c(-10, 0, 10, 20, 30)
mean(group1)
group2 <- c(8,9,10,11,12)
mean(group2)


# plot 1 standard deviation away from the mean
avg <- mean(df$height)
sdt <- sd(df$height)
df %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = height) +
    ggplot2::geom_histogram(fill = "dodgerblue3", color = "navy", bins = 10) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Height (cm)", y = "Frequency") +
    ggplot2::geom_vline(xintercept = avg, color = "red", size = 2) +
    ggplot2::geom_vline(xintercept = c(mean + sdt, mean - sdt), color = "cyan2", size = 2) +
    ggplot2::geom_vline(xintercept = c(mean + 2*sdt, mean - 2*sdt), color = "grey", size = 2, linetype = 2)
ggsave("standard_deviation1.png", height = 5, width = 10)


# linear transformation

set.seed(21)
mg <- data.frame(mg = rnorm(100, mean = 250, sd = 20))

p1 <- mg %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = mg) +
    ggplot2::geom_histogram(fill = "dodgerblue3", color = "navy", bins = 10) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Mass (mg)", y = "Frequency")

p2 <- mg %>%
    dplyr::mutate(grams = mg / 1000) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = grams) +
    ggplot2::geom_histogram(fill = "dodgerblue3", color = "navy", bins = 10) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Mass (g)", y = "Frequency")
cowplot::plot_grid(p1, p2, nrow = 1)
ggsave("linear_transformation.png", height = 4, width = 12)

p2 <- mg %>%
    dplyr::mutate(new = mg + 50) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = new) +
    ggplot2::geom_histogram(fill = "dodgerblue3", color = "navy", bins = 10) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Adjusted mass (g)", y = "Frequency")
cowplot::plot_grid(p1, p2, nrow = 1)
ggsave("linear_transformation2.png", height = 4, width = 12)

# non linear transformation

p1 <- mg %>%
    dplyr::mutate(new = 1/(mg),
                  new2 = log(new)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = new) +
    ggplot2::geom_histogram(fill = "pink", color = "grey20", bins = 10) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Value", y = "Frequency")


p2 <- mg %>%
    dplyr::mutate(new = 1/(mg),
                  new2 = log(new)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = new2) +
    ggplot2::geom_histogram(fill = "pink", color = "grey20", bins = 10) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "log(Value)", y = "Frequency")

cowplot::plot_grid(p1, p2, nrow = 1)
ggsave("log_transformation.png", height = 4, width = 12)

