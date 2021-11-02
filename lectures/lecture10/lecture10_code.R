# lecture 10 - multiple hypothesis correction
library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/figures"))

# show multiple test problem with dice example
dice <- data.frame(rolls = seq(1,50)) %>%
    dplyr::mutate(prob_6 = 1 - (5/6)**rolls)

dice %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = rolls, y = prob_6) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Number of rolls", y = "Probability of a six")
ggsave("dice_example.png", height = 4, width = 5)

# repeat example but with a general t-test with alpha = 0.05
general <- data.frame(tests = seq(1,100)) %>%
    dplyr::mutate(false_pos = 1 - (1-0.05)**tests)

general %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = tests, y = false_pos) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Number of tests", y = "Prob. of false positive")
ggsave("dice_example2.png", height = 4, width = 5)

#### FDR simulations ####
# one population
data.frame(x = c(-5,5)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "blue", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   panel.grid = element_blank())
ggsave("single_distribution.png", height = 3, width = 5)

# plot p value distributions from a single distribution
set.seed(19)
dist <- rnorm(1000)
pval <- NULL
for(i in 1:1000) {
    pval <- c(pval, t.test(sample(dist, 5), sample(dist, 5))[[3]])
}
data.frame(pval) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = pval) +
    ggplot2::geom_histogram(bins = 20, fill = "grey70", color = "black") +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "p-value", y = "Frequency")
ggsave("single_distribution2.png", height = 3, width = 5)

# two populations
data.frame(x = c(-5,7)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = -1, sd = 1),
                           color = "blue", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 2, sd = 1),
                           color = "red", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   panel.grid = element_blank())
ggsave("two_distributions.png", height = 3, width = 5)

# plot p value distributions from a single distribution
set.seed(19)
dist1 <- rnorm(500, mean = -1)
dist2 <- rnorm(500, mean = 1)

pval2 <- NULL
for(i in 1:500) {
    pval2 <- c(pval2, t.test(sample(dist1, 5), sample(dist2, 5))[[3]])
}
data.frame(pval2) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = pval2) +
    ggplot2::geom_histogram(bins = 20, fill = "skyblue3", color = "black") +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "p-value", y = "Frequency")
ggsave("two_distributions2.png", height = 3, width = 5)

# combine both distributions
data.frame(p = c(pval, pval2)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = p) +
    ggplot2::geom_histogram(bins = 20, fill = "grey70", color = "black") +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "p-value", y = "Frequency")
ggsave("full_distribution.png", height = 3, width = 5)

# show p-values as geom_point
data.frame(p = c(pval, pval2)) %>%
    dplyr::mutate(distribution = c(rep("Uniform", 1000), rep("Two-distributions", 500))) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = p, y = 1) +
    ggplot2::geom_boxplot(alpha = 0.5)+
    ggplot2::geom_jitter(size = 0.5, height = 0.3, aes(color = distribution)) +
    # ggplot2::geom_histogram(bins = 20, fill = "grey70", color = "black") +
    ggplot2::theme_bw(20) +
    ggplot2::scale_color_manual(values = c("red", "black")) +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank()) +
    ggplot2::labs(x = "p-value", y = "")

# what is the FDR for this data?
fdr <- data.frame(p = c(pval, pval2)) %>%
    dplyr::mutate(distribution = c(rep("Uniform", 1000), rep("Two-distributions", 500))) %>%
    dplyr::mutate(new.p = p.adjust(p, "fdr"),
                  sig = new.p < 0.05,
                  old.sig = p < 0.05)

table(fdr$distribution, fdr$old.sig)

table(fdr$distribution, fdr$sig)


# adjust pvalues with R
vals <- sort(c(0.084,0.036,0.063,0.186,0.108,0.042,0.01,0.132,0.175,0.0012))
p.adjust(vals, "bonferroni")
p.adjust(vals, "fdr")
