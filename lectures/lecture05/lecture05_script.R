# lecture05 - distributions pt. 2
library(tidyverse)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# set seed
set.seed(215)

# create a population - 10% disease
# 0 = no disease, 1 = disease
pop <- c(rep(0, 45), rep(1, 5))

# create a random sample 1000 times
# 50 choices for first individual, 49 for second, 48... = 50x49x48x47x46 ways to sample the data
df <- data.frame(rep = seq(1:1000)) %>%
    dplyr::rowwise() %>%
    # create the samples
    dplyr::mutate(sample = paste(sample(pop, 5), collapse = ",")) %>%
    # separate each element of the sample into 5 columns for easy math
    tidyr::separate(sample, into = c("one", "two", "three", "four", "five"), sep = ",", remove = FALSE) %>%
    # rowwise helps to make sure the calculations are done row by row instead of summing
    dplyr::rowwise() %>%
    # find the proportion of disease individuals in the population
    dplyr::mutate(prop = sum(as.numeric(one), 
                             as.numeric(two), 
                             as.numeric(three), 
                             as.numeric(four), 
                             as.numeric(five))/5) %>%
    dplyr::select(rep, sample, prop)


df <- data.frame(rep = seq(1:1000)) %>%
    dplyr::rowwise() %>%
    # create the samples
    dplyr::mutate(sample = paste(sample(pop, 5), collapse = ",")) %>%
    dplyr::mutate(individual = sample) %>%
    # separate each element of the sample
    tidyr::separate_rows(individual, sep = ",", convert = TRUE) %>%
    dplyr::mutate(prob = ifelse(individual == 0, 45/50, 5/50)) %>%
    dplyr::group_by(rep, sample) %>%
    dplyr::summarize(probability = prod(prob),
                     relative_freq = mean(individual))

# check out the distribution
# mean = 0.094! population mean = 0.1 :) 
summary(df$probability)
summary(df$relative_freq)

# c. elegans samples
set.seed(17593)
sam1 <- rnorm(10, mean = 300, sd = 20)
mean(sam1)
sd(sam1)
sam2 <- rnorm(10, mean = 300, sd = 20)
mean(sam2)
sd(sam2)
sam3 <- rnorm(10, mean = 300, sd = 20)
mean(sam3)
sd(sam3)
sam4 <- rnorm(10, mean = 300, sd = 20)
mean(sam4)
sd(sam4)

# mean of sample means
mean(c(mean(sam1), mean(sam2), mean(sam3), mean(sam4)))

# sd of samples
sd(c(mean(sam1), mean(sam2), mean(sam3), mean(sam4)))
20/sqrt(4)

# plot sample standard deviation as a function of sample size n
data.frame(x = c(250,350)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 300, sd = 10),
                           color = "red", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 300, sd = 6.32),
                           color = "navy", size = 1.5) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 300, sd = 4.47),
                           color = "forestgreen", size = 1.5) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 300, sd = 2.82),
                           color = "purple", size = 1.5) +
    ggplot2::labs(y = "Probability", x = "Brood size") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) +
    ggplot2::geom_vline(xintercept = 300, linetype = 2, size = 1)
ggsave("sd_v_n.png", height = 5, width = 7)


# seed weight example
data.frame(x = c(320,680)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    # ggplot2::stat_function(fun = dnorm, 
    #                        n = 100, 
    #                        geom = "area",
    #                        args = list(mean = 0, sd = 1),
    #                        xlim = c(-3, 1.53),
    #                        alpha = 0.7,
    #                        fill = "steelblue3", size = 1.5) + 
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 500, sd = 60),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "Sample mean weight (mg)") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(320,380,440,500,560,620,680)) +
    ggplot2::theme_bw(24)
ggsave("seed_weights1.png", height = 5, width = 7)

data.frame(x = c(320,680)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm,
                           n = 100,
                           geom = "area",
                           args = list(mean = 500, sd = 60),
                           xlim = c(550, 680),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 500, sd = 60),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "Sample mean weight (mg)") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(320,380,440,500,560,620,680)) +
    ggplot2::theme_bw(24)
ggsave("seed_weights2.png", height = 5, width = 7)

# prob z > 0.83
1-pnorm(0.83)
1-pnorm(550, mean = 500, sd = 60)

#######
# Central limit theorem
# make non-normal distribution
set.seed(1)
skewed <- data.frame(val = rbeta(1000, 2, 5))
skewed %>%
    data.frame(val = rbeta(1000, 2, 5)) %>%
    ggplot(.) +
    aes(x = val) +
    geom_density(color = "grey50", size = 1.5) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks.y = element_blank())
ggsave("skewed_distribution.png", height = 5, width = 7)
mean(skewed$val)
sd(skewed$val)

# take samples from skewed distribution
# sample of 20 values, 3 times
sample1 <- data.frame(rep = seq(1,3), test = "sample1") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sample = paste(sample(skewed$val, 20), collapse = ","))

# sample of 20 values, 10 times
sample2 <- data.frame(rep = seq(1,10), test = "sample2") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sample = paste(sample(skewed$val, 20), collapse = ","))

# sample of 20 values, 20 times
sample3 <- data.frame(rep = seq(1,20), test = "sample3") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sample = paste(sample(skewed$val, 20), collapse = ","))

# sample of 20 values, 50 times
sample4 <- data.frame(rep = seq(1,50), test = "sample4") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sample = paste(sample(skewed$val, 20), collapse = ","))

# sample of 20 values, 100 times
sample5 <- data.frame(rep = seq(1,100), test = "sample5") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sample = paste(sample(skewed$val, 20), collapse = ","))

# sample of 20 values, 1000 times
sample6 <- data.frame(rep = seq(1,1000), test = "sample6") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sample = paste(sample(skewed$val, 20), collapse = ","))

# combine samples and plot distributions
all_samples <- sample1 %>%
    dplyr::bind_rows(sample2, sample3, sample4, sample5, sample6) %>%
    dplyr::mutate(individual = sample) %>%
    tidyr::separate_rows(individual, convert = TRUE) %>%
    dplyr::group_by(test, rep, sample) %>%
    dplyr::summarize(mean = mean(individual),
                     sd = sd(individual))

all_samples %>%
    dplyr::mutate(test2 = dplyr::recode(test,
                                        "sample1" = "n = 3",
                                        "sample2" = "n = 10",
                                        "sample3" = "n = 20",
                                        "sample4" = "n = 50",
                                        "sample5" = "n = 100",
                                        "sample6" = "n = 1000")) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = mean) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(~factor(test2,
                                levels = c("n = 3", "n = 10", 
                                           "n = 20", "n = 50", 
                                           "n = 100", "n = 1000")), scales = "free") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank())
ggsave("skewed_distribution_samples.png", height = 7, width = 12)

# total means
total <- all_samples %>%
    dplyr::mutate(samples = dplyr::recode(test,
                                        "sample1" = "n = 3",
                                        "sample2" = "n = 10",
                                        "sample3" = "n = 20",
                                        "sample4" = "n = 50",
                                        "sample5" = "n = 100",
                                        "sample6" = "n = 1000")) %>%
    dplyr::group_by(test, samples) %>%
    dplyr::summarize(sample_mean = mean(mean))
