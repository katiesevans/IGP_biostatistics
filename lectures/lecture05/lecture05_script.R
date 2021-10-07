# lecture05 - distributions pt. 2
library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/figures"))

# assessing normality
# fish population - normal
set.seed(1582391)
fish <- rnorm(100, 54, 4.5)

ggplot2::ggplot(data.frame(fish)) +
    ggplot2::aes(x = fish) +
    ggplot2::geom_histogram(bins = 10, fill = "grey70", color = "black") +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Fish length", y = "Frequency")
ggsave("fish_distribution.png", height = 4, width = 5)

summary(fish)

#height
height <- c(61, 62.5, 63, 64, 64.5, 65, 66.5, 67, 68, 68.5, 70.5)
mean(height)
sd(height)

heightdf <- data.frame(height) %>%
    dplyr::arrange(height) %>%
    tibble::rownames_to_column() %>% # great for getting rank!
    dplyr::mutate(perc = (as.numeric(rowname) - 0.5)/11,
                  expected = qnorm(perc, mean(height), sd(height)))

heightdf %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = expected, y = height) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Expected height", y = "Observed height")
ggsave("qqplot_height.png", height = 4, width = 6)


# plot different graphs
data.frame(x = c(0,10)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = sqrt, 
                           n = 100, 
                           args = list(mean = 300, sd = 10),
                           color = "red", size = 1.5)
    ggplot2::labs(y = "Probability", x = "Brood size") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme_bw(24) +
    ggplot2::geom_vline(xintercept = 300, linetype = 2, size = 1)
ggsave("sd_v_n.png", height = 5, width = 7)

sqrt <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
    stat_function(fun = function(x) sqrt(x), color = "black", size = 2) +
    xlim(0, 10) +
    ggplot2::theme_minimal()
log <- ggplot(data = data.frame(x = 1), mapping = aes(x = x)) +
    stat_function(fun = function(x) log(x), color = "black", size = 2) +
    xlim(0, 2) +
    ggplot2::theme_minimal()
negsqrt <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
    stat_function(fun = function(x) 1/sqrt(x), color = "black", size = 2) +
    xlim(0, 10) +
    ggplot2::theme_minimal()
inv <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
    stat_function(fun = function(x) 1/x, color = "black", size = 2) +
    xlim(0, 10) +
    ggplot2::theme_minimal()

cowplot::plot_grid(sqrt, log, negsqrt, inv, labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2)
ggsave("right_skew.png", height = 5, width = 5)

# left skew
sq <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
    stat_function(fun = function(x) x^2, color = "black", size = 2) +
    xlim(0, 10) +
    ggplot2::theme_minimal()
cub <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
    stat_function(fun = function(x) x^3, color = "black", size = 2) +
    xlim(0, 10) +
    ggplot2::theme_minimal()
quar <- sq <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
    stat_function(fun = function(x) x^4, color = "black", size = 2) +
    xlim(0, 10) +
    ggplot2::theme_minimal()
cowplot::plot_grid(sq, cub, labels = c("A", "B"), nrow = 1, ncol = 2)
ggsave("left_skew.png", height = 2.5, width = 5)

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

# plot sampling distribution
data.frame(dist = c(mean(sam1), mean(sam2), mean(sam3), mean(sam4))) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = dist) +
    ggplot2::geom_histogram(bins = 3) +
    ggplot2::theme_bw(14) +
    ggplot2::labs(x = "Sample mean distribution", y = "Frequency")
ggsave("sample_dist1.png", height = 4, width = 4)

set.seed(58)
# plot larger sampling distribution
data.frame(dist = rnorm(100, 300, 20)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = dist) +
    ggplot2::geom_histogram(bins = 10, fill = "grey70", color = "black") +
    ggplot2::theme_bw(14) +
    ggplot2::labs(x = "Sample mean distribution", y = "Frequency")
ggsave("sample_dist2.png", height = 4, width = 4)

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
set.seed(50)
val <- rchisq(100, 3)
mean(val)
sd(val)
data.frame(x = c(0,15)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dchisq, n = 100, args = list(df = 3), color = "black", size = 1) + 
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank()) +
    ggplot2::geom_vline(xintercept = mean(val), color = "red", linetype = 2, size = 1.5)
ggsave("new_skewed_distribution.png", height = 5, width = 7)


# take samples from skewed distribution
# sample of 20 values, 3 times
set.seed(1)
sample1 <- data.frame(rep = seq(1,3), test = "sample1") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sample = paste(sample(val, 20), collapse = ","))

# sample of 20 values, 10 times
sample2 <- data.frame(rep = seq(1,10), test = "sample2") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sample = paste(sample(val, 20), collapse = ","))

# sample of 20 values, 20 times
sample3 <- data.frame(rep = seq(1,20), test = "sample3") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sample = paste(sample(val, 20), collapse = ","))

# sample of 20 values, 50 times
sample4 <- data.frame(rep = seq(1,50), test = "sample4") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sample = paste(sample(val, 20), collapse = ","))

# sample of 20 values, 100 times
sample5 <- data.frame(rep = seq(1,100), test = "sample5") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sample = paste(sample(val, 20), collapse = ","))

# sample of 20 values, 1000 times
sample6 <- data.frame(rep = seq(1,1000), test = "sample6") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sample = paste(sample(val, 20), collapse = ","))

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
    ggplot2::geom_histogram(binwidth = 0.3, fill = "cornflowerblue", color = "black") +
    # ggplot2::geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
    ggplot2::facet_wrap(~factor(test2,
                                levels = c("n = 3", "n = 10", 
                                           "n = 20", "n = 50", 
                                           "n = 100", "n = 1000")), scales = "free") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::geom_vline(xintercept = mean(val), color = "red", linetype = 2, size = 1.5)
ggsave("skewed_distribution_samples2.png", height = 7, width = 12)

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

# calculate QQ plots for each of the samples?
all_samples %>%
    dplyr::mutate(test2 = dplyr::recode(test,
                                        "sample1" = "n = 3",
                                        "sample2" = "n = 10",
                                        "sample3" = "n = 20",
                                        "sample4" = "n = 50",
                                        "sample5" = "n = 100",
                                        "sample6" = "n = 1000")) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(sample = mean) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::facet_wrap(~factor(test2,
                                levels = c("n = 3", "n = 10", 
                                           "n = 20", "n = 50", 
                                           "n = 100", "n = 1000")), scales = "free") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank())
ggsave("qq_plots.png", height = 7, width = 12)

test <- all_samples %>%
    dplyr::filter(test == "sample6") %>%
    dplyr::pull(mean)
qqnorm(test)
qqline(test)
