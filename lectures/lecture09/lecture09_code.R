# lecture 09 - nonparametric alternatives
library(tidyverse)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# permutation example - butterflies
orange <- c(29.7, 33.3,30.7,31.6)
white <- c(26.1,25.4,26.0,30.6)

# calculate the difference
mean(orange) - mean(white)

set.seed(182)
# shuffle the population 10000 times, and calculate the difference
total <- c(orange, white)
new_mean <- c()
for(i in 1:1000) {
    new_orange <- sample(total, 4)
    new_white <- total[!(total %in% new_orange)]
    new_mean <- c(new_mean, mean(new_orange) - mean(new_white))
}

(sum(abs(new_mean) > 4)+ 1)/1001


# what is the distribution of new_mean?
summary(new_mean)

ggplot2::ggplot(data = data.frame(new_mean)) +
    ggplot2::aes(x = new_mean) +
    ggplot2::geom_histogram(bins = 15, fill = "grey70", color = "black") +
    # ggplot2::geom_density(size = 1.5) +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Permuted mean", y = "Frequency")
ggsave("permutation1_2.png", height = 4, width = 7)

# where does our real value fall?
ggplot2::ggplot(data = data.frame(new_mean)) +
    ggplot2::aes(x = new_mean) +
    ggplot2::geom_histogram(bins = 15, fill = "grey70", color = "black") +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Permuted mean", y = "Frequency") +
    ggplot2::geom_vline(xintercept = 4.3, color = "blue", linetype = 2, size = 1.5)
ggsave("permutation2_2.png", height = 4, width = 7)

# how many of our values (Abs value) > 4.3
View(data.frame(new_mean))

(sum(abs(new_mean) > 4)+ 1)/1001

# 3 values / 100 are = 4.3 or -4.3 => 0.03 probability of us getting the observed data by chance
# at an alpha of 0.05 we would reject the null hypothesis!

# permuation for FDR linkage mapping
a <- c(450,439,412,329)
c <- c(400,378,356,311)

total <- c(c, a)
new_mean= NULL
for(i in 1:1000) {
    new_a <- sample(total, 4)
    new_c <- total[!(total %in% new_a)]
    new_mean <- c(new_mean, mean(new_a) - mean(new_c))
}

# what is the distribution of new_mean?
summary(new_mean)

ggplot2::ggplot(data = data.frame(new_mean)) +
    ggplot2::aes(x = new_mean) +
    ggplot2::geom_density(size = 1.5) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Permuted mean", y = "Probability") +
    ggplot2::geom_vline(xintercept = 46.25, color = "blue", linetype = 2, size = 1.5)
ggsave("mapping1.png", height = 4, width = 7)


# if we are ok being wrong 5% of the time - i.e. (false positive = alpha) 0.05
# what should the critical value be?
# in this case it is a two-sided test, so 2.5% on either side
quantile(new_mean, 0.975)

# critical value of 64.9. Our data < critical value = not enough evidence to reject the null

# where does our real value fall?
ggplot2::ggplot(data = data.frame(new_mean)) +
    ggplot2::aes(x = new_mean) +
    ggplot2::geom_density(size = 1.5) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Permuted difference", y = "Probability") +
    ggplot2::geom_vline(xintercept = 46.25, color = "blue", linetype = 2, size = 1.5) +    
    ggplot2::geom_vline(xintercept = c(64.9, -64.9), color = "red", size = 1.5)
ggsave("mapping2.png", height = 4, width = 7)

#######################
# Wilcoxon-mann-whitney test
#######################

# create data
study <- data.frame(experimental = c(5.32,5.60,5.74,6.06,6.32,6.34,6.79,7.18),
           control = c(4.50,4.78,4.79,4.86,5.41,5.70,6.08,6.21))

# plot distributions
study %>%
    tidyr::pivot_longer(experimental:control) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = name, y = value) +
    ggplot2::geom_point() +
    ggplot2::geom_boxplot(alpha = 0.5) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "", y = "Value") +
    ggplot2::theme(axis.title.x = element_blank())
ggsave("experiment1.png", height = 4, width = 7)


study %>%
    tidyr::pivot_longer(experimental:control) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = value, color = name) +
    ggplot2::geom_density(size = 1.5) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Value", y = "Relative Frequency") +
    ggplot2::theme(legend.position = "none")
ggsave("experiment2.png", height = 4, width = 7)

# plot qq plots
study %>%
    tidyr::pivot_longer(experimental:control) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(sample = value) +
    ggplot2::geom_qq() +
    ggplot2::geom_qq_line() +
    ggplot2::facet_wrap(~name, scales = "free") +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Expected", y = "Observed") +
    ggplot2::theme(legend.position = "none")
ggsave("experiment3.png", height = 4, width = 7)

# use R test
wilcox.test(study$experimental, study$control)

#########################
# sign test
#########################

# binomial distribution, N = 11, p = 0.5
data.frame(num = seq(1:11), prob = dbinom(1:11, 11, 0.5)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = num, y = prob) +
    ggplot2::geom_bar(stat = "identity", fill = "plum4", color = "grey10")+
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Number of positives", y = "Probability") 
ggsave("signtest.png", height = 4, width = 7)

# what is the probability of getting 9 (or more) positives?
pbinom(2, 11, 0.5)

# wilcox signed rank test
siteI <- c(50.6,39.2,35.2,17,11.2,14.2,24.2,37.4,35.2)
siteII <- c(38,18.6,23.2,19,6.6,16.4,14.4,37.6,24.4)
wilcox.test(siteI,siteII,paired = T)


# binomial distribution, N = 11, p = 0.5
data.frame(num = seq(1:9), prob = dbinom(1:9, 9, 0.5)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = num, y = prob) +
    ggplot2::geom_bar(stat = "identity", fill = "plum4", color = "grey10")+
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Number of positives", y = "Probability") 
ggsave("signtest2.png", height = 4, width = 7)

########################
# permutation 
boot.conf.int <- function(x, CI = 0.95, nsims = 10000) {
    xbars <- c()
    for(i in 1:nsims) {
        perm <- sample(x, replace = T)
        xbars <- c(xbars, mean(perm))
    }
    CI_quantiles <- c((1-CI)/2, 1-(1-CI)/2)
    boot_ci <- quantile(xbars, CI_quantiles)
    return(boot_ci)
}

set.seed(100)
# generate random variable x
x <- rnorm(100)

# what is the mean of x?
mean(x)
# [1] 0.002912563

# what is the 95% CI for this mean? - bootstrap!

# create variable to hold permutation means
perm_means <- c()

# do 10,000 permutations
for(i in 1:10000) {
    # sample from x with replacement (perm is same size as x)
    perm <- sample(x, replace = T)
    
    # calcualte mean and add to perm_means
    perm_means <- c(perm_means, mean(perm))
    
}

# get the middle 95% of the mean of the perms distribution
quantile(perm_means, c(0.025, 0.975))
#       2.5%      97.5% 
# -0.1987355  0.2013981

# plot mean of the perms distribution
ggplot2::ggplot(data.frame(perm_means)) +
    ggplot2::aes(x = perm_means) +
    ggplot2::geom_histogram(fill = "grey70", color = "black") +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Permutation means", y = "Frequency") +
    ggplot2::geom_vline(xintercept = mean(x), color = "red", size = 1.5) +
    ggplot2::geom_vline(xintercept = quantile(perm_means, c(0.025, 0.975)), color = "blue", linetype = 2, size = 1.5)
ggsave("permutation_distribution.png", height = 3, width = 7)
