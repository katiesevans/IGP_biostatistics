# lecture 04 - binomial distributions (and normal)
library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/figures"))

test <- data.frame(parasite) %>%
    dplyr::mutate(bin = dplyr::case_when(parasite < 1 ~ "1",
                                         parasite < 2 ~ "2",
                                         parasite < 3 ~ "3",
                                         parasite < 4 ~ "4",
                                         TRUE ~ "5")) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarize(rel = n()/500)

### Expected value
cells <- data.frame(markers = c(1,2,3,5,8),
                    percent = c(40,28,19,9,4))

cells %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = markers, y = percent) +
    ggplot2::geom_bar(stat = "identity", fill = "grey70", color = "black") +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Number of markers", y = "Percent of cells") +
    ggplot2::geom_vline(xintercept = 2.3, size = 1.5, color = "red", linetype = 2)
ggsave("expected_value.png", height = 4, width = 7)


# expected value vs. mean of simulated samples
cells <- data.frame(markers = c(1,2,3,5,8),
                    perc = c(0.4, .28, .19, .09, .04)) %>%
    dplyr::mutate(total = perc * 1000,
                  var = ((markers - 2.3)**2)*perc)
cell_sample <- c(rep(1, .4*1000),
                 rep(2, .28*1000),
                 rep(3, .19*1000),
                 rep(5, .09*1000),
                 rep(8, .04*1000))
mean(cell_sample)
sum(cells$var)
sd(cell_sample)

sum(cells$markers*(cells$perc))


######## adding and subtracting random variables
set.seed(10)
df <- data.frame(exp=rnorm(1000, 10, 1.5))
df %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = exp) +
    ggplot2::geom_density() +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Gene expression", y = "Relative frequency") +
    ggplot2::geom_vline(aes(xintercept = mean(exp)), size = 1.5, color = "red") +
    ggplot2::geom_vline(aes(xintercept = mean(exp) + sd(exp)), size = 1.5, color = "blue", linetype = 2) +
    ggplot2::geom_vline(aes(xintercept = mean(exp) - sd(exp)), size = 1.5, color = "blue", linetype = 2)
ggsave("gene_exp.png", height = 4, width = 7)

df2 <- data.frame(exp=rnorm(100, 12, 1.5))
df %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = exp) +
    ggplot2::geom_density() +
    ggplot2::geom_density(data = df2) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Gene expression", y = "Relative frequency") +
    ggplot2::geom_vline(data = df2, aes(xintercept = mean(exp)), size = 1.5, color = "red") +
    ggplot2::geom_vline(data = df2, aes(xintercept = mean(exp) + sd(exp)), size = 1.5, color = "blue", linetype = 2) +
    ggplot2::geom_vline(data = df2, aes(xintercept = mean(exp) - sd(exp)), size = 1.5, color = "blue", linetype = 2)
ggsave("gene_exp4.png", height = 4, width = 7)

# what about adding two random variables?
set.seed(61)
var1 <- rnorm(10000, 5, 2.5)
var2 <- rnorm(10000, 7, 1.75)
var3 <- var1 + var2

ggplot2::ggplot() +
    ggplot2::geom_density(data = data.frame(var1), aes(x = var1), size = 1.5, color = "red") +
    ggplot2::theme_bw(24) +
    ggplot2::geom_vline(xintercept = mean(var1), color = "red", linetype = 2) +
    ggplot2::geom_rect(aes(xmin = mean(var1) - sd(var1),
                                                    xmax = mean(var1) + sd(var1),
                                                    ymin = 0, ymax = Inf),
                       fill = "red", alpha = 0.1) +
    ggplot2::geom_density(data = data.frame(var2), aes(x = var2), color = "blue", size = 1.5) +
    ggplot2::geom_vline(xintercept = mean(var2), color = "blue", linetype = 2) +
    ggplot2::geom_rect(aes(xmin = mean(var2) - sd(var2),
                           xmax = mean(var2) + sd(var2),
                           ymin = 0, ymax = Inf),
                       fill = "blue", alpha = 0.1) +
    ggplot2::labs(x = "Variable", y = "Relative frequency")
ggsave("adding_var1.png", height = 4, width = 7)

ggplot2::ggplot() +
    ggplot2::geom_density(data = data.frame(var1), aes(x = var1), size = 1, color = "red") +
    ggplot2::geom_density(data = data.frame(var2), aes(x = var2), color = "blue", size = 1) +
    ggplot2::geom_density(data = data.frame(var3), aes(x = var3), color = "purple", size = 1.5) +
    ggplot2::theme_bw(24) +
    ggplot2::labs(x = "Variable", y = "Relative frequency")
ggsave("adding_var2.png", height = 4, width = 7)




pa <- ggplot2::ggplot() +
    ggplot2::geom_density(data = data.frame(var1), aes(x = var1), size = 1.5, color = "red") +
    ggplot2::theme_bw(20) +
    ggplot2::geom_vline(xintercept = mean(var1), color = "red", linetype = 2) +
    ggplot2::geom_rect(aes(xmin = mean(var1) - sd(var1),
                           xmax = mean(var1) + sd(var1),
                           ymin = 0, ymax = Inf),
                       fill = "red", alpha = 0.1) +
    ggplot2::xlim(-5,25) +
    ggplot2::ylim(0,0.25) +
    ggplot2::labs(x = "Variable A", y = "Relative frequency")
pb <- ggplot2::ggplot() +
    ggplot2::geom_density(data = data.frame(var2), aes(x = var2), color = "blue", size = 1.5) +
    ggplot2::geom_vline(xintercept = mean(var2), color = "blue", linetype = 2) +
    ggplot2::geom_rect(aes(xmin = mean(var2) - sd(var2),
                           xmax = mean(var2) + sd(var2),
                           ymin = 0, ymax = Inf),
                       fill = "blue", alpha = 0.1) +
    ggplot2::xlim(-5,25) +
    ggplot2::ylim(0,0.25) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.title.y = element_blank()) +
    ggplot2::labs(x = "Variable B", y = "Relative frequency")
pc <- ggplot2::ggplot() +
    ggplot2::geom_density(data = data.frame(var3), aes(x = var3), color = "purple", size = 1.5) +
    ggplot2::geom_vline(xintercept = mean(var3), color = "purple", linetype = 2) +
    ggplot2::geom_rect(aes(xmin = mean(var3) - sd(var3),
                           xmax = mean(var3) + sd(var3),
                           ymin = 0, ymax = Inf),
                       fill = "purple", alpha = 0.1) +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Variable A + B", y = "Relative frequency") +
    ggplot2::xlim(-5,25) +
    ggplot2::ylim(0,0.25) +
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.title.y = element_blank())

cowplot::plot_grid(pa, pb, pc, nrow = 1, rel_widths = c(1.2, 0.8, 0.8))
ggsave("adding_var1.png", height = 3, width = 7)

var4 <- var2-var1
pd <- ggplot2::ggplot() +
    ggplot2::geom_density(data = data.frame(var4), aes(x = var4), color = "purple", size = 1.5) +
    ggplot2::geom_vline(xintercept = mean(var4), color = "purple", linetype = 2) +
    ggplot2::geom_rect(aes(xmin = mean(var4) - sd(var4),
                           xmax = mean(var4) + sd(var4),
                           ymin = 0, ymax = Inf),
                       fill = "purple", alpha = 0.1) +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Variable A + B", y = "Relative frequency") +
    ggplot2::xlim(-5,25) +
    ggplot2::ylim(0,0.25) +
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.title.y = element_blank())

cowplot::plot_grid(pa, pb, pd, nrow = 1, rel_widths = c(1.2, 0.8, 0.8))
ggsave("adding_var2.png", height = 3, width = 7)
