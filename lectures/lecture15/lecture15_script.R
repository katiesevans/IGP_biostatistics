# lecture15 script
library(tidyverse)
library(ggbeeswarm)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/figures"))

# corn data
corn <- data.frame(nematode = c(16.5,15,11.5,12,12.5,9,16,6.5,8,14.5,7,10.5),
                   wasps = c(11,15,9,9,11.5,11,9,10,9,8,8,5),
                   nem_wasp = c(8.5,13,12,10,12.5,8.5,9.5,7.0,10.5,10.5,13,9),
                   bacteria = c(16,14.5,15,9,10.5,14,12.5,9,9,9,6.5,8.5),
                   control = c(13,10.5,11,10,14,12,11,9.5,18.5,17,10,11))

summary_corn <- corn %>%
    tidyr::pivot_longer(nematode:control) %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(mean = mean(value), sd = sd(value))

# plot corn
corn %>%
    tidyr::pivot_longer(nematode:control) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(mean_value = mean(value)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = factor(name, levels = c("nematode", "wasps", "nem_wasp", "bacteria", "control")), 
                 y = value, fill = name) +
    ggplot2::geom_jitter(width = 0.1, aes(color = name)) +
    ggplot2::geom_point(aes(x = name, y = mean_value), shape = 24, size = 3) +
    # ggplot2::geom_boxplot(alpha = 0.5, outlier.color = NA) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Treatment", y = "Weight of ear of corn")
ggsave("corn1.png", height = 4, width = 7)

# calculate anova
long_corn <- corn %>%
    tidyr::pivot_longer(nematode:control, names_to = "treatment", values_to = "weight") 

# not significant
anova(lm(weight ~ treatment, data = long_corn))
aov(weight ~ treatment, data = long_corn)

# randomized
set.seed(76)
samples <- corn %>%
    tidyr::pivot_longer(nematode:control) %>%
    dplyr::mutate(rep1 = sample(name), 
                  rep2 = sample(name),
                  rep3 = sample(name))  
samples %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = factor(rep3, levels = c("nematode", "wasps", "nem_wasp", "bacteria", "control")), 
                 y = value, fill = rep3) +
    ggplot2::geom_jitter(width = 0.1, aes(color = name)) +
    ggplot2::geom_boxplot(alpha = 0.5, outlier.color = NA) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Treatment", y = "Weight of ear of corn")
ggsave("corn4.png", height = 4, width = 7)

samples %>%
    dplyr::select(rep1, rep2, rep3, value) %>%
    tidyr::pivot_longer(rep1:rep3, names_to = "rep", values_to = "name") %>%
    dplyr::group_by(rep, name) %>%
    dplyr::summarize(mean = mean(value)) %>%
    dplyr::group_by(rep) %>%
    dplyr::filter(mean == min(mean) | mean == max(mean))

# three examples:
set.seed(3)
df <- data.frame(a = rnorm(30, 30, 2),
                 b = rnorm(30, 30, 1),
                 c = rnorm(30, 30, 1),
                 d = rnorm(30, 30, 2))

df %>%
    tidyr::pivot_longer(a:d) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(avg = mean(value)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = name, 
                 y = value, fill = name) +
    ggbeeswarm::geom_beeswarm(aes(color = name)) +
    ggplot2::geom_point(aes(x = name, y = avg), shape = 24, size = 3, fill = "black") +
    # ggplot2::geom_jitter(width = 0.1, aes(color = name)) +
    # ggplot2::geom_boxplot(alpha = 0.5, outlier.color = NA) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Group", y = "Value")
ggsave("anova1.png", height = 3, width = 4)

df <- data.frame(a = rnorm(30, 29, 2),
                 b = rnorm(30, 28, 1),
                 c = rnorm(30, 33, 2),
                 d = rnorm(30, 34, 1))
keep <- df

df %>%
    tidyr::pivot_longer(a:d) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(avg = mean(value)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = name, 
                 y = value, fill = name) +
    ggbeeswarm::geom_beeswarm(aes(color = name)) +
    ggplot2::geom_point(aes(x = name, y = avg), shape = 24, size = 3, fill = "black") +
    # ggplot2::geom_jitter(width = 0.1, aes(color = name)) +
    # ggplot2::geom_boxplot(alpha = 0.5, outlier.color = NA) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Group", y = "Value")
ggsave("anova2.png", height = 3, width = 4)

df <- data.frame(a = rnorm(30, 29, 11),
                 b = rnorm(30, 28, 12),
                 c = rnorm(30, 33, 10),
                 d = rnorm(30, 34, 9.7))

df %>%
    tidyr::pivot_longer(a:d) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(avg = mean(value)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = name, 
                 y = value, fill = name) +
    ggbeeswarm::geom_beeswarm(aes(color = name)) +
    ggplot2::geom_point(aes(x = name, y = avg), shape = 24, size = 3, fill = "black") +
    # ggplot2::geom_jitter(width = 0.1, aes(color = name)) +
    # ggplot2::geom_boxplot(alpha = 0.5, outlier.color = NA) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Group", y = "Value")
ggsave("anova3.png", height = 3, width = 4)

# calculate anova with keep data
keep2 <- keep %>%
    tidyr::pivot_longer(a:d)

# weight gain example
weight <- data.frame(diet1 = c(8,16,9, NA,NA),
                     diet2 = c(9,16,21,11,18),
                     diet3 = c(15,10,17,6,NA)) %>%
    tidyr::pivot_longer(diet1:diet3, names_to = "diet", values_to = "weight") %>%
    na.omit()

# what does the anova look like in R?
anova(lm(weight ~ diet, data = weight))

weight %>%
    dplyr::group_by(diet) %>%
    dplyr::mutate(avg = mean(weight)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = diet, 
                 y = weight, fill = diet) +
    ggbeeswarm::geom_beeswarm(aes(color = diet)) +
    ggplot2::geom_point(aes(x = diet, y = avg), shape = 24, size = 3) +
    ggplot2::scale_color_manual(values = rev(gg_color_hue(3))) +
    ggplot2::scale_fill_manual(values = rev(gg_color_hue(3))) +
    # ggplot2::geom_jitter(width = 0.1, aes(color = name)) +
    # ggplot2::geom_boxplot(alpha = 0.5, outlier.color = NA) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme(axis.title.x = element_blank()) +
    ggplot2::labs(x = "Group", y = "Weight gain") +
    ggplot2::geom_hline(yintercept = 13, color = "red", size = 1.5)
ggsave("weight1.png", height = 3, width = 4)

# f distribution
data.frame(x = c(0,6)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = df, n = 100, args = list(df1 = 4, df2 = 20), size = 1.5, color = "grey50") + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme_bw(24) +
    ggplot2::theme(panel.grid = element_blank()) +
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks.y = element_blank())
ggsave("f_distribution1.png", height = 5, width = 10)

# show with F_0.05
data.frame(x = c(0,6)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = df, n = 100, args = list(df1 = 4, df2 = 20), size = 1.5, color = "grey50") + 
    ggplot2::stat_function(fun = df, n = 100, args = list(df1 = 4, df2 = 20), size = 1.5,
                           geom = "area", fill = "steelblue3", alpha = 0.7, xlim = c(2.87, 6)) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme_bw(24) +
    ggplot2::theme(panel.grid = element_blank()) +
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks.y = element_blank())
ggsave("f_distribution2.png", height = 5, width = 10)

# for weight gain
data.frame(x = c(0,6)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = df, n = 100, args = list(df1 = 2, df2 = 9), size = 1.5, color = "grey50") + 
    ggplot2::stat_function(fun = df, n = 100, args = list(df1 = 2, df2 = 9), size = 1.5,
                           geom = "area", fill = "steelblue3", alpha = 0.7, xlim = c(0.77, 6)) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme_bw(24) +
    ggplot2::theme(panel.grid = element_blank()) +
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks.y = element_blank())
ggsave("f_distribution3.png", height = 5, width = 10)

# checking assumptions for anova
# residuals plot

# good example - corn!
corn %>%
    tidyr::pivot_longer(nematode:control) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(mean_value = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(residual = value - mean_value) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = factor(name, levels = c("nematode", "wasps", "nem_wasp", "bacteria", "control"),
                            labels = c("N", "W", "N+W", "B", "C")), 
                 y = value, fill = name) +
    ggplot2::geom_jitter(width = 0.1, aes(color = name), size = 3) +
    # ggplot2::geom_point(aes(x = name, y = mean_value), shape = 24, size = 3) +
    # ggplot2::geom_boxplot(alpha = 0.5, outlier.color = NA) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Treatment", y = "Weight of ear of corn")
ggsave("corn5.png", height = 5, width = 5)

corn %>%
    tidyr::pivot_longer(nematode:control) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(mean_value = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(residual = value - mean_value) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = mean_value, 
                 y = residual, fill = name) +
    ggplot2::geom_jitter(width = 0.1, aes(color = name), size = 3) +
    # ggplot2::geom_point(aes(x = name, y = mean_value), shape = 24, size = 3) +
    # ggplot2::geom_boxplot(alpha = 0.5, outlier.color = NA) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Group mean", y = "Residual weight") +
    ggplot2::geom_hline(yintercept = 0, size = 1.5)
ggsave("corn6.png", height = 5, width = 5)


# bad example
set.seed(5)
fake_data <- data.frame(A = rnorm(10,40,3),
                        B = rnorm(10, 27, 1),
                        C = rnorm(10, 21, 0.5),
                        D = rnorm(10, 41, 5),
                        E = rnorm(10, 50, 10)) %>%
    tidyr::pivot_longer(A:E)

fake_data %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = name, y = value, color = name) +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Group", y = "Value")
ggsave("residual1.png", height = 5, width = 5)

fake_data %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(avg = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(residual = value - avg) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = avg, y = residual, color = name) +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Group mean", y = "Residual") +
    ggplot2::geom_hline(yintercept = 0, size = 1.5)
ggsave("residual2.png", height = 5, width = 5)

# calculate anova with R
corn_long <- corn %>%
    tidyr::pivot_longer(nematode:control, names_to = "treatment", values_to = "weight")
anova(lm(weight ~ treatment, data = corn_long))

# TWO_WAY ANOVA - leaf area
leaf_area <- data.frame(Control_low = c(264,200,225,268,215,241,232,256,229,288,253,288,230),
                        Stress_low = c(235,188,195,205,212,214,182,215,272,163,230,255,202),
                        Control_mod = c(314,320,310,340,299,368,345,271,285,309,337,282,273),
                        Stress_mod = c(283,312,291,259,216,201,267,326,241,291,269,282,257)) %>%
    tidyr::pivot_longer(Control_low:Stress_mod, names_to = "conditions", values_to = "leaf_area") %>%
    tidyr::separate(conditions, into = c("treatment", "light")) %>%
    dplyr::mutate(light = ifelse(light == "mod", "Moderate light", "Low light"))

leaf_area %>%
    dplyr::group_by(treatment, light) %>%
    dplyr::mutate(avg = mean(leaf_area)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = treatment, y = leaf_area) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_point(size = 5, shape = 24, aes(x = treatment, y = avg), fill = "red") +
    ggplot2::facet_grid(~light) +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Treatment", y = "Leaf area")
ggsave("leaf_area.png", height = 4, width = 8)

leaf_area %>%
    dplyr::group_by(treatment, light) %>%
    dplyr::mutate(avg = mean(leaf_area)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = treatment, y = leaf_area, color = light) +
    ggplot2::geom_point(size = 3, position = position_dodge(width = 0.5), alpha = 0.5) +
    ggplot2::geom_point(size = 5, shape = 24, aes(x = treatment, y = avg, fill = light),
                        color = "black", position = position_dodge(width = 0.5)) +
    ggplot2::theme_bw(20) +
    ggplot2::scale_color_manual(name = "Light", values = c("palevioletred1", "dodgerblue")) +
    ggplot2::scale_fill_manual(name = "Light",values = c("palevioletred1", "dodgerblue")) +
    ggplot2::labs(x = "Treatment", y = "Leaf area")
ggsave("leaf_area2.png", height = 4, width = 8)

summary(lm(leaf_area ~ treatment + light, data = leaf_area))
anova(aov(leaf_area ~ treatment*light, data = leaf_area), type = 3)
leaf_area <- leaf_area %>%
    dplyr::rename(stress = treatment)
anova(lm(leaf_area~stress*light, data = leaf_area))

# easy way to make an interaction plot! put in practicum...
interaction.plot(leaf_area$light, leaf_area$treatment, leaf_area$leaf_area)

# use anova to compare models

# graphical view of anova - distributions
data.frame(x = c(-3,3)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dnorm, 
                           n = 100, 
                           args = list(mean = 0, sd = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme_bw(24) +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3))
ggsave("normal.png", height = 4, width = 8)

# using anova to compare models
model1 <- lm(leaf_area ~ light, data = leaf_area)
model2 <- lm(leaf_area ~ stress, data = leaf_area)
model3 <- lm(leaf_area ~ stress + light, data = leaf_area)
model4 <- lm(leaf_area ~ stress * light, data = leaf_area)

anova(model1, model3)
anova(model3, model4)

# tukey HSD test
TukeyHSD(aov(leaf_area ~ light + stress + light*stress, data = leaf_area))
TukeyHSD(aov(leaf_area ~ light + stress, data = leaf_area))

pairwise.t.test(leaf_area$leaf_area, leaf_area$light)

TukeyHSD(aov(weight ~ treatment, data = long_corn))
