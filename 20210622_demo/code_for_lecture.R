# linear regression - happiness data
library(tidyverse)
# if you don't want to load tidyverse:
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(broom)

# set working directory
##### EDIT OR COMMENT OUT ####
setwd("<working_dir_here>")

###########################
#       load data         #
###########################

# upload data from class github
income_data <- read.csv("https://raw.githubusercontent.com/katiesevans/IGP_biostatistics/main/20210622_demo/income.data.csv") %>%
    dplyr::select(-X)

###########################
#   Initial data check    #
###########################

# check out the data
summary(income_data$income)
summary(income_data$happiness)

# plot data distribution
hist(income_data$income)
hist(income_data$happiness)

# plot with ggplot
income_data %>%
    tidyr::gather(var, value) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = value, fill = var) +
    ggplot2::geom_histogram(bins = 20) +
    ggplot2::facet_grid(~factor(var, levels = c("income", "happiness")), scales = "free") +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none",
          panel.grid = element_blank()) +
    ggplot2::scale_fill_manual(values = c("happiness" = "darkslateblue", "income" = "darkslategray3")) +
    ggplot2::labs(x = "", y = "Frequency")
ggsave("plots/data_distribution.png", height = 6, width = 10)

###########################
#    Plot linear model    #
###########################

# linear model
lm(happiness ~ income, data = income_data)

# plot to see if there is a linear relationship
income_data %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = income, y = happiness) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none",
          panel.grid = element_blank())
ggplot2::ggsave("happiness_income_plot.png", height = 6, width = 10)

# plot to see if linear relationship (base R)
plot(income_data$income, income_data$happiness)

# plot linear regression model with data
income_data %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = income, y = happiness) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none",
                   panel.grid = element_blank()) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ x, color = "red", fill = "red")
ggplot2::ggsave("plots/happiness_income_plot2.png", height = 6, width = 10)

# plot linear regression model with data in base R
plot(income_data$income, income_data$happiness)
abline(lm(happiness ~ income, data = income_data))

# predict one value - more on prediction below!
# needs a data frame for a value... if none given, uses the fitted values along the regression line 
predict(model, data.frame(income = 5))

# plot linear regression model with data and SE
income_data %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = income, y = happiness) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none",
          panel.grid = element_blank()) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "red", fill = "red")
ggplot2::ggsave("plots/happiness_income_plot3.png", height = 6, width = 10)
ggplot2::ggsave("plots/happiness_income_plot4.png", height = 4, width = 4)

# get more info from the linear model
summary(lm(happiness ~ income, data = income_data))

# predict happiness given set income values
model <- lm(happiness ~ income, data = income_data)

###########################
#       Residuals         #
###########################

# plot residual data
residuals <- income_data %>%
    dplyr::mutate(resid = resid(model))

# another cool way to get tidy residuals and other stats from a linear model!
residuals <- broom::augment(model)

# plot residuals
residuals %>%
    ggplot(.) +
    ggplot2::aes(x = income, y = .resid) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    labs(y = "residual happiness")
ggplot2::ggsave("plots/residuals.png", height = 6, width = 10)

# plot residuals base R
plot(residuals$income, residuals$.resid)

###########################
#    Regression v. Cor    #
###########################

# correlation vs. regression
# regression coefficient = 0.71
lm(happiness ~ income, data = income_data)

# correlation = 0.865
cor(income_data$income, income_data$happiness)
# correlation*correlation = variance explained! (0.75)
0.8656337^2

###########################
#     Extra prediction    #
###########################

# seq(1.5, 7.5, 0.1) - produces a vector of 1.5, 1.6, 1.7, 1.8, etc. to use as potential incomes
predicted_data <- data.frame(income = seq(1.5, 7.5, 0.1)) %>%
    dplyr::mutate(predicted_happiness = predict(model, newdata = .))

# plot predicted data - should line up with our regression line
predicted_data %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = income, y = predicted_happiness) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20)
ggplot2::ggsave("plots/happiness_predict.png", height = 6, width = 10)

# plot predicted data in base R
plot(predicted_data$income, predicted_data$predicted_happiness)



