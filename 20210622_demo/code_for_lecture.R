# linear regression - happiness data
library(tidyverse)

# set working directory
setwd("~/Dropbox/AndersenLab/LabFolders/Katie/presentations/workshops/DGP_stats_demo/")

# upload data from internet
temp <- tempfile()
download.file("https://cdn.scribbr.com/wp-content/uploads//2020/02/income.data_.zip", temp)
income_data <- read.csv(unz(temp, "income.data.csv")) %>%
    dplyr::select(-X)
unlink(temp)

# check out the data
summary(income_data$income)
summary(income_data$happiness)

# plot data distribution
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

# linear model
lm(happiness ~ income, data = income_data)

income_data %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = income, y = happiness) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none",
          panel.grid = element_blank())
ggplot2::ggsave("happiness_income_plot.png", height = 8, width = 12)

income_data %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = income, y = happiness) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::theme(legend.position = "none",
          panel.grid = element_blank()) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "red", fill = "red")
ggplot2::ggsave("happiness_income_plot3.png", height = 8, width = 12)

# get more info from the linear model
summary(lm(happiness ~ income, data = income_data))

# predict
model <- lm(happiness ~ income, data = income_data)
predicted_data <- data.frame(income = seq(1.5, 7.5, 0.1)) %>%
    dplyr::mutate(predicted_happiness = predict(model, newdata = .))

predicted_data %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = income, y = predicted_happiness) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20)
ggplot2::ggsave("happiness_predict.png", height = 8, width = 12)

# plot residual data
residuals <- broom::augment(model)

residuals %>%
    ggplot(.) +
    ggplot2::aes(x = income, y = .resid) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    labs(y = "residual happiness")
ggplot2::ggsave("residuals.png", height = 8, width = 12)
