# lecture12 script
library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/figures"))

# show a few examples of relationships between x and y
df1 <- data.frame(x = c(rep(0, 8), rep(2.5, 8), rep(5, 8)),
                  y = c(112,102,90,81,105,93,106,108,73,84,67,55,80,90,75,72,38,81,57,62,51,48,42,57))
plot_df1 <- df1 %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = y) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Dose (mg/kg)", y = "Food consumption (g/kg)")

df2 <- data.frame(x = rnorm(50, 14, 3), 
                  y = rnorm(50, 8, 2))
plot_df2 <- df2 %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = y) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Temperature (C)", y = "Dissolved Oxygen (mg/L)")

df3 <- data.frame(x = c(60,69,66,64,54,67,59,65,63),
                  y = c(136,198,194,140,93,172,116,174,145))

plot_df3 <- df3 %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = y) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Length (cm)", y = "Weight (g)")

cowplot::plot_grid(plot_df1, plot_df2, plot_df3, nrow = 1)
ggsave("relationships.png", height = 4, width = 12)

# just plot weight/length
plot_df3
ggsave("linear_relationship1.png", height = 4, width = 4)

plot_df3 +
    ggplot2::geom_vline(xintercept = mean(df3$x), size = 1.5, color = "green3", linetype = 2) +
    ggplot2::geom_hline(yintercept = mean(df3$y), size = 1.5, color = "purple", linetype = 2)
ggsave("linear_relationship2.png", height = 4, width = 4)

df3 <- df3 %>%
    dplyr::mutate(x_dev = x - mean(x),
                  y_dev = y - mean(y),
                  prod = x_dev * y_dev)
sum(df3$prod)

# change units
df3 <- df3 %>%
    dplyr::mutate(x_in = x*0.393701,
                  y_lb = y*0.00220462,
                  x_dev_in = x_in - mean(x_in),
                  y_dev_lb = y_lb - mean(y_lb),
                  prod_in = x_dev_in * y_dev_lb)
sum(df3$prod_in)

df3 %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x_in, y = y_lb) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Length (in)", y = "Weight (lb)") +
    ggplot2::geom_vline(xintercept = mean(df3$x_in), size = 1.5, color = "green3", linetype = 2) +
    ggplot2::geom_hline(yintercept = mean(df3$y_lb), size = 1.5, color = "purple", linetype = 2)
ggsave("linear_relationship3.png", height = 4, width = 4)

# correlation
sum(df3$prod_in)/8/(sd(df3$x_in)*sd(df3$y_lb))
cor(df3$x_in, df3$y_lb)
cor(df3$x, df3$y)

sum(df3$prod)/8/(sd(df3$x)*sd(df3$y))

# correlation with z scores
df3 <- df3 %>%
    dplyr::mutate(std_x = x_dev / sd(x),
                  std_y = y_dev / sd(y))
df3 %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = std_x, y = std_y) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Standard length", y = "Standard weight") +
    ggplot2::geom_vline(xintercept = mean(df3$std_x), size = 1.5, color = "green3", linetype = 2) +
    ggplot2::geom_hline(yintercept = mean(df3$std_y), size = 1.5, color = "purple", linetype = 2)
ggsave("linear_relationship4.png", height = 4, width = 4)

###################################
# Hypothesis testing: data
set.seed(808)
df4 <- data.frame(x = rnorm(38, 97, 5)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(y = x + 10, 
                  z = rnorm(1, 0, 7),
                  y = y + z)

# what is the correlation
cor(df4$x, df4$y)

df4 %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = y) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Blood pressue (mm Hg)", y = "Platelet calcium (nM)")
ggsave("hypothesis_test1.png", height = 4, width = 5)


# permutations
set.seed(100)
cors <- lapply(1:10000, FUN = function(i) {
    new <- data.frame(x = df4$x,
                      y = sample(df4$y, replace = F))
    cor(new$x, new$y)
})

cors2 <- unlist(cors)

# how many of perms are >= cor val?
sum(abs(cors2) >= cor(df4$x, df4$y))/10000

# t test
cor_val <- 0.529041
ts <- cor_val*sqrt(36/(1-cor_val^2))

# student t test, df = 36, ts = 3.74
data.frame(x = c(-5,5)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dt,
                           n = 100,
                           geom = "area",
                           args = list(df = 36),
                           xlim = c(-5, qt(0.025,36)),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dt,
                           n = 100,
                           geom = "area",
                           args = list(df = 36),
                           xlim = c(qt(0.975,36), 5),
                           alpha = 0.7,
                           fill = "steelblue3", size = 1.5) +
    ggplot2::stat_function(fun = dt, 
                           n = 100, 
                           args = list(df = 36),
                           color = "grey50", size = 1.5) + 
    ggplot2::geom_vline(xintercept = c(3.74, -3.74), color = "blue", linetype = 2, size= 1.5) +
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks.y = element_blank())
ggsave("ttest1.png", height = 4, width = 5)


### regression line
temp <- data.frame(c = rnorm(15, 15, 2)) %>%
    dplyr::mutate(f = 32 + (9/5)*c)

temp %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = c, y = f) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Degrees (Celsius)", y = "Degrees (Fahrenheit)")
ggsave("temp1.png", height = 4, width = 5)

temp %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = c, y = f) +
    ggplot2::geom_smooth() +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Degrees (Celsius)", y = "Degrees (Fahrenheit)")
ggsave("temp2.png", height = 4, width = 5)

temp %>%
    dplyr::mutate(meanx = mean(c), meany = mean(f)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = c, y = f) +
    ggplot2::geom_smooth() +
    ggplot2::geom_point() +
    ggplot2::geom_point(aes(x = mean(c), y = mean(f)), shape = 24, fill = "red", size = 2) +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Degrees (Celsius)", y = "Degrees (Fahrenheit)")
ggsave("temp3.png", height = 4, width = 5)

temp %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = c, y = f) +
    ggplot2::geom_smooth() +
    ggplot2::geom_point() +
    ggplot2::geom_point(aes(x = mean(c), y = mean(f)), shape = 24, fill = "red", size = 2) +
    ggplot2::geom_vline(xintercept = 17.07002) +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "Degrees (Celsius)", y = "Degrees (Fahrenheit)")
ggsave("temp4.png", height = 4, width = 5)

# find something with lower r value
income_data <- read.csv("https://raw.githubusercontent.com/katiesevans/IGP_biostatistics/main/archive/20210622_demo/income.data.csv") %>%
    dplyr::select(-X)
cor(income_data$income, income_data$happiness)

# i <- 6374
# cor <- 0
# while(cor < 0.5) {
#     set.seed(i)
#     df4 <- income_data %>%
#         dplyr::rowwise() %>%
#         dplyr::mutate(income = income + rnorm(1, 0, 2))
#     cor <- cor(df4$income, df4$happiness)
#     i <- i + 1
# }
set.seed(12866)
df4 <- income_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(income = income + rnorm(1, 0, 2) + 4.5)
df4 %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = income, y = happiness) +
    ggplot2::geom_smooth(method = "lm", se = F, size = 2) +
    ggplot2::geom_point() +
    ggplot2::geom_point(aes(x = mean(income), y = mean(happiness)), shape = 24, fill = "red", size = 3) +
    # ggplot2::geom_vline(aes(xintercept = mean(income) + sd(income))) +
    # ggplot2::geom_hline(aes(yintercept = mean(happiness) + sd(happiness))) +
    # ggplot2::geom_vline(aes(xintercept = mean(income) - sd(income))) +
    # ggplot2::geom_hline(aes(yintercept = mean(happiness) - sd(happiness))) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank()) +
    ggplot2::labs(x = "Income ($10,000)", y = "Happiness")
cor(df4$income, df4$happiness)
# ggsave("happiness4.png", height = 4, width = 6)

# ggsave("happiness5.png", height = 4, width = 5)

summary(lm(happiness ~ income, data = df4))

# beware of dependent variables
df5 <- data.frame(x = c(130, 131, 150, 155, 159, 170, 179, 180, 181, 183, 190, 195, 199,
                        205, 207, 212,215, 220, 223, 225),
                  y = c(70, 76, 79,83,80,74,72,84,80,83,77,90,95,92,91,87,92,85,96,93),
                  patient = c(rep("Subject A", 10), rep("Subject B", 10)))
df5 %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = y) +
    # ggplot2::aes(x = x, y = y, color = patient) +
    ggplot2::geom_smooth(method = "lm", se = F, size = 2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_manual(values = c("indianred1", "indianred4"),
                                name = "") +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank()) +
    ggplot2::labs(x = "Cholesterol (mg/dl)", y = "Glucose (mg/dl)")
ggsave("cholesterol2.png", height = 4, width = 6)

# ggsave("cholesterol3.png", height = 4, width = 8)

# regression and t-test
# butterflies
set.seed(76)
y1 <- rnorm(14, 32, 2.5)
y2 <- rnorm(12, 28, 3.4)

# t test
t.test(y1, y2)

# regression
butterfly <- data.frame(orange = y1, white = c(y2, NA, NA))  %>%
    tidyr::pivot_longer(orange:white, names_to = "butterfly", values_to = "area") %>%
    dplyr::mutate(butterfly = ifelse(butterfly == "orange", 0, 1)) %>%
    na.omit()

butterfly %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = butterfly, y = area) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::geom_point(aes(x = 0, y = mean(y1)), shape = 24, fill = "red", size = 3) +
    ggplot2::geom_point(aes(x = 1, y = mean(y2)), shape = 24, fill = "red", size = 3) +
    ggplot2::geom_smooth(method = "lm", se = F) +
    ggplot2::labs(x = "Butterfly group", y = "Wing area")
ggsave("butterfly2.png", height = 4, width = 5)

summary(lm(area ~ butterfly, data = butterfly))

# when would regression > t test
df5 %>%
    dplyr::group_by(patient) %>%
    dplyr::summarize(mean = mean(y), sd = sd(y), num = n())
subjectA <- df5 %>% dplyr::filter(patient == "Subject A") %>% dplyr::pull(y)
subjectB <- df5 %>% dplyr::filter(patient == "Subject B") %>% dplyr::pull(y)
t.test(subjectA, subjectB)

df5 %>%
    ggplot2::ggplot(.) +
    # ggplot2::aes(x = x, y = y) +
    ggplot2::aes(x = x, y = y, color = patient) +
    ggplot2::geom_smooth(method = "lm", se = F, size = 2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_manual(values = c("indianred1", "indianred4"),
                                name = "") +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank()) +
    ggplot2::labs(x = "Cholesterol (mg/dl)", y = "Glucose (mg/dl)")
ggsave("cholesterol4.png", height = 4, width = 8)
summary(lm(y~x, data = df5 %>% dplyr::filter(patient == "Subject A")))
summary(lm(y~x, data = df5 %>% dplyr::filter(patient == "Subject B")))

df <- df5 %>%
    dplyr::select(cholesterol = x, glucose = y, subject = patient) %>%
    dplyr::mutate(subject = ifelse(subject == "Subject A", "A", "B"))
summary(lm(glucose ~ cholesterol + subject, data = df))

#### spearman cor
set.seed(8)
x <- c(rnorm(48), 5, 7)
y <- c(rnorm(48), 5, 7)

ggplot2::ggplot(data.frame(x, y)) +
    ggplot2::aes(x = x, y = y) +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::labs(x = "X", y = "Y")

ggsave("spearmancor.png", height = 4, width = 5)

cor(x, y)
cor(x,y, method = "spearman")
cor(x,y, method = "kendall")

cor(x[1:48], y[1:48])

smoker <- data.frame(smoker = c(89, 6063), non = c(37, 5711))
fisher.test(smoker)

smoker2 <- data.frame(cancer = c(89, 37), healthy = c(6063, 5711))
fisher.test(smoker2)

# refresher
# chi square distribution df = 1
data.frame(x = c(0,45)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x) +
    ggplot2::stat_function(fun = dchisq, 
                           n = 100, 
                           args = list(df = 1),
                           color = "grey50", size = 1.5) + 
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme_bw(24) +
    ggplot2::theme(axis.text.y = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks.y = element_blank()) +
    ggplot2::geom_vline(xintercept = 42.9, color = "red", linetype = 2, size = 1)
ggsave("refresher.png", height = 4, width = 5)
