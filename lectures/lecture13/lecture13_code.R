# lecture 13 script
library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/figures"))

set.seed(16)
ca <- data.frame(x = rnorm(75, 2.5, 1.5)) %>%
    dplyr::mutate(y = 6000 - 1000*x) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(y = y + sample(c(1,-1), 1)*rnorm(1, 500, 500),
                  x = x + rnorm(1, 2, 3) + 3)

ca %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = y) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::labs(x = "X", y = "Y")
ggsave("refresher.png", height = 4, width = 4)

cor(ca$x, ca$y)
cor.test(ca$x, ca$y)

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

# i = 18
i <- 19
cor <- 0
while(cor < 0.5 | cor > 0.51) {
    set.seed(i)
    df4 <- income_data %>%
        dplyr::rowwise() %>%
        dplyr::mutate(income = income + rnorm(1, 0, 2))
    cor <- cor(df4$income, df4$happiness)
    i <- i + 1
}
set.seed(18)
df4 <- income_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(income = income + rnorm(1, 0, 2) + 3.5)
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

# residual sum of squares:
df3 <- data.frame(x = c(60,69,66,64,54,67,59,65,63),
                  y = c(136,198,194,140,93,172,116,174,145))

set.seed(17812)
ssresid <- data.frame(x = rnorm(20)) %>%
    dplyr::mutate(y = 3*x + 5)

a <- ssresid %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = y) +
    # ggplot2::geom_smooth(se = F, size = 2, method = "lm") +
    ggplot2::geom_point(size = 2) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::labs(x = "X", y = "Y")

b <- ssresid %>%
    dplyr::rowwise() %>%
    dplyr::mutate(y = y + rnorm(1, 0, 2)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = y) +
    # ggplot2::geom_smooth(se = F, size = 2, method = "lm") +
    ggplot2::geom_point(size = 2) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::labs(x = "X", y = "Y")

c <- ssresid %>%
    dplyr::rowwise() %>%
    dplyr::mutate(y = y + rnorm(1, 0, 0.5)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = y) +
    # ggplot2::geom_smooth(se = F, size = 2, method = "lm") +
    ggplot2::geom_point(size = 2) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::labs(x = "X", y = "Y")

cowplot::plot_grid(a, c, b, nrow = 1)
ggsave("show_ssresid.png", height = 3, width = 8)

# calculate ssresid in R:
deviance(lm(y~x, data = ssresid))
deviance(lm(y~x, data = c))
deviance(lm(y~x, data = b))

sd(ssresid$y)
sd(c$y)
sd(b$y)

# correlation
cor(ssresid$x, ssresid$y)
cor(c$x, c$y)
cor(b$x, b$y)

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

# example for calculations
load("~/Documents/git/IGP_biostatistics/lectures/practicum02/all_genex_data.RData")
long <- expression_df %>%
    tidyr::pivot_longer(-gene, names_to = "subject", values_to = "expression") %>%
    dplyr::filter(gene %in% c("WNT10B", "CYP2C19")) %>%
    tidyr::pivot_wider(names_from = gene, values_from = expression)

long %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = WNT10B, y = CYP2C19) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::geom_smooth(se = F, method = "lm") +
    ggplot2::theme(panel.grid = element_blank())
ggsave("genex_example2.png", height = 4, width = 5)

# for(g1 in unique(long$gene)[1:20]) {
#     for(g2 in unique(long$gene)[1:20]) {
#         print(paste0(g1, "-", g2))
#         print(cor(dplyr::filter(long, gene == g1)$expression, dplyr::filter(long, gene == g2)$expression))
#     }
#     }

mean(long$CYP2C19)
sd(long$CYP2C19)
mean(long$WNT10B)
sd(long$WNT10B)

# [1] "WNT10B-CYP2C19"
# [1] 0.7086126