# lecture 14 script
library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/figures"))

# residual plots
curves <- data.frame(x = c(0.5, 0.75, 1, 1.2,1.5,1.6,1.8,2,2.2,2.5,2.6,2.7,3,3.1, 3.2,3.4,3.7,4,4.25,4.5,5,5.1,5.3,5.4,5.6,6),
                     y = c(1,5,4.5,6.5,10,8,10.5,11,12,13,14.5,16.5,15,15.5,17,16,17.5,17.5,16.5,18,17.5,19,19,20,19,18))

a <- curves %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = y) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::geom_smooth(method = "lm", se = F) +
    ggplot2::labs(x = "X", y = "Y")

resid <- broom::augment(lm(y~x, data = curves))

b <- resid %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = .resid) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2)+ 
    # ggplot2::geom_smooth(method = "lm", se = F) +
    ggplot2::labs(x = "X", y = "Residuals")

cowplot::plot_grid(a,b,nrow = 1)
ggsave("residuals1.png", height = 2.6, width = 7)

# plot QQ for resids
resid %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(sample = .fitted) +
    # ggplot2::geom_point() +
    ggplot2::geom_qq_line(size = 2, color = "red") +
    ggplot2::geom_qq(size = 3) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    # ggplot2::geom_hline(yintercept = 0, linetype = 2)+ 
    # ggplot2::geom_smooth(method = "lm", se = F) +
    ggplot2::labs(x = "Theoretical X", y = "Standardized residuals")
ggsave("residuals_QQ.png", height = 3.5, width = 5)

# good residuals:
df4 <- df4 %>%
    dplyr::mutate(income = income + 3)
happy_resid <- broom::augment(lm(happiness ~ income, df4))

happy_resid %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = income, y = .resid) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank()) +
                   #axis.text = element_blank(),
                   #axis.ticks = element_blank()) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2)+ 
    # ggplot2::geom_smooth(method = "lm", se = F) +
    ggplot2::labs(x = "Income", y = "Residual Happiness")
ggsave("happiness_resid.png", height = 4, width = 6)

# transform data?
curves %>%
    tidyr::pivot_longer(x:y) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = value) +
    ggplot2::geom_histogram(bins = 10, fill = "grey70", color = "black") +
    ggplot2::facet_grid(~name, scales = "free") +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.x = element_blank()) +
    ggplot2::labs(x = "Value", y = "Frequency")
ggsave("transformation1.png", height = 4, width = 6)

# plot Y2
transform <- curves %>%
    dplyr::mutate(new_y = y^3)
a <- transform %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = new_y) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::geom_smooth(method = "lm", se = F) +
    ggplot2::labs(x = "X", y = expression(Y^3))

resid <- broom::augment(lm(new_y~x, data = transform))

b <- resid %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = .resid) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2)+ 
    # ggplot2::geom_smooth(method = "lm", se = F) +
    ggplot2::labs(x = "X", y = "Residuals")

cowplot::plot_grid(a,b,nrow = 1)
ggsave("residuals2.png", height = 2.6, width = 7)

# better example:
growth <- data.frame(x = c(rep(12,5), rep(24,5), rep(28,5), rep(32,5), rep(35,5)),
                     y = c(rnorm(5,0.5,0.1),rnorm(5,2.5,0.5),rnorm(5,4,1),rnorm(5,8,1.5),rnorm(5,12,2.5)))

a <- growth %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = y) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(12) +
    ggplot2::theme(panel.grid = element_blank()) +
    # ggplot2::geom_smooth(se = F) +
    ggplot2::labs(x = "Days of growth", y = "Dry weight (g)")

b <- growth %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = log(y)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(12) +
    ggplot2::theme(panel.grid = element_blank()) +
    ggplot2::geom_smooth(method = "lm", se = F) +
    ggplot2::labs(x = "Days of growth", y = "Log(dry weight)")

cowplot::plot_grid(a, b, nrow = 1)
ggsave("residuals3.png", height = 2.6, width = 7)

resid_a <- broom::augment(lm(y~x, data = growth)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = .resid) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(12) +
    ggplot2::theme(panel.grid = element_blank()) +
    # ggplot2::geom_smooth(se = F) +
    ggplot2::labs(x = "Days of growth", y = "Residual weight")

new <- growth %>%
    dplyr::mutate(y = log(y))
resid_b <- broom::augment(lm(y~x, data = new)) %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = x, y = .resid) +
    ggplot2::geom_point() +
    ggplot2::theme_bw(12) +
    ggplot2::theme(panel.grid = element_blank()) +
    # ggplot2::geom_smooth(se = F) +
    ggplot2::labs(x = "Days of growth", y = "Residual log(weight)")

cowplot::plot_grid(resid_a, resid_b, nrow = 1)
ggsave("residuals4.png", height = 2.6, width = 7)


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
    ggplot2::geom_point(size = 2) +
    ggplot2::theme_bw(20) +
    ggplot2::geom_point(aes(x = 0, y = mean(y1)), shape = 24, fill = "red", size = 4) +
    ggplot2::geom_point(aes(x = 1, y = mean(y2)), shape = 24, fill = "red", size = 4) +
    ggplot2::geom_smooth(method = "lm", se = F) +
    ggplot2::labs(x = "Butterfly group", y = "Wing area")
ggsave("butterfly2.png", height = 4, width = 5)

summary(lm(area ~ butterfly, data = butterfly))

# when would regression > t test
df5 <- data.frame(x = c(130, 131, 150, 155, 159, 170, 179, 180, 181, 183, 190, 195, 199,
                        205, 207, 212,215, 220, 223, 225),
                  y = c(70, 76, 79,83,80,74,72,84,80,83,77,90,95,92,91,87,92,85,96,93),
                  patient = c(rep("Subject A", 10), rep("Subject B", 10)))
df5 %>%
    dplyr::group_by(patient) %>%
    dplyr::summarize(mean = mean(y), sd = sd(y), num = n())
subjectA <- df5 %>% dplyr::filter(patient == "Subject A") %>% dplyr::pull(y)
subjectB <- df5 %>% dplyr::filter(patient == "Subject B") %>% dplyr::pull(y)
t.test(subjectA, subjectB)

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
# ggsave("cholesterol5.png", height = 4, width = 8)
ggsave("cholesterol6.png", height = 4, width = 5)

summary(lm(y~x, data = df5 %>% dplyr::filter(patient == "Subject A")))
summary(lm(y~x, data = df5 %>% dplyr::filter(patient == "Subject B")))

df <- df5 %>%
    dplyr::select(cholesterol = x, glucose = y, subject = patient) %>%
    dplyr::mutate(subject = ifelse(subject == "Subject A", "A", "B"))
summary(lm(glucose ~ cholesterol + subject, data = df))
plot(lm(glucose ~ cholesterol + subject, data = df))


# regression with set X values
set.seed(1)
drug <- data.frame(dose = c(rep(0,5), rep(5,5), rep(10,5))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(y = rnorm(1, 20-dose, 2))

drug %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = dose, y = y) +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank()) +
    ggplot2::geom_smooth(method = "lm", se = F, size = 2) +
    ggplot2::labs(x = "Dose (mg)", y = "Phenotype")
ggsave("discrete_x2.png", height = 4, width = 5)


### logistic regression
metastasis <- read.csv("../metastasis.csv")

m2 <- data.frame(Metastasis = c(rep(0,15), rep(1, 15)),
                 Size = c(rnorm(15, 3.5, 3), rnorm(15,5.5,2)))

metastasis %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = Size, y = Metastasis) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1)) +
    ggplot2::scale_x_continuous(breaks = c(2,4,6,8)) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank()) +
    ggplot2::geom_smooth(method = "lm", se = "F", size = 2) +
    ggplot2::labs(x = "Tumor size (cm)", y = "Metastasis")
ggsave("metastasis.png", height = 4, width = 6)

model <- glm(Metastasis ~ Size, data = metastasis, family = "binomial")
summary(model)

test <- data.frame(Size = seq(0,12,0.1))
Y <- predict(model, test)
test$Metastasis <- Y

test %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = Size, y = Metastasis) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1)) +
    ggplot2::scale_x_continuous(breaks = c(2,4,6,8)) +
    ggplot2::theme_bw(20) +
    ggplot2::theme(panel.grid = element_blank()) +
    # ggplot2::geom_smooth(method = "glm", se = "F", size = 2) +
    ggplot2::labs(x = "Tumor size (cm)", y = "Metastasis")


### refresher
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
    ggplot2::geom_smooth(method = "lm", se = F)+
    ggplot2::labs(x = "X", y = "Y")
ggsave("refresher.png", height = 4, width = 4)

cor(ca$x, ca$y)
cor.test(ca$x, ca$y)

summary(lm(ca$y ~ ca$x))
