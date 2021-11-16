# Linear model demo

# LOAD AND INSPECT STOPPING DISTANCE DATA
data(cars)
?cars
head(cars)
plot(cars)

# COMPUTING REGRESSION LINE BY HAND
Y <- cars$dist
X <- cars$speed
n <- nrow(cars)
r <- cor(X, Y)

beta1 <- r*sd(Y)/sd(X)
beta0 <- mean(Y) - beta1*mean(X)
fit <- beta0 + beta1*X

# plot regression line
# a = intercept, b = slope
abline(a = beta0, b = beta1, col = 2)

# use lm()
model <- lm(Y ~ X)
model
model <- lm(dist ~ speed, data = cars)

# look at summary
summary(model)

# plot model
plot(model)

# bonus: plotting with ggplot2
ggplot2::ggplot(cars) +
    ggplot2::aes(x = speed, y = dist) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() + # I just think this theme should be default...
    ggplot2::geom_smooth(method = "lm", se = F) # can use se = T to show error
