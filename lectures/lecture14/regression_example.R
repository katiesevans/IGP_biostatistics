# multiple regression demo
library(ggplot2) # for prettier plotting

# car data
data(mtcars)
?mtcars
str(mtcars)

# "factor" tells R to treat it as a categorical variable
mtcars$trans <- factor(mtcars$am, labels = c("auto","stick"))

# hp vs mpg, linear fit
plot(mtcars$hp, mtcars$mpg)

# prettier plot with ggplot qplot
ggplot2::qplot(hp, mpg, data = mtcars)

# summary of model
summary(lm(mpg~hp,data=mtcars))

# mpg vs transmission as a categorical variable
ggplot2::qplot(trans, mpg, data = mtcars)
summary(lm(mpg~trans,data=mtcars))

# with both
qplot(hp,mpg,col=trans,data=mtcars)
summary( lm(mpg~trans+hp,data=mtcars))
qplot(hp,mpg,col=trans,data=mtcars) + geom_smooth(method="lm",se=F)


