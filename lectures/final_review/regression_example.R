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

# add categorical variable
summary( lm(mpg~hp+factor(gear),data=mtcars))
model <- lm(mpg~hp+factor(gear),data=mtcars)

hp <- 10
gear4 <- 0
gear5 <- 0

27.88193-0.06685*hp+2.63486*gear4+6.57476*gear5
predict(model, data.frame(hp = 10, gear = 3))

hp <- 10
gear4 <- 1
gear5 <- 0
27.88193-0.06685*hp+2.63486*gear4+6.57476*gear5

predict(model, data.frame(hp = 10, gear = 4))

model <- lm(mpg~hp*factor(gear),data=mtcars)
summary(model)

predict(model, data.frame(hp = 10, gear = 4))
hp <- 10
gear5 <- 0
gear4 <- 1
25.307903 -0.052240*hp +15.262616*gear4+7.469550*gear5-0.126946*(hp*gear4) -0.006029*(hp*gear5) 

model <- lm(mpg~trans*factor(gear),data=mtcars)
summary(model)



