# Applied

#### Question 8 ####
### a ###
main.dir = getwd()
setwd(
  'Documents/Training/An Introduction to Statistical Learning V2 - 2021/ALL CSV FILES - 2nd Edition/'
)
auto = read.csv('Auto.csv', header = TRUE, na.strings = "?")
auto = na.omit(auto)
setwd(main.dir)
getwd()
attach(auto)
summary(auto)

fit = lm(mpg~horsepower,data=auto)
summary(fit)
## i ##
# there is relattionship between predictor and response because of it's p-value.

## ii ##
# R-squared is 0.6059. this means 60% variability of mpg can be explained by horsepower. there are 
# strong relationship based on p-value of horsepower.

## iii ##
# based on beta1 of horsepower, their relationship is negative.

## iv ##
predict(fit,newdata = data.frame(horsepower=c(98)),interval = 'prediction')
# conf int. 95%:
predict(fit,newdata = data.frame(horsepower=c(98)),interval = 'confidence')

### b ###
plot(mpg~horsepower,col='darkgray',main= "Scatter plot of mpg vs. horsepower")
abline(fit,col='red')

### c ###
par(mfrow=c(2,2))
plot(fit)
# Residual vs. Fitted show non-linear relationship
# Some observations are potential outliers:
rstudent(fit)[which(rstudent(fit)>3)]

#### Question 9 ####
### a ###
pairs(auto,col='blue')

### b ###
cor(subset(auto,select=-name))

### c ###
fit2 = lm(mpg~.-name,data=auto)
summary(fit2)

## i ##
# p.val of F-statistics is 2.2e-16. so there is strong relationship between predictors and
# response

## ii ##
# Displacement, weight, year and origin are statistically significant as their p-values are
# below 0.05

## iii ##
# coefficient for year is 0.75. this means assuming all other variables fixed, after one year
# mpg will increase by 0.75.

### d ###
par(mfrow=c(2,2))
plot(fit2)
# Residual vs. Fitted show non-linear relationship
# in Residual vs. Leverage -> 14th observation has high leverage.
# Some observations are potential outliers:
rstudent(fit2)[which(rstudent(fit2)>3)]

### e ###
fit3 = lm(mpg~.-name+year:cylinders+acceleration:horsepower,data=auto)
summary(fit3)
# cylinders:year and horsepower:acceleration are statistically significant. 
# R-squared has increased from 0.82 to 0.85.

### f ###
fit4 = lm(mpg~.-name+year:cylinders+acceleration:horsepower+I(horsepower^2),data=auto)
summary(fit4)
fit5 = lm(mpg~.-name-cylinders-horsepower-displacement+log(weight)+log(acceleration)+sqrt(origin),data=auto)
summary(fit5)

