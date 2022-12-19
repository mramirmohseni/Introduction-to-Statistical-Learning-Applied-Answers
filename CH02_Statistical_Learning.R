# Applied

#### Question 8 ####
### a ###
main.dir = getwd()
setwd(
  'Documents/Training/An Introduction to Statistical Learning V2 - 2021/ALL CSV FILES - 2nd Edition/'
)
college <- read.csv('College.csv', header = TRUE)
# change working directory to main
setwd(main.dir)
getwd()
attach(college)


### b ###
row.names(college) = college[,1]
fix(college)
college = college[,-1]
fix(college)

### c ###
## i ##
summary(college) 

## ii ##
pairs(college[,1:10])

## iii ##
plot(Private,Outstate)

## iv ##
Elite = rep('No',nrow(college))
Elite[college$Top10perc>50]='Yes'
Elite = as.factor(Elite)
college$Elite = Elite
summary(college)
plot(Elite,Outstate)

## v ##
par(mfrow=c(2,2))
hist(Apps, xlab = "Applications", xlim=c(0,25000), main="Apps using default bin sizes")
hist(Apps, xlab = "Applications", xlim=c(0,25000), main="Apps using small bin sizes",
     breaks = 25)
hist(Top10perc, xlab = "Pct. new students from top 10% of H.S. class", breaks = 25, main = "Top10Perc")
hist(Outstate, xlab = "Out-of-state tuition", ylab="Amount", main = "Outstate")

## vi ##
# to reset par
dev.off()
plot(S.F.Ratio,Grad.Rate, xlab = "Student to Faculty Ratio", ylab = "Graduation Ration",
     main="Plot of Grad.Rate vs S/F Ratio")
# linear regression line (explain in future chapters)
abline(lm(Grad.Rate~S.F.Ratio), col='red')
# Local Regression line with smoothing of 0.25 (explain in future chapters)
?loess()
local.reg.25 = loess(Grad.Rate~S.F.Ratio,data=college,span = 0.25)
names(local.reg.25)
j = order(S.F.Ratio)
lines(S.F.Ratio[j],local.reg.25$fitted[j],col='blue')

#### Question 9 ####
setwd(
  'Documents/Training/An Introduction to Statistical Learning V2 - 2021/ALL CSV FILES - 2nd Edition/'
)
auto <- read.csv('Auto.csv', header = TRUE)
setwd(main.dir)
getwd()
attach(auto)
summary(auto)
auto = na.omit(auto)
# here NA is shown by ?. so we remove them in this way:
auto = subset(auto, auto$horsepower != "?")

### a ###
# Quantitative: mpg,cylinders,displacement,horsepower, weight, acceleration, year
# Qualitative: name, origin

### b,c ###
# get min and max of each column
auto[,4] = as.numeric(as.character(auto[,4]))
{
  print(sapply(auto[,1:7],range))
  print(sapply(auto[,1:7],mean))
  print(sapply(auto[,1:7],sd))
}

### d ###
auto.reduced = auto[-c(10,85),]
{
  print(sapply(auto.reduced[,1:7],range))
  print(sapply(auto.reduced[,1:7],mean))
  print(sapply(auto.reduced[,1:7],sd))
}

### e ###
pairs(auto[,1:7])
cor(auto[,1:7])
# mpg has negative linear relationship with displacement, horsepower, weight (increase in mpg,
# will decrease displacement, horsepower, weight)
# mpg has positive acceleration with year (newer models have higher mpg)

#### Question 10 ####
### a ###
library(MASS)
?Boston
attach(Boston)
summary(Boston)
dim(Boston)

### b ###
pairs(~crim+nox+dis+tax+medv)

### c ###
cor(Boston[,1:14])


### d ###
high.crime = Boston[which(crim> mean(crim) + 2*sd(crim)),]
range(crim);mean(crim);sd(crim)
# 16 suburb with crime > mean+2*sd
high.tax = Boston[which(tax > mean(tax) + 2*sd(tax)),]
range(tax);mean(tax);sd(tax)
# no suburb with tax > mean+2*sd
high.ptratio = Boston[which(ptratio>mean(ptratio)+2*sd(ptratio)),]
range(ptratio);mean(ptratio);sd(ptratio)
# no suburb with pupil-teacher ratio > mean+2*sd

### e ###
bound.by.Charles.river = Boston[which(chas==1),]
dim(bound.by.Charles.river)[1]
# 35 suburbs bound the Charles river

### f ###
median(ptratio)

### g ###
Boston[which(medv == min(medv)),]
# There are  two suburbs with lowest median value of owner-occupied homes
mean(crim) + 2*sd(crim)
# They are in higher crime suburb
mean(lstat) + 2*sd(lstat)
# one of them are in higher lstat suburb

### h ###
dim(Boston[which(rm>=7),])[1]
dim(Boston[which(rm>=8),])[1]
summary(Boston)
summary(subset(Boston,rm>=8))
# suburbs that average more than eight rooms per dwelling have lower crime, 
# higher medv and proportion of blacks
