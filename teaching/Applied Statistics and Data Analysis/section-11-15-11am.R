###################################################
#### Problems in Homework
####
#### Cong Mu
#### 11/15/2019
###################################################


library(faraway)
library(dplyr)
library(MASS)
library(ggplot2)



#### Problem 3 in Homework 3

## Data
data(pipeline)
head(pipeline)

## Simple regression
lmod1 <- lm(Lab ~ Field, pipeline)
summary(lmod1)

## Check for non-constant variance
plot(fitted(lmod1), residuals(lmod1), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = 'red')

## Transformation
temp <- boxcox(lmod1)
lambda <- temp$x[which.max(temp$y)]

pipeline <- pipeline %>%
  mutate(Labprime = (Lab^lambda-1)/lambda, logLab = log(Lab))

lmod2 <- lm(Labprime ~ Field, pipeline)
summary(lmod2)

lmod22 <- lm(logLab ~ Field, pipeline)
summary(lmod22)

## Check for non-constant variance after transformation
plot(fitted(lmod2), residuals(lmod2), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = 'red')

plot(fitted(lmod22), residuals(lmod22), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = 'red')

## Another transformation
lmod3 <- lm(Labprime ~ Field + sqrt(Field), pipeline)
summary(lmod3)

lmod33 <- lm(logLab ~ Field + I(Field^2), pipeline)
summary(lmod33)

## Check for non-constant variance after second transformation
plot(fitted(lmod3), residuals(lmod3), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = 'red')

plot(fitted(lmod33), residuals(lmod33), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = 'red')



#### Problem 6 in Homework 3

## Data
data(hills)
head(hills)

lmod1 <- lm(time ~ dist + climb, hills)
summary(lmod1)

## Diagnostics
plot(fitted(lmod1), residuals(lmod1), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = 'red')

plot(fitted(lmod1), rstandard(lmod1), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = 'red')

# Studentized residuals
stud <- rstudent(lmod1)
range(stud)
stud[which.max(abs(stud))]

# Bonferroni critical value
n <- nrow(hills)
p <- 3
qt(.05/(n*2), n-p-1)

which(abs(stud) > abs(qt(.05/(n*2), n-p-1)))

lmod2 <- lm(time ~ dist + climb, data = hills, subset = (abs(stud)<abs(qt(.05/(n*2),n-p-1))))
summary(lmod2)

plot(fitted(lmod2), residuals(lmod2), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = 'red')

# Partial residual plot
termplot(lmod2, partial.resid = TRUE, terms = 2)

lmod3 <- lm(time ~ dist + climb + I(climb^2), data = hills, subset = (abs(stud)<abs(qt(.05/(n*2),n-p-1))))
summary(lmod3)

plot(fitted(lmod3), residuals(lmod3), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = 'red')





