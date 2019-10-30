###################################################
#### Examples and Exercises in Faraway Chapter 6
####
#### Cong Mu
#### 10/25/2019
###################################################

library(faraway)
library(ggplot2)


#### Examples in Chapter 6

## Leverage

# savings dataset
data(savings)
head(savings)

lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)

# H matrix and h_i
hatv <- hatvalues(lmod)
head(hatv)
sum(hatv)

n <- nrow(savings)
p <- length(lmod$coefficients)
hatv[hatv>2*p/n]

# H and h_i by hand
y <- savings$sr
X <- cbind(Intercept=rep(1, nrow(savings)), as.matrix(savings[,2:5]))
H <- X %*% solve(t(X)%*%X) %*% t(X)
hatv2 <- diag(H)
head(hatv2)
sum(hatv2)

# Half-normal plot
countries <- row.names(savings)
halfnorm(hatv, 4, labs = countries, ylab = "Leverages")

# Standardized residuals
qqnorm(rstandard(lmod))
abline(0, 1, col = 'red')


## Outliers

# Illustration
set.seed(123)
testdata <- data.frame(x = 1:10, y = 1:10+rnorm(10))
lmod <- lm(y ~ x, testdata)

par(mfrow=c(1,3))

# extra point with a central predictor value
p1 <- c(5.5,12)
lmod1 <- lm(y ~ x, rbind(testdata, p1)) 
plot(y ~ x, rbind(testdata, p1))
points(5.5,12,pch=4,cex=2)
abline(lmod)
abline(lmod1, lty=2)

# extra point outside the range of X
p2 <- c(15,15.1)
lmod2 <- lm(y ~ x, rbind(testdata, p2)) 
plot(y ~ x, rbind(testdata, p2))
points(15,15.1,pch=4,cex=2)
abline(lmod)
abline(lmod2,lty=2)

# extra point in a different position on the response scale
p3 <- c(15,5.1)
lmod3 <- lm(y ~ x, rbind(testdata, p3)) 
plot(y ~ x, rbind(testdata, p3))
points(15,5.1,pch=4,cex=2)
abline(lmod)
abline(lmod3,lty=2)

par(mfrow=c(1,1))

# savings dataset
lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)

# Studentized residuals
stud <- rstudent(lmod)
stud[which.max(abs(stud))]

# Bonferroni critical value
qt(.05/(50*2), 44)

# star dataset
data(star)
head(star)
plot(star$temp, star$light, xlab = "log(Temperature)", ylab = "log(LightIntensity)")

lmod <- lm(light ~ temp, star)
abline(lmod)

range(rstudent(lmod))
qt(.05/(47*2), 44)

lmodi <- lm(light ~ temp, data = star, subset = (temp>3.6))
abline(lmodi, lty = 2)


## Influential observations

# savings dataset
lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)

# Cook statistics
cook <- cooks.distance(lmod)
halfnorm(cook, 3, labs = countries, ylab = "Cook's distances")

lmodi <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings, subset = (cook<max(cook)))
sumary(lmodi)

sumary(lmod)

# leave-out-one differences in the coefficients
plot(dfbeta(lmod)[,2], ylab = "Change in pop15 coef")
abline(h=0, col = 'red')

lmodj <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings, subset = (countries!="Japan"))
sumary(lmodj)

plot(lmod)



#### Exercises in Chapter 6

## sat dataset
data(sat)
head(sat)

lmod <- lm(total ~ expend + salary + ratio + takers, sat)


## constant variance
plot(fitted(lmod), residuals(lmod), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = 'red')


## normality
qqnorm(residuals(lmod), ylab = "Residuals" , main = "")
qqline(residuals(lmod), col = 'red')

shapiro.test(residuals(lmod))


## large leverage points
hatv <- hatvalues(lmod)
head(hatv)
sum(hatv)

n <- nrow(sat)
p <- length(lmod$coefficients)
hatv[hatv>2*p/n]

states <- row.names(sat)
halfnorm(hatv, 4, labs = states, ylab = "Leverages")


## outliers
range(rstudent(lmod))


## influential points
cook <- cooks.distance(lmod)
halfnorm(cook, 3, labs = states, ylab = "Cook's distances")

lmodi <- lm(total ~ expend + salary + ratio + takers, sat, subset = (cook<max(cook)))
sumary(lmodi)

sumary(lmod)

plot(dfbeta(lmod)[,2], ylab = "Change in expend coef")
abline(h=0, col = 'red')


plot(lmod)




