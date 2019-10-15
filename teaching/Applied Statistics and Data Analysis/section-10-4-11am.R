###################################################
#### Example and Exercise in Faraway Chapter 3 & 4
####
#### Cong Mu
#### 10/04/2019
###################################################

library(faraway)
library(ggplot2)
library(ellipse)


#### Example in Chapter 3
data(savings)
head(savings)


## 4 Regression with different predictors
lmod1 <- lm(sr ~ pop75 + pop15 + ddpi + dpi, savings)
lmod2 <- lm(sr ~ pop75 + ddpi + dpi, savings)
lmod3 <- lm(sr ~ pop75 + ddpi, savings)
lmod4 <- lm(sr ~ pop75, savings)


## Compare the coefficients of the same predictor
sumary(lmod1)
sumary(lmod2)
sumary(lmod3)
sumary(lmod4)


## Prediction
x0 <- data.frame(pop15=32, pop75=3, dpi=700, ddpi=3)

predict(lmod1, x0)
predict(lmod2, x0)
predict(lmod3, x0)
predict(lmod4, x0)


## Individual CI for coefficients
lmod <- lm(sr ~ ., savings)
sumary(lmod)

-1.69149768 + c(-1,1) * qt(0.975, 50-5) * 1.08359893

confint(lmod)


## Joint CI for coefficients
plot(ellipse(lmod, c(2,3)), type="l", xlim=c(-1,0))
points(0, 0)
points(coef(lmod)[2], coef(lmod)[3], pch=18)

abline(v=confint(lmod)[2,], lty=2)
abline(h=confint(lmod)[3,], lty=2)

# CI and hypothesis test
lmod0 <- lm(sr ~ ddpi + dpi, savings)
lmoda <- lm(sr ~ pop75 + pop15 + ddpi + dpi, savings)
anova(lmod0, lmoda)


## CI for predictions
data(gala)
head(gala)

lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)
x0 <- c(1, 0.08, 93, 6.0, 12.0, 0.34)
y0 <- sum(x0*coef(lmod))

# Design matrix
x <- model.matrix(lmod)
xtxi <- solve(t(x) %*% x)

# SSE and sigmahat
sum(lmod$residuals^2)
deviance(lmod)

sse <- deviance(lmod)
sigmahat <- sqrt(sse/df.residual(lmod))

y0 + c(-1,1) * c(sigmahat * qt(0.975, df.residual(lmod)) * sqrt(x0 %*% xtxi %*% x0))
y0 + c(-1,1) * c(sigmahat * qt(0.975, df.residual(lmod)) * sqrt(1 + x0 %*% xtxi %*% x0))

# Alternatively
x0 <- data.frame(Area=0.08, Elevation=93, Nearest=6.0, Scruz=12, Adjacent=0.34)
predict(lmod, x0, se=TRUE)

predict(lmod, x0, interval="confidence")
predict(lmod, x0, interval="prediction")




#### Exercise 1 in Chapter 3
data(prostate)
head(prostate)

lmod <- lm(lpsa ~ ., prostate)
sumary(lmod)


## Individual CI for coefficients
-0.0196372 + c(-1,1) * qt(0.975, 97-9) * 0.0111727
-0.0196372 + c(-1,1) * qt(0.95, 97-9) * 0.0111727

confint(lmod)
confint(lmod, level = 0.90)


## Joint CIs for coefficients
plot(ellipse(lmod, c(4,5)), type="l")
points(0, 0)
points(coef(lmod)[4], coef(lmod)[5], pch=18)

abline(v=confint(lmod)[4,], lty=2)
abline(h=confint(lmod)[5,], lty=2)


## CI for predictions
x0 <- c(1, 1.44692, 3.62301, 65.00000, 0.30010, 0.00000, -0.79851, 7.00000, 15.00000)
y0 <- sum(x0*coef(lmod))

# Design matrix
x <- model.matrix(lmod)
xtxi <- solve(t(x) %*% x)

# SSE and sigmahat
sum(lmod$residuals^2)
deviance(lmod)

sse <- deviance(lmod)
sigmahat <- sqrt(sse/df.residual(lmod))

y0 + c(-1,1) * c(sigmahat * qt(0.975, df.residual(lmod)) * sqrt(x0 %*% xtxi %*% x0))
y0 + c(-1,1) * c(sigmahat * qt(0.975, df.residual(lmod)) * sqrt(1 + x0 %*% xtxi %*% x0))

# Alternatively
x0 <- data.frame(lcavol=1.44692, lweight=3.62301, age=65.00000, lbph=0.30010, 
                 svi=0.00000, lcp=-0.79851, gleason=7.00000, pgg45=15.00000)
predict(lmod, x0, se=TRUE)

predict(lmod, x0, interval="confidence")
predict(lmod, x0, interval="prediction")




#### Example in Chapter 4


## Constant variance
par(mfrow=c(3,3))
n <- 50

# Constant variance
for(i in 1:9) {x <- runif(n) ; plot(x,rnorm(n)) ; abline(0,0,col='red')}

# Strong nonconstant variance
for(i in 1:9) {x <- runif(n) ; plot(x,x*rnorm(n)); abline(0,0,col='red')}

# Mild nonconstant variance
for(i in 1:9) {x <- runif(n) ; plot(x,sqrt((x))*rnorm(n)); abline(0,0,col='red')}

# Nonlinearity
for(i in 1:9) {x <- runif(n) ; plot(x,cos(x*pi/25)+rnorm(n)); abline(0,0,col='red')}

par(mfrow=c(1,1))





