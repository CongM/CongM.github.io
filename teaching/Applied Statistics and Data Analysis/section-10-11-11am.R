###################################################
#### Examples in Faraway Chapter 6
####
#### Cong Mu
#### 10/11/2019
###################################################

library(faraway)
library(ggplot2)
library(lmtest)


#### Example in Chapter 6

## Constant variance
data(savings)
head(savings)

lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)

plot(fitted(lmod), residuals(lmod), xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = 'red')


## Non-constant variance
data(gala)
head(gala)

lmod <- lm(Species ~ Area + Elevation + Scruz + Nearest + Adjacent, gala)

plot(lmod$fitted.values, lmod$residuals, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = 'red')


## Normality
lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)

# Q-Q plot
qqnorm(residuals(lmod), ylab = "Residuals" , main = "")
qqline(residuals(lmod), col = 'red')

hist(residuals(lmod), xlab = "Residuals", main = "")

# Some examples
par(mfrow=c(1,3))
n <- 50

for(i in 1:3) {x <- rnorm(n); qqnorm(x); qqline(x,col='red')}
for(i in 1:3) {x <- exp(rnorm(n)); qqnorm(x); qqline(x,col='red')}
for(i in 1:3) {x <- rcauchy(n); qqnorm(x); qqline(x,col='red')} 
for(i in 1:3) {x <- runif(n); qqnorm(x); qqline(x,col='red')}

par(mfrow=c(1,1))

# Shapiro-Wilk test 
shapiro.test(residuals(lmod))


## Correlated errors
data(globwarm)
head(globwarm)

lmod <- lm(nhtemp ~ wusa + jasper + westgreen + chesapeake + tornetrask + urals + mongolia + tasman, globwarm)

plot(residuals(lmod) ~ year, na.omit(globwarm), ylab = "Residuals")
abline(h = 0, col = 'red')

# Successive pairs of residuals
n <- length(residuals(lmod))
plot(tail(residuals(lmod),n-1) ~ head(residuals(lmod),n-1), xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]))
abline(h = 0, v = 0, col = grey(0.75))

# Durbin–Watson test
dwtest(nhtemp ~ wusa + jasper + westgreen + chesapeake + tornetrask + urals + mongolia + tasman, data = globwarm)




