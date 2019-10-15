#######################################
#### Example and Exercise
####
#### Cong Mu
#### 09/13/2019
#######################################




#### Example with gala dataset



## Load the package
library(faraway)


## Attach the data
data(gala, package = "faraway")


## Learn the data
head(gala[,-2])


## Fit a linear model
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(lmod)


## OLS by hand
x <- model.matrix( ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)
y <- gala$Species
xtxi <- solve(t(x) %*% x)

xtxi %*% t(x) %*% y
solve(crossprod(x,x), crossprod(x,y))


## Regression quantities
names(lmod)

lmodsum <- summary(lmod)
names(lmodsum)

# sigma
sqrt(deviance(lmod)/df.residual(lmod))
lmodsum$sigma

# standard errors for the coefficients
xtxi <- lmodsum$cov.unscaled
sqrt(diag(xtxi))*60.975

lmodsum$coef[,2]


#### Exercise with teengamb dataset

## Attach the data
data(teengamb, package = "faraway")


## Fit a linear model
lmod <- lm(gamble ~ sex + status + income + verbal, data = teengamb)
summary(lmod)
lmod <- lm(gamble ~ ., data = teengamb)
summary(lmod)

# Residuals
e <- lmod$residuals
which.max(e)
mean(e)
median(e)

# Correlation
fitted <- lmod$fitted.values
cor(e, fitted)

cor(e, teengamb$income)

# Coefficients
lmod$coefficients



