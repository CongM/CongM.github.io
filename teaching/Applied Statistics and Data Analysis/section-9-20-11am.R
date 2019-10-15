#######################################
#### Exercise in Faraway
####
#### Cong Mu
#### 09/20/2019
#######################################

library(faraway)
library(ggplot2)



#### Exercise 2 

## Load and learn data
data(uswages)
head(uswages)


## Fit model
model1 <- lm(wage ~ educ + exper, uswages)
summary(model1)

model2 <- lm(log(wage) ~ educ + exper, uswages)
summary(model2)




#### Exercise 3


## Generate data
set.seed(920)
x <- 1:20
y <- x + rnorm(20)


## Fit model
poly2 <- lm(y ~ x + I(x^2))
summary(poly2)
X2 <- model.matrix( ~ x + I(x^2), data.frame(x, y))
solve(crossprod(X2,X2), crossprod(X2,y))

poly3 <- lm(y ~ x + I(x^2) + I(x^3))
summary(poly3)
X3 <- model.matrix( ~ x + I(x^2) + I(x^3), data.frame(x, y))
solve(crossprod(X3,X3), crossprod(X3,y))

poly5 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4))
summary(poly5)
X4 <- model.matrix( ~ x + I(x^2) + I(x^3) + I(x^4), data.frame(x, y))
solve(crossprod(X4,X4), crossprod(X4,y))

poly7 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7))
summary(poly7)
X7 <- model.matrix( ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7), data.frame(x, y))
solve(crossprod(X7,X7), crossprod(X7,y))



#### Exercise 4

## Load and learn data
data(prostate)
head(prostate)


## Fit model
rse <- vector()
rsquared <- vector()

model <- lm(lpsa ~ lcavol, prostate)
modelsummary <- summary(model)
rse[1] <- modelsummary$sigma
rsquared[1] <- modelsummary$r.squared
  
model <- lm(lpsa ~ lcavol + lweight, prostate)
modelsummary <- summary(model)
rse[2] <- modelsummary$sigma
rsquared[2] <- modelsummary$r.squared
  
model <- lm(lpsa ~ lcavol + lweight + svi, prostate)
modelsummary <- summary(model)
rse[3] <- modelsummary$sigma
rsquared[3] <- modelsummary$r.squared 

model <- lm(lpsa ~ lcavol + lweight + svi + lbph, prostate)
modelsummary <- summary(model)
rse[4] <- modelsummary$sigma
rsquared[4] <- modelsummary$r.squared 

model <- lm(lpsa ~ lcavol + lweight + svi + lbph + age, prostate)
modelsummary <- summary(model)
rse[5] <- modelsummary$sigma
rsquared[5] <- modelsummary$r.squared 

model <- lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp, prostate)
modelsummary <- summary(model)
rse[6] <- modelsummary$sigma
rsquared[6] <- modelsummary$r.squared 

model <- lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp + pgg45, prostate)
modelsummary <- summary(model)
rse[7] <- modelsummary$sigma
rsquared[7] <- modelsummary$r.squared 

model <- lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp + pgg45 + gleason, prostate)
modelsummary <- summary(model)
rse[8] <- modelsummary$sigma
rsquared[8] <- modelsummary$r.squared 


dat <- data.frame(vars = 1:8, rse, rsquared)  
ggplot(dat) + geom_line(aes(x=vars, y=rse, color='rse')) + 
  geom_point(aes(x=vars, y=rse, color='rse')) +
  geom_line(aes(x=vars, y=rsquared, color='rsquared')) + 
  geom_point(aes(x=vars, y=rsquared, color='rsquared')) + 
  labs(x = "Number of Variables", y = "Measure of Model Performance", color = "Measure")





