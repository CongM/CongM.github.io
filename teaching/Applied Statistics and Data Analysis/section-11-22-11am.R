###################################################
#### Examples in Faraway Chapter 7 & 8
####
#### Cong Mu
#### 11/22/2019
###################################################


library(faraway)
library(dplyr)
library(MASS)
library(car)
library(ggplot2)



#### Examples in Faraway Chapter 7


## Data
data(savings)
head(savings)


## Regression
lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
sumary(lmod)


## Change of scale
lmod2 <- lm(sr ~ pop15 + pop75 + I(dpi/1000) + ddpi, savings)
sumary(lmod2)

scsav <- data.frame(scale(savings))
lmod3 <- lm(sr ~ ., scsav)
sumary(lmod3)


## Compare the estimates
edf <- data.frame(coef(lmod3), confint(lmod3))[-1,]
names(edf) <- c('Estimate', 'lb', 'ub')

ggplot(aes(x=row.names(edf),y=Estimate,ymin=lb,ymax=ub), data = edf) + 
  geom_pointrange() +
  geom_hline(yintercept = 0, col = gray(0.75)) + 
  coord_flip() +
  xlab("Predictor")


## Data
data(seatpos)
head(seatpos)


## Regression
lmod <- lm(hipcenter ~ ., seatpos)
sumary(lmod)


## Correlation matrix
round(cor(seatpos[,-9]), 2)


## Eigenvalues of X^T X
x <- model.matrix(lmod)[,-1] 
e <- eigen(t(x) %*% x)
e$values

sqrt(e$values[1] / e$values)


## VIF
summary(lm(x[,1] ~ x[,-1]))$r.squared
1 / (1-0.49948)

vif(x)


## Second regression
lmod2 <- lm(hipcenter ~ Age + Weight + Ht, seatpos) 
sumary(lmod2)



#### Examples in Faraway Chapter 8


## Data
data(fpe)
head(fpe)


## WLS
lmod <- lm(A2 ~ A+B+C+D+E+F+G+H+J+K+N-1, fpe, weights = 1/EI)
coef(lmod)

lm(A2 ~ A+B+C+D+E+F+G+H+J+K+N-1, fpe)$coef

lm(A2 ~ A+B+C+D+E+F+G+H+J+K+N-1, fpe, weights=53/EI)$coef

lm(A2 ~ offset(A+G+K)+C+D+E+F+N-1, fpe, weights=1/EI)$coef






