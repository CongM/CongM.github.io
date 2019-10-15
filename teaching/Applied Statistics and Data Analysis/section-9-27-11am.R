###################################################
#### Example and Exercise in Faraway Chapter 3
####
#### Cong Mu
#### 09/27/2019
###################################################

library(faraway)
library(ggplot2)



#### Example
data(gala)
head(gala)


## Test all predictors
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)
nullmod <- lm(Species ~ 1, gala)
anova(nullmod, lmod)

# By hand
sse0 <- deviance(nullmod)
sse <- deviance(lmod)

sum(nullmod$residuals^2)
sum(lmod$residuals^2)

df0 <- df.residual(nullmod)
df <- df.residual(lmod)

nullmod$df.residual
lmod$df.residual

fstat <- ((sse0-sse)/(df0-df))/(sse/df)

1 - pf(fstat, df0-df, df)


## Test one predictor
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)
lmods <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, gala)
anova(lmods, lmod)

sumary(lmod)


## Test a pair of predictors
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)
lmods <- lm(Species ~ Elevation + Nearest + Scruz, gala)
anova(lmods, lmod)


## Test a subspace
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)

lmods <- lm(Species ~ I(Area+Adjacent) + Elevation + Nearest + Scruz, gala)
anova(lmods, lmod)

lmods <- lm(Species ~ Area + offset(0.5*Elevation) + Nearest + Scruz + Adjacent, gala)
anova(lmods, lmod)

summary(lmod)

tstat <- (0.31946 - 0.5) / 0.05366
2 * pt(tstat, 24)
tstat^2


## Confidence interval
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)
sumary(lmod)

qt(0.975, 30-6)
-0.02394 + c(-1,1) * 2.0639 * 0.02242
-0.07480 + c(-1,1) * 2.0639 * 0.01770

confint(lmod)



#### Exercise 1
data(prostate)
head(prostate)

lmod <- lm(lpsa ~ ., prostate)
sumary(lmod)

-0.0196372 + c(-1,1) * qt(0.975, 97-9) * 0.0111727
-0.0196372 + c(-1,1) * qt(0.95, 97-9) * 0.0111727

confint(lmod)
confint(lmod, level = 0.90)


lmod0 <- lm(lpsa ~ lcavol + lweight + svi, prostate)
anova(lmod0, lmod)




