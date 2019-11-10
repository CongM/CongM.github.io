###################################################
#### Examples in Faraway Chapter 14
####
#### Cong Mu
#### 11/08/2019
###################################################

library(faraway)
library(ggplot2)



#### Examples in Chapter 14


## Factors and quantitative predictors
data(sexab)
head(sexab)
table(sexab$csa)

lmod4 <- lm(ptsd ~ cpa + csa + cpa:csa, sexab)
sumary(lmod4)

lmod4 <- lm(ptsd ~ cpa*csa, sexab)
sumary(lmod4)

model.matrix(lmod4)

lmod3 <- lm(ptsd ~ cpa + csa, sexab)
sumary(lmod3)


## Plot the fiited line
plot(ptsd ~ cpa, sexab, pch = as.numeric(csa))
abline(3.96, 0.764)
abline(3.96+6.86, 0.764-0.314, lty = 2)

plot(ptsd ~ cpa, sexab, pch = as.numeric(csa))
abline(3.98, 0.551)
abline(3.98+6.27, 0.551, lty = 2)


## Simple diagonstics
plot(fitted(lmod3), residuals(lmod3), pch=as.numeric(sexab$csa), xlab="Fitted", ylab="Residuals")
abline(0, 0, col = 'red')


## Interpretation with interaction terms
data(whiteside, package="MASS")
head(whiteside)

ggplot(aes(x=Temp,y=Gas), data = whiteside) + 
  geom_point() + 
  facet_grid(~ Insul) + 
  geom_smooth(method = "lm")

lmod <- lm(Gas ~ Temp*Insul, whiteside)
sumary(lmod)

mean(whiteside$Temp)
whiteside$ctemp <- whiteside$Temp - mean(whiteside$Temp)
lmodc <- lm(Gas ~ ctemp*Insul, whiteside)
sumary(lmodc)


## Factors with more than two levels
data(fruitfly)
head(fruitfly)
table(fruitfly$activity)

plot(longevity ~ thorax, fruitfly, pch = unclass(activity))
legend(0.63, 100, levels(fruitfly$activity), pch = 1:5)

ggplot(aes(x=thorax, y=longevity), data = fruitfly) + 
  geom_point() + 
  facet_wrap( ~ activity)

lmod <- lm(longevity ~ thorax*activity, fruitfly)
sumary(lmod)

# Simple diagonstics
plot(lmod)

# ANOVA
anova(lmod)

lmodp <- lm(longevity ~ thorax+activity, fruitfly)
sumary(lmodp)

drop1(lmodp, test = "F")

plot(residuals(lmodp) ~ fitted(lmodp), pch=unclass(fruitfly$activity), xlab="Fitted", ylab="Residuals")
abline(h = 0, col = 'red')

# Log transformation
lmodl <- lm(log(longevity) ~ thorax + activity, fruitfly)
sumary(lmodl)

plot(residuals(lmodl) ~ fitted(lmodl), pch=unclass(fruitfly$activity), xlab="Fitted", ylab="Residuals")
abline(h = 0, col = 'red')





