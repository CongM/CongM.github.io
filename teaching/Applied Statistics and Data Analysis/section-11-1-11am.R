###################################################
#### Examples in Faraway Chapter 6 and ANOVA
####
#### Cong Mu
#### 11/01/2019
###################################################

library(faraway)
library(ggplot2)
library(car)


#### Examples in Chapter 6

## Partial regression
d <- residuals(lm(sr ~ pop75 + dpi + ddpi, savings))
m <- residuals(lm(pop15 ~ pop75 + dpi + ddpi, savings))
plot(m, d, xlab = "pop15 residuals", ylab = "Savings residuals")

coef(lm(d ~ m))

lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
coef(lmod)

abline(0, coef(lmod)['pop15'])

avPlots(lmod)

## Partial residual plots
termplot(lmod, partial.resid = TRUE, terms = 1)
termplot(lmod, partial.resid = TRUE, terms = 2)
termplot(lmod, partial.resid = TRUE)

mod1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings, subset = (pop15 > 35))
mod2 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings, subset = (pop15 < 35))
sumary(mod1)
sumary(mod2)

## Introduce extra dimensions into diagnostic plots
savings$status <- ifelse(savings$pop15 > 35, "young", "old")
ggplot(savings, aes(x=ddpi,y=sr,shape=status)) + geom_point()
ggplot(savings, aes(x=ddpi,y=sr)) + geom_point() + facet_grid(~status) + stat_smooth(method = "lm")



### ANOVA

## Completely randomized design
LS_GRE <- c(630,660,640,660,470,480,600,650,580,710)  #GRE scores for life sci program
PS_GRE <- c(660,760,640,670,720,700,690,710,530,450)  #GRE scores for phys sci program
Soc_GRE <- c(440,540,330,450,670,570,570,530,590,630) #GRE scores for soc sci program

LS <- rep("LS",10)      # generate a vector of 10 repetitions of  "LS"
PS <- rep("PS",10)      # generate a vector of 10 repetitions of  "PS"
SocS <- rep("SocS",10)  # generate a vector of 10 repetitions of  "SocS"

Score <- c(LS_GRE,PS_GRE,Soc_GRE)  # generates a vector that lists all GRE data
Major <- c(LS,PS,SocS)             # generates a vector that lists the corresponding program

Scores_by_Major <- data.frame(Score,Major)

GRE_anova <- aov(Score ~ Major, data = Scores_by_Major) # use aov to see if scores differ by major
summary(GRE_anova)

TukeyHSD(GRE_anova)


## Randomized Block Design
Premium <- c(3922,2378,2560,2584,4073,2512,2476,2759,
             3663,2478,2549,2494,4075,3056,2756,2940,
             3876,2508,2614,2714)

Cent_21 <- rep("Cent_21",4) 
Geico <- rep("Geico",4)
TripleA <- rep("TripleA",4)
Firemen <- rep("Firemen",4)
StateFarm <- rep("StateFarm",4)
Company <- c(Cent_21, Geico,TripleA,Firemen,StateFarm)

loc <- c("WHolly", "LaBeach", "Red", "Riverside")
Location <- rep(loc,5)

Premium_Location_Company <- data.frame(Premium, Company, Location)

Premium_anova <- aov(Premium ~ Company + Location, data = Premium_Location_Company)
summary(Premium_anova)

TukeyHSD(Premium_anova)







