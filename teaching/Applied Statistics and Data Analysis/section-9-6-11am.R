#######################################
#### Initial Example of Data Analysis
####
#### Cong Mu
#### 09/06/2019
#######################################



## Install the package
install.packages("faraway")

## Load the package
library(faraway)

## Call up this particular dataset
data(pima)

## Print out the data
pima

## Summary of the data
summary(pima)

## Check the data quality
sort(pima$diastolic)

## Re-code missing values
pima$diastolic[pima$diastolic == 0] <- NA
pima$glucose[pima$glucose == 0] <- NA
pima$triceps[pima$triceps == 0] <- NA
pima$insulin[pima$insulin == 0] <- NA
pima$bmi[pima$bmi == 0] <- NA

## Set categorical variable
pima$test <- factor(pima$test)
summary(pima$test)

## Add descriptive labels
levels(pima$test) <- c("negative", "positive")
summary(pima)

## Histogram
hist(pima$diastolic, xlab = "Diastolic", main = "")

## Kernel estimate 
plot(density(pima$diastolic,na.rm=TRUE), main = "")

## Sorted data against its index
plot(sort(pima$diastolic), ylab = "Sorted Diastolic")

## Scatterplot
plot(diabetes ~ diastolic, pima)

## Boxplot
plot(diabetes ~ test, pima)

## Powerful ggplot2
install.packages("ggplot2")
library(ggplot2)

ggplot(pima, aes(x=diastolic)) + geom_histogram()
ggplot(pima, aes(x=diastolic)) + geom_density()
ggplot(pima, aes(x=diastolic,y=diabetes)) + geom_point()

ggplot(pima, aes(x=diastolic,y=diabetes,shape=test)) + geom_point() + theme(legend.position = "top", legend.direction = "horizontal")
ggplot(pima, aes(x=diastolic,y=diabetes)) + geom_point(size = 1) + facet_grid(~test)











