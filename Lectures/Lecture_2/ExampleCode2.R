rm(list = ls()) 
graphics.off()


library(ISLR)

MyData <- read.csv(file="Advertising.csv", header=TRUE,  sep=",")
attach(MyData)

# Removing the first row from the data file
MyData <- MyData[-1]

lm.fit <- lm(sales ~ TV + radio + newspaper ,data=MyData)

# getting a full summary of the fit:
print(summary(lm.fit))
# to calculate Std. Error, sigma has been estimated using the equation sigma=RSE = sqrt(RSS/(n-2))
# See Multiple R-squared for our suggested R-squared value (the adjusted version is not our focus
# yet- that is adjusted to the number of predictors in the model)

# confidence intervals:
confint(lm.fit)

#prinitng the correlation matrix
print(cor(MyData))


