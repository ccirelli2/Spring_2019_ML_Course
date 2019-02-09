rm(list = ls()) 
graphics.off()


library(ISLR)

MyData <- read.csv(file="Advertising.csv", header=TRUE,  sep=",")
attach(MyData)

# Removing the first row from the data file
MyData <- MyData[-1]



lm.fit <- lm(sales ~ TV ,data=MyData)

# getting a full summary of the fit:
print(summary(lm.fit))
# to calculate Std. Error, sigma has been estimated using the equation sigma=RSE = sqrt(RSS/(n-2))
# See Multiple R-squared for our suggested R-squared value (the adjusted version is not our focus
# yet- that is adjusted to the number of predictors in the model)

# confidence intervals:
Beta0 = coef(summary(lm.fit))[1, 1]
Beta1 = coef(summary(lm.fit))[2, 1]
# coef is a generic function which extracts model coefficients from objects returned by modeling functions

SEBeta0 = coef(summary(lm.fit))[1, 2]
SEBeta1 = coef(summary(lm.fit))[2, 2]

# 95% confidence intervals
Conf.Beta.0 <- c(Beta0-2*SEBeta0, Beta0+2*SEBeta0)
Conf.Beta.1 <- c(Beta1-2*SEBeta1, Beta1+2*SEBeta1)
# OR INSTEAD OF THE ABOVE YOU CAN USE THE COMMAND BELOW (more accurate)
print(confint(lm.fit))


# plotting the data and the fit

plot(TV,sales)
abline(lm.fit,lwd=3,col="red")


