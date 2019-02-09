# CHAPTER 3 CONTINUED

# Clear namespace
rm(list=ls())

# Load Data
library(MASS)
install.packages("ISLR")
library(ISLR)
fix(Boston)

# ANOVA - Used to compare linea model with model that has a quadratic term

lm.fit1 = lm(Boston$medv ~ Boston$lstat)
lm.fit2 = lm(medv ~ I(lstat^2), data = Boston)
anova(lm.fit1, lm.fit2)


# Qualitative Predictors
fix(Carseats)
names(Carseats)
?Carseats
lm.fit3 = lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit3)         # R automatically makes dummy variables to the qualitative predictors
