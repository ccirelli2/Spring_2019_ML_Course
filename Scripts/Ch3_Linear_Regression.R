### Spring 2019 ML Class - Ch3 - Linear Regression___________________________


## Clear namespace
rm(list = ls())

## Load library using library()
library(MASS) # large collection of datasets
install.packages("ISLR")
library(ISLR) # dataset for book


## Simple Linear Regression:
fix(Boston)       # load dataset (media house values)
names(Boston)     # view column names
summary(Boston)   # get summary of data set. 
# medv = median value of home
lm.fit = lm(medv~lstat, data = Boston)  #lstat percentage of households with low socioeconomic status
lm.fit

# Getting Data out of the model
names(lm.fit)     # will give you the lm variables that you can call by name.  ex coefficeints
coef(lm.fit)      # this returns our b0 and b1 for this lm model. 
summary(lm.fit)   # summary statistics
plot(lm.fit)      # odd that it automatically used a polinomial function for f(x) instead of a straight line. 

# Generating Confidence Intervals for our Coefficients
confint(lm.fit)   # Using a 5% alpha and a two tailed test, we get a CI of 33.44 - 35.65, which means our intercept of 34.55 is within this range. 
                  # so we can be 95% confident that our interceipt in this CI. 
plot(resid(lm.fit))  # resid() will return all of the residuals, which you can plot easily with the plot() function. 
effects(lm.fit)

# Generate Prediction Intervals 
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval = 'confidence')   # Generate Confidence Intervals for specific values of lstat 
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval = 'prediction')   # Notice how the prediction intervals are larger
# 95% confidence interval with lstat value of 10 is (24.47, 25.63). Predict gives you the fitted value plus the confidence intervals.  Looks to default at 95%. 

# Plot Data & Abline Feature Selection
plot(Boston$lstat, Boston$medv)
abline(lm.fit)            # Note this plots a linear line, which is different from what we saw with using hte lm function. 
abline(lm.fit, lwd=3)     # increase line width by a factor of 3
abline(lm.fit, lwd=3, col='red')   # change line color to red
plot(Boston$lstat, Boston$medv, lwd=3, col='red', pch=20)
plot(Boston$lstat, Boston$medv, lwd=3, col='red', pch='+')  # Plot '+' instead of a dot. 
plot(Boston$lstat, Boston$medv, lwd=3, col='red', pch=1:20) # looks like it generates a range of sybmols. really weird output. 
par(mfrow=c(2,2))         # splits plot area into 4 boxes.  output of plot(lm.fit) now includes our residuals vs fitted values, Normal Q-Q plot, etc. 
plot(lm.fit)
studentized.residuals = rstudent(lm.fit)
plot(studentized.residuals)
plot(predict(lm.fit), rstudent(lm.fit))









