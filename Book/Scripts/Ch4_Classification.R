# CHAPTER 4:  CLASSIFICATION

# Clear Namespace
rm(list = ls())

# Dataset
summary(Credit.data)
length(Credit.data$Income)

# Plot 
library(ggplot2)

plot(Credit.data$Balance, Credit.data$Income)
boxplot(Credit.data$Gender, Credit.data$Income)
show(Credit.data)
boxplot.default(Credit.data$Balance)

# Default Dataset
library(ISLR)
Default.dataset = ISLR::Default
summary(Default.dataset)

# Generate a Boxplot
plot(Default.dataset$income ~ Default.dataset$default)

# Predict Default with Linear Approach
default = as.numeric(Default.dataset$default)
lm.linear.default = lm(default ~ Default.dataset$income)
plot(lm.linear.default)
abline(lm(default ~ Default.dataset$income))


# Logit Function
num_range = seq(0,100,1)               # syntax:  seq(from, to, by)
rm(list = ls())

l = vector()
for(i in 0:100) {
  l[i] <- (exp(1)^i / (1 + exp(1)^i))                    # append to the ith element in the list the value after <-  
}

# Test the difference between a range of -1 to 1 and -10 to 10.  Notice how the contribution of the 1 in the denominator affects the curve. 
# When you ony do a range of -1 to 1, you get that linear line at the center of the shape.  When you do -10 to 10 you can a clear S shape. 
# Note that the y axis is always between 0 and 1 and this is because but for the 1 in the denominator, but the numerator and den are equal.
# In fact, it can never be more than 1 or less than 0. 

vector.1 = seq(-10,10, 0.1)
vector.1
hist(1 / (1 + exp(1)^-vector.1))

x = 10
exp(1)^x / (1 + exp(1)^x)



# Fit a Logistic Regression Model Using Default Data  (glm is used to fit generalized linear models)
rm(list = ls())
library(ISLR)
Def_data = ISLR::Default

# General Linear Models (glm())
syntax = 'glm(y ~ x1-xn, data = mydata, family = binomial'

glm.default = glm(Def_data$default ~ Def_data$income, family = 'binomial')

summary(glm.default)
plot(glm.default)
abline(glm.default)

















