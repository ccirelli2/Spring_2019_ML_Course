rm(list = ls())
library(ISLR)

# Load Data
target_dir = '/home/ccirelli2/Desktop/GSU/2019_Spring/Spring_2019_ML_Course/Datasets'
setwd(target_dir)
MyData = read.csv('/home/ccirelli2/Desktop/GSU/2019_Spring/Spring_2019_ML_Course/Datasets/Advertising.csv', header = TRUE, sep=',')

# Load DF - Remove Index Column
data.sales = MyData[-1]


# Fit Model 
lm.simple.fit = lm(sales ~ TV, data = data.sales)

# Get Summary of Model
summary(lm.simple.fit)

# Plot 
plot(lm.simple.fit)
abline(lm.simple.fit)
?lm()
?abline


