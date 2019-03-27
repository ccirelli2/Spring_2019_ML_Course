# Clear Data--------------------------------------------------------------------
rm(list=ls())

# Import Packages---------------------------------------------------------------
library(ISLR)
library(glmnet)
library(pls)

# Create Training & Test Set----------------------------------------------------
setwd('/home/ccirelli2/Desktop/Repositories/Spring_2019_ML_Course/HW/HW3/HW3')
fert_data = read.csv('Fertility.csv')
train_fert_data = fert_data[0:30, ]
test_fert_data = fert_data[31:47, ]

# a. Fit a Lnear model---------------------------------------------------------- 
head(train_fert_data)
lm1.train = lm(Fertility ~ ., data = train_fert_data)
lm1.train.mse = mean(lm1.train$residuals^2)
# mse =27.66205

# b. Fit Ridge Regrssion Model to training set---------------------------------
'glmnet:  fit a generalized linear model via penalized MLE.  The regularizationpath is 
          computed for the lasso or elasticnet penalty at a grid of values for lamgda. 
          glmnet(x,y, family=c(gaussian, binomial, poisson, multinomial, cox, mgaussian), 
                 weights, offset=Null, apha = 1, lambda = n)
          ref:  drsimonj.svbtle.com 
 LinearRidge: Anothe package o conduct Ridge Regression
'

# Define x, y values
x = model.matrix(Fertility ~.,data = train_fert_data)[,-1]       # Create a matrix object for as input to glmnet
y = train_fert_data$Fertility

# Define grid of possible lambda values
lambda.grid=10^seq(from = 10, to = -2,length=100)
lm2.train.ridge = glmnet(x, y, alpha = 0, lambda = lambda.grid)
summary(lm2.train.ridge)

# Use Cross Validation to Find the Optimal Lambda Value
lm2.train.ridge.cv_fit = cv.glmnet(x, y, alpha = 0, lambda = lambda.grid)

# Plot the Lambda Values vs MSE
plot(lm2.train.ridge.cv_fit)

# Return Optimal Value
lm2.train.ridge.opt_lambda = lm2.train.ridge.cv_fit$lambda.min
lm2.train.ridge.opt_lambda   # 3.51

# Report Test Error
fit = lm2.train.ridge.cv_fit$glmnet.fit
y_predict = predict(fit, s = lm2.train.ridge.opt_lambda, newx = x)
SST = sum((y - mean(y))^2)        #Total Error
SSE = sum((y_predict - y)^2)    #Sum squared error
MSE = mean((y_predict - y) ^2)
R2 = (1- (SSE / SST))
MSE  # 29.79
R2   # 0.75


# c.) Fit a LASSO model to the training set-----------------------------------------------

# Define grid of possible lambda values
lambda.lasso=10^seq(from = 10, to = -2,length=100)
lm2.train.lasso = glmnet(x, y, alpha = 1, lambda = lambda.grid)
summary(lm2.train.lasso)

# Use Cross Validation to Find the Optimal Lambda Value
lm2.train.lasso.cv_fit = cv.glmnet(x, y, alpha = 1, lambda = lambda.grid)

# Plot the Lambda Values vs MSE
plot(lm2.train.lasso.cv_fit)

# Return Optimal Value
lm2.train.lasso.opt_lambda = lm2.train.lasso.cv_fit$lambda.min
lm2.train.lasso.opt_lambda   # 3.51

# Report Test Error
fit = lm2.train.lasso.cv_fit$glmnet.fit
y_predict = predict(fit, s = lm2.train.lasso.opt_lambda, newx = x)
SST = sum((y - mean(y))^2)        #Total Error
SSE = sum((y_predict - y)^2)    #Sum squared error
MSE = mean((y_predict - y) ^2)
R2 = (1- (SSE / SST))
MSE  # 28.54
R2   # 0.762


# d.) Fit a PCR model to the training set------------------------------------

# Fit Principle Component Regression Model - Set scale = True
help(pcr)

# Step1:  Fit to Entire Dataset
m3.pcr.all = pcr(Fertility~., data=fert_data, scale=TRUE, validation="CV")
print(summary(m3.pcr))   # M = 5, % Variance Explain w/ M=5 76.96%. 

# plotting the CV curves
help(validationplot)  
'Functions to plot validation statistics, such as RMSEP or R^2, as a 
 function of the number of components.'
validationplot(m3.pcr,val.type="MSEP")

# Step2:  Fit to Training Data
m3.pcr.train = pcr(Fertility~., data=train_fert_data, scale=TRUE, validation="CV")
print(summary(m3.pcr.train))   # M = 5, % Variance Explain w/ M=5 76.96%. 

# Step3:  Generate Prediction
test_fert_data$Fertility
m3.pcr.pred = predict(m3.pcr.train, train_fert_data,ncomp=5)
print(mean((pcr.pred-y.test)^2))# pcrMSE:96556.22


# Comapre Models
'Please see results reported at the end of each section(a-d)'




