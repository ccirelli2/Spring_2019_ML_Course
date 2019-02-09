# Question 2____________________________________________________________________________________
'Newtons law:   states that every particle attracks every other particle
with a force which is directly proportional to the product of their
masses and inversely proportional to the square of the distance
between their centers. 
Dataset Fields:  Mass1, Mass2, Distance, Force
'
# Load Data
rm(list = ls())
setwd('/home/ccirelli2/Desktop/GSU/2019_Spring/ML_Course/HW/HW1')
data.gravity = read.csv('GravityForce.csv', header = TRUE)
head(data.gravity)

# Split Training & Test Data
typeof(data.gravity)
class(data.gravity)
data.training = data.gravity[0:200,]                      # training data 0-200 observations
data.test = data.gravity[201:240,]    # test data 201-240 observations. 

### Fit Multi Regression Model and Return Summary-------------------------------------------

# Generate Scatter Plot of Data:
'Distance appears to have the strongest linear relationship.'
plot(x = log(data.training$DISTANCE), y = log(data.training$FORCE))
plot(x = log(data.training$MASS1), y = log(data.training$FORCE))
plot(x = log(data.training$MASS2), y = log(data.training$FORCE))


# Generate Linear Model For FORCE ~ DISTANCE (Log vs no Log)
lm.gravity.dist = lm(FORCE ~ DISTANCE, data = data.training)
summary(lm.gravity.dist)

lm.gravity.dist.log = lm(log(FORCE) ~ log(DISTANCE), data = data.training)        # Log generates much stronger results. 
summary(lm.gravity.m1)
plot(lm.gravity.m1)
abline(lm.gravity.m1)

# Generate Multi Linear Model
'Results of multilinear regression look poor.  Very high Std. Error and p-values.  R2 is .11'
lm.gravity.multi = lm(FORCE ~ MASS1 + MASS2 + DISTANCE, data = data.training)
summary(lm.gravity.multi)
plot(lm.gravity)

## a.) report the fitted parameters------------------------------------------------------------
coefficients(lm.gravity.multi)                        # Returns B0 and B1
lm.gravity.coef = coef(summary(lm.gravity.multi))                       # returns all coefficients
lm.gravity.betas = lm.gravity.multi.coef[,1]
lm.gravity.std.errors = lm.gravity.coef[,2]
lm.gravity.tvalues = lm.gravity.coef[,3]
lm.gravity.pvalues = lm.gravity.coef[,4]

# 95% confidence interval for each estimated parameter (pg 81 ISLR)
'Notes:  Standard errors can be used to compute confidence intervals. 2stdv = 95% CI.  
SE(B0) = sigma.2 & [ 1/n * (x.bar^2 / sum(xi - x.bar))]                # Calculate SE of Intercept
SE(B1) = sigma.2 / sum(xi - X.bar)                                     # Calculate SE of Slope. 
sigma.2 = var(e) our epsilon term.                                     # Variacen = var of error term. 
RSE   = Residual Sum of Error, estimate for sigma.2, = sqrt(RSS / n-2)
CI(B1)  = B1 +- 2*SE(B1)                                               # CI.95 for B1
CI(B0)  = B0 +- 2*SE(B0)                                               # CI.95 for B0
'
# Betas
lm.gravity.coef
lm.gravity.B0 = lm.gravity.coef[1,1]
lm.gravity.B1 = lm.gravity.coef[-1,1]

# SE for B0 and B1 
B0.SE = lm.gravity.coef[1,2]
B1.SE = lm.gravity.coef[-1,2]

# Confidence Intervals for B0
CI.B0.UB = lm.gravity.B0 + 2*B0.SE
CI.B0.LB = lm.gravity.B0 - 2*B0.SE
CI.B0.95 = c(CI.B0.LB, CI.B0.UB)
CI.B0.95   # -11.46 TO 31.81
  
# Confidence Intervals for B1 (X, X.1, X.2)
CI.B1.UB = lm.gravity.B1 + 2*B1.SE
CI.B1.UB
CI.B1.LB = lm.gravity.B1 - 2*B1.SE
CI.B1.95 = c(CI.B1.LB, CI.B1.UB)
CI.B1.95   # MASS1   3.78 -06 TO  3.33 -05
           # MASS2   3.15 -07 TO  2.95 -05
           # DIST   -4.33 00  TO -1.33 00 

# R2 statistic (maybe just call each from the model)
'The r-statistic tells us how much of the variation in our response variable is predicted by our dependent variable. 
 In the case of this model, the R2 value is 0.1167, which is fairly low. One would think that natural laws would have a much strong
 linear relationship, almost deterministic.'

## b.) P-values--------------------------------------------------------------------------------------------------------- 
lm.gravity.pvalues = lm.gravity.coef[-1,4]
lm.gravity.pvalues
'ANALYSIS:
B0     PVALUE 0.348      The P-value is greater than alpha, which indicates that we may have an issue. 
B0     CI                -11.46 to 31.81:  Our actual intercept is 10.017, which falls right in the middle of our CI.  No issues here. 
MASS1  PVALUE 0.012      No issues as the p.value is below 5%.
MASS1  CI                MASS1   3.78 -06 TO  3.33 -05:  1.859 -05 falls within this range. 
MASS2  PVALUE 0.0423     P value is less than alpha, so no issues. 
MASS2  CI                3.15 -07 TO  2.95 -05:
DIST   PVAUE  0.000126   P value is less than alpha, so no issues. 
DIST   CI                -4.33 00  TO -1.33 00: Actual DIST value -2.73, which falls within our CI range. 
'

## c.) Generate Predictions for Test Data
'Use the fitted model to make predictions for test data. 
 Calculate Euclidean Distance between them. 
 syntax:  predict(fitted_model, test_data)'
rm(list = ls())
lm.gravity.multi.predict = predict(lm.gravity.multi, data.test)                    # Generate prediction for our test data. 
summary(lm.gravity.multi.predict)                                                  # print summary 
Euclidean.dist.lm1.multi = sqrt(sum(data.test$FORCE - lm.gravity.multi.predict)^2) # Calculate Square Root of Residual Sum of Squared Errors. 


## d.) FIT MULTI LINEAR REGRESSION MODEL TO LOG OF COEFFICIENTS----------------------------------------------------------------
'FIt the model using log-values of the features to the log-value of the response.
 Report the 95% CI for each beta and p-value.'
lm.gravity.multi.log = lm(log(FORCE) ~ log(MASS1) + log(MASS2) + log(DISTANCE), data = data.training)
summary(lm.gravity.multi.log)
'Note the R2 and R2-adjusted jupt to .97, the p-values are almost 0 and SE has fallen for almost all coefficients.'

# Retreive Std Errors for Coefficients
coef.lm.multi.log = coef(summary(lm.gravity.multi.log))
SE.B0    = coef.lm.multi.log[1,2] 
SE.MASS1 = coef.lm.multi.log[2,2]
SE.MASS2 = coef.lm.multi.log[3,2]
SE.DIST  = coef.lm.multi.log[4,2]

# Coefficeints
lm.log.b0 = coef.lm.multi.log[1,1]
lm.log.mass1 = coef.lm.multi.log[1,2]
lm.log.mass2 = coef.lm.multi.log[1,3]
lm.log.dist  = coef.lm.multi.log[1,4]

# Calculate CI
CI.B0.LOG.UB = lm.log.b0 + 2*SE.B0 
CI.B0.LOG.LB = lm.log.b0 - 2*SE.B0
B0.UB.LB = matrix(data = c(CI.B0.LOG.UB, lm.log.b0, CI.B0.LOG.LB), dimnames = list(c('Upper Bound', 'B0 Coefficient', 'Lower Bound')))
print('Intercept Confidence Intervals')
B0.UB.LB  # note that b0 is within the CI. 

CI.MASS1.LOG.UB = lm.log.mass1 + 2*SE.MASS1 
CI.MASS1.LOG.LB = lm.log.mass1 - 2*SE.MASS1
MASS1.UB.LB = matrix(data = c(CI.MASS1.LOG.UB, lm.log.mass1, CI.MASS1.LOG.LB), dimnames = list(c('Upper Bound', 'Mass1 Coefficient', 'Lower Bound')))
print('MASS 1 Confidence Intervals')
MASS1.UB.LB   # Mass1 coeff within CI. 


CI.MASS2.LOG.UB = lm.log.mass2 + 2*SE.MASS2 
CI.MASS2.LOG.LB = lm.log.mass2 - 2*SE.MASS2
MASS2.UB.LB = matrix(data = c(CI.MASS2.LOG.UB, lm.log.mass2, CI.MASS2.LOG.LB), dimnames = list(c('Upper Bound','Mass2 Coefficient', 'Lower Bound')))
print('MASS 2 Confidence Intervals')
MASS2.UB.LB   # Mass2 coeff within CI. 

CI.DIST.LOG.UB = lm.log.dist + 2*SE.DIST 
CI.DIST.LOG.LB = lm.log.dist - 2*SE.DIST
DIST.UB.LB = matrix(data = c(CI.DIST.LOG.UB, lm.log.dist, CI.DIST.LOG.LB), dimnames = list(c('Upper Bound','Distance Coefficient', 'Lower Bound')))
print('DIST Confidence Intervals')
DIST.UB.LB    # Distance coeff within CI.

# REVIEW PVALUES & CI VS MODEL COEFFICIENTS
lm.log.pvalues = coef(summary(lm.gravity.multi.log))[,4]
lm.log.pvalues # near 0 pvalues.  

# E.) COMPARE THE EUCLIDEAN DISTANCE BETWEEN THE NORMAL LM MODEL AND THE ONE WITH THE LOG OF THE COEFFICIENTS-------------------------------
'Calculation:  Here we add a logarithmic function to the calculation of the Euclidean distance that was not present in the other model'
lm.log.predict = predict(lm.gravity.multi.log, data.test)
Euclidean.dist.lm.log = sqrt(sum(log(data.test$FORCE - lm.log.predict))^2)
print(c('Euclidean Distance Comparison', 'Multilinear function = ', Euclidean.dist.lm1.multi, 'Log lm function =', Euclidean.dist.lm.log))

# F.) MODEL 3:  INTERACTION VARIABLE-------------------------------------------------------------------------------------------------------
'While there is some confusion regarding the professors question in the homework, I assume that he is asking that we add an interaction
term to create a third model'
lm3.multi.log = lm(log(FORCE) ~ log(MASS1) + log(MASS2) + log(DISTANCE) + (MASS1*MASS2*DISTANCE), data = data.training)
summary(lm3.multi.log)

# M3:  Pvalues 
lm3.multi.log.pvalues = coef(summary(lm3.multi.log))[,4]                 # Pvalues are almost all zero. 

# M3: Confidence Intervals
coef.lm3.multi.log = coef(summary(lm3.multi.log))
SE.B0    = coef.lm3.multi.log[1,2] 

# M3: Coefficients
LM3.B0 = coef.lm3.multi.log[1,1]
LM3.MASS1 = coef.lm3.multi.log[2,1]
LM3.MASS2 = coef.lm3.multi.log[3,1]
LM3.DIST  = coef.lm3.multi.log[4,1]
LM3.MASS1.MASS2 = coef.lm3.multi.log[5,1]
LM3.MASS1.DISTANCE = coef.lm3.multi.log[6,1]
LM3.MASS1.MASS2.DISTANCE = coef.lm3.multi.log[7,1]

# M3: Standard Errors
SE.B0 = coef.lm3.multi.log[1,2]
SE.MASS1 = coef.lm3.multi.log[2,2]
SE.MASS2 = coef.lm3.multi.log[3,2]
SE.DIST  = coef.lm3.multi.log[4,2]
SE.MASS1.MASS2 = coef.lm3.multi.log[5,2]
SE.MASS1.DISTANCE = coef.lm3.multi.log[6,2]
SE.MASS1.MASS2.DISTANCE = coef.lm3.multi.log[7,2]

# M3: Confidence Intervals
CI.M3.B0.UB = LM3.B0 + 2*SE.B0; CI.M3.B0.LOB = LM3.B0 - 2*SE.B0 
CI.M3.MASS1.UB = LM3.MASS1 + 2*SE.MASS1; CI.M3.MASS1.LOB = LM3.MASS1 - 2*SE.MASS1 
CI.M3.MASS2.UB = LM3.MASS2 + 2*SE.MASS2; CI.M3.MASS2.LOB = LM3.MASS2 - 2*SE.MASS2 
CI.M3.DIST.UB = LM3.DIST + 2*SE.DIST; CI.M3.DIST.LOB = LM3.DIST - 2*SE.DIST 


# G.) CALCULATE THE EUCLIDEAN DISTANCE-----------------------------------------------------------------------------------------------------
lm3.log.predict = predict(lm3.multi.log, data.test)
Euclidean.dist.lm3.log = sqrt(sum(log(data.test$FORCE - lm3.log.predict))^2)
Euclidean.dist.lm3.log

# H.) COMPARE M1, M2, M3-------------------------------------------------------------------------------------------------------------------
print(c('Compare Euclidean distances for M1-M3', 'M3 =>', Euclidean.dist.lm3.log, 'M2 =>', Euclidean.dist.lm.log, 'M1', Euclidean.dist.lm1.multi))
'Results:  M3 has the lowest Euclidean distance of the 3 models'








