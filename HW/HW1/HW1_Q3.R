### Q3.) POLARIZATION _______________________________________________________________________________________________________
'The file corresponds to some wave field measurements through a material. The matter causes
 a change in the polarization (and magnitude) of the field when the temperature changes.
 
 The data file contains 1100 measurements of the temperature and the corresponding field
 measurement. The goal is to find a simple model which reliably relates the temperature to
 the filed value, i.e.,

 field = G(temp) with at most an order of 10

    G(temp) = β 0 + β 1 temp + β 2 temp 2 + . . . β 10 temp 10'


# LOAD DATA-----------------------------------------------------------------
'Train:  500, Test: 600'
rm(list = ls())
setwd('/home/ccirelli2/Desktop/GSU/2019_Spring/ML_Course/HW/HW1')
pol.data = read.csv('Polarization.csv')
head(pol.data)
plot(pol.data)

# Train / Test Split
train = pol.data[0:550,]
test  = pol.data[551:1100, ]



# A.) GENERATE LINEAR MODEL W/ ALL 10 FEATURES-------------------
'Relate field to temp'

# Try Simple Model
lm.simple.pol = lm(train$FIELD ~ train$TEMP)
summary(lm.simple.pol)
lm.simple.predict = predict(lm.simple.pol, test)
lm.simple.predict

# Plot simple model
plot(pol.data$TEMP, pol.data$FIELD)
abline(lm.simple.pol)


# Try Polynomial Model P-10
'Approaches to including polynomial terms
 1.) feature = feature ^p
 2.) lm(dependant ~ independent + I(independant^p))
 3.) lm(dependant ~ poly(independant, degree = #, raw = T/F))
     degree:  up to what degree.  If degree = 2, then the model will include 1 and 2
     raw:  if F, then R would fit orthogonal models
 Note:  Must always include all lower order coefficients in the model.  Cant just use x^3
'

lm1.polynomial = lm(train$FIELD ~ poly(train$TEMP, degree = 10, raw = T))
lm1.coef = coef(summary(lm1.polynomial))
lm1.summary = summary(lm1.polynomial)
lm1.predict = predict(lm1.polynomial, test)


# Report CI's for each estimated parameter
col_names = c('Upper', 'Actual', 'Lower')
row_names = seq(from = 0, to = 10, by = 1)
dim.names = list(row_names,col_names)
upper.bound = format(confint(lm1.polynomial)[,1], scientific = FALSE)
actual.coef = format(lm1.coef[,1], scientific = FALSE)
lower.bound = format(confint(lm1.polynomial)[,2], scientific = FALSE)
matrix(data = c(upper.bound, actual.coef, lower.bound), ncol = 3, dimnames = dim.names)

# Results
actual.v.upper = actual.coef < upper.bound
actual.v.lower = actual.coef > lower.bound
col_names = c('Actual v Upper', 'Actual v Lower')
row_names = seq(from = 0, to = 10, by = 1)
dim.names = list(row_names, col_names)
matrix(data = c(actual.v.upper, actual.v.lower), ncol = 2, dimnames = dim.names)     # Matrix of Whether Coef Was Between Upper/Lower CI. 

# P Value
'Are all p-values in a safe range?'
pvalue.less.alpha = coef(summary(lm1.polynomial))[,4] < 0.05
col_names = c('Actual v Upper', 'Actual v Lower', 'Pvalue Less Alpha')
row_names = seq(from = 0, to = 10, by = 1)
dim.names = list(row_names, col_names)
matrix(data = c(actual.v.upper, actual.v.lower, pvalue.less.alpha), ncol = 3, dimnames = dim.names)     # Matrix of Whether Coef Was Between Upper/Lower CI. 


# R2 Statistic
'Explain what the R2 statistic tells you:
 The R-statistic tells us how much of the variability in y is explained by our independent variables. 
 For this particular model, we get an R-squared of .9631 and Adjusted R-sqaured value of 0.9624, which are very high. 
'
summary(lm1.polynomial)


# B.) GENERATE PREDICTIONS FOR TEST DATA---------------------------------------------------------------------------
lm1.poly.predict = predict(lm1.polynomial, test)

# Calculate the Euclidean Distance For Test Data (Actual vs Predicted)
Euclidean.dist.lm1.poly = sqrt(sum((test$FIELD - lm1.poly.predict)^2))
Euclidean.dist.lm1.poly   # 313.0903


# C.) OPTIMAL MODEL-----------------------------------------------------------------------------------------------
'field = B0 + B3temp^3 + B6temp^6
 Train the model using this optimal solution.  
'
TEMP_3 = train$TEMP^3
TEMP_6 = train$TEMP^6
lm2.poly = lm(train$FIELD ~ train$TEMP + TEMP_3 + TEMP_6)
lm2.summary = summary(lm2.poly)
lm2.summary

# 95% CI 
CI_upper = confint(lm2.poly)[,1]
actual_betas = coef(summary(lm2.poly))[,1]
CI_lower = confint(lm2.poly)[,2]
CI.v.actuals = matrix(data = c(CI_upper, actual_betas, CI_lower), ncol = 3)

# P-values
'Intercept is greater than alpha'
lm2.pvalues.greater.alpha = coef(lm2.summary)[,4] > 0.05

# R2 statistic
'R-squared is 0.9624 and Ajusted R-squared is 0.9622'
lm2.summary

# Apply model to test data and calculate Euclidean distance
'Note:  I am getting an odd error here w/r/t the test data that is not allowing for the prediction to generate propertly'
lm2.poly.predict = predict(lm2.poly, test)
Euclidean.dist.lm2.poly = sqrt(sum((test$FIELD - lm2.poly.predict)^2))
Euclidean.dist.lm2.poly


# Results:
'Are all p-values in a safe range?'
lm2.pvalues.greater.alpha          #train$TEMP pvalue is greater than alpha.  Temp_3 and Temp_6 are not. 

'Compare the R2 values'
lm1.summary$r.squared
lm1.summary$adj.r.squared
lm2.summary$r.squared
lm2.summary$adj.r.squared
col.names = list('Model 1', 'Model 2')
r.square.comp = matrix(c(lm1.summary$r.squared, lm1.summary$adj.r.squared, lm2.summary$r.squared, lm2.summary$adj.r.squared), 
                       ncol = 2, nrow = 2)
r.square.comp                     # Model1 R2 .9631, Adj R2 .9623
                                  # Model2 R2 .9623, Adj R2 .9621

# d.) Backward Selection -----------------------------------------------------------------------------------------------------
'1.) Train model with all 10 factors
 2.) Progressively remove features with highest p-values
 3.) Redue the model and remove the next variable
 4.) Implement the approach until all p-values are below 0.01
 5.) Which features selected are the most important 
 6.) Is the model close to the model suggested in part (c)?'

rm(list = ls())
setwd('/home/ccirelli2/Desktop/GSU/2019_Spring/ML_Course/HW/HW1')
pol.data = read.csv('Polarization.csv')
head(pol.data)
plot(pol.data)

# Create Train/Test Split
train = pol.data[0:550,]
test  = pol.data[551:1100, ]

# Features
TEMP_1 = train$TEMP
TEMP_2 = train$TEMP^2
TEMP_3 = train$TEMP^3
TEMP_4 = train$TEMP^4
TEMP_5 = train$TEMP^5
TEMP_6 = train$TEMP^6
TEMP_7 = train$TEMP^7
TEMP_8 = train$TEMP^8
TEMP_9 = train$TEMP^9
TEMP_10 = train$TEMP^10

# Model 1
lm1d = lm(train$FIELD ~ TEMP_1 + TEMP_2 + TEMP_3 + TEMP_4 + TEMP_5 + TEMP_6 + TEMP_7 + TEMP_8 + TEMP_9 + TEMP_10)
lm1d.summary = summary(lm1d)
lm1d.summary
lm1d.pvalues = lm3.summary$coefficients[2:11,4]
lm1d.pvalues
'RESULTS:

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.321e+00  2.079e-01  11.166  < 2e-16 ***
TEMP_1       4.329e-02  4.508e-02   0.960  0.33733    
TEMP_2      -9.850e-03  5.469e-03  -1.801  0.07227 .  
TEMP_3      -1.901e-03  6.223e-04  -3.056  0.00236 ** 
TEMP_4       6.781e-05  3.385e-05   2.003  0.04569 *  
TEMP_5       2.281e-06  2.890e-06   0.789  0.43023    
TEMP_6      -1.311e-07  7.089e-08  -1.849  0.06504 .  
TEMP_7      -2.662e-09  5.491e-09  -0.485  0.62800    
TEMP_8       1.558e-10  7.009e-11   2.222  0.02667 *  
TEMP_9       9.046e-13  3.691e-12   0.245  0.80650    
TEMP_10     -4.919e-14  5.912e-14  -0.832  0.40576    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.844 on 539 degrees of freedom
Multiple R-squared:  0.9639,	Adjusted R-squared:  0.9632 
F-statistic:  1438 on 10 and 539 DF,  p-value: < 2.2e-16

Removing largest, whcih is TEMP_9
'

# Model 2
lm2d = lm(train$FIELD ~ TEMP_1 + TEMP_2 + TEMP_3 + TEMP_4 + TEMP_5 + TEMP_6 + TEMP_7 + TEMP_8 + TEMP_10)
lm2d.summary = summary(lm2d)
lm2d.summary
lm2d.pvalues = lm3.summary$coefficients[,4]
lm2d.pvalues
'RESULTS:

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.334e+00  2.004e-01  11.651  < 2e-16 ***
TEMP_1       3.690e-02  3.675e-02   1.004  0.31574    
TEMP_2      -1.058e-02  4.572e-03  -2.315  0.02098 *  
TEMP_3      -1.778e-03  3.646e-04  -4.876 1.43e-06 ***
TEMP_4       7.261e-05  2.759e-05   2.631  0.00875 ** 
TEMP_5       1.624e-06  1.074e-06   1.511  0.13127    
TEMP_6      -1.375e-07  6.574e-08  -2.092  0.03690 *  
TEMP_7      -1.337e-09  9.486e-10  -1.409  0.15939    
TEMP_8       1.479e-10  6.235e-11   2.373  0.01800 *  
TEMP_10     -3.518e-14  1.507e-14  -2.335  0.01993 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.842 on 540 degrees of freedom
Multiple R-squared:  0.9639,	Adjusted R-squared:  0.9633 
F-statistic:  1600 on 9 and 540 DF,  p-value: < 2.2e-16

Remove:  TEMP_1
'


# Model 3
lm3d = lm(train$FIELD ~ TEMP_2 + TEMP_3 + TEMP_4 + TEMP_5 + TEMP_6 + TEMP_7 + TEMP_8 + TEMP_10)
lm3d.summary = summary(lm3d)
lm3d.summary
lm3d.pvalues = lm3.summary$coefficients[,4]
lm3d.pvalues

'RESULTS:

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.304e+00  1.981e-01  11.633   <2e-16 ***
TEMP_2      -9.345e-03  4.402e-03  -2.123   0.0342 *  
TEMP_3      -1.445e-03  1.513e-04  -9.550   <2e-16 ***
TEMP_4       6.195e-05  2.547e-05   2.432   0.0153 *  
TEMP_5       7.563e-07  6.387e-07   1.184   0.2369    
TEMP_6      -1.070e-07  5.829e-08  -1.836   0.0669 .  
TEMP_7      -6.489e-10  6.563e-10  -0.989   0.3233    
TEMP_8       1.183e-10  5.493e-11   2.154   0.0317 *  
TEMP_10     -3.033e-14  1.427e-14  -2.125   0.0341 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.842 on 541 degrees of freedom
Multiple R-squared:  0.9638,	Adjusted R-squared:  0.9633 
F-statistic:  1800 on 8 and 541 DF,  p-value: < 2.2e-16

Removing TEMP_5
'

# Model 4
lm4d = lm(train$FIELD ~ TEMP_2 + TEMP_3 + TEMP_4 + TEMP_6 + TEMP_7 + TEMP_8 + TEMP_10)
lm4d.summary = summary(lm4d)
lm4d.summary
lm4d.pvalues = lm3.summary$coefficients[,4]
lm4d.pvalues

'RESULTS:

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.293e+00  1.979e-01  11.587   <2e-16 ***
TEMP_2      -8.296e-03  4.314e-03  -1.923   0.0550 .  
TEMP_3      -1.270e-03  3.370e-05 -37.695   <2e-16 ***
TEMP_4       5.023e-05  2.348e-05   2.140   0.0328 *  
TEMP_6      -6.753e-08  4.783e-08  -1.412   0.1586    
TEMP_7       1.191e-10  1.003e-10   1.188   0.2353    
TEMP_8       7.475e-11  4.079e-11   1.832   0.0675 .  
TEMP_10     -2.139e-14  1.212e-14  -1.765   0.0781 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


Remove:  TEMP_6 and TEMP_7 and TEMP_2
'

# Model 5
lm5d = lm(train$FIELD ~ TEMP_3 + TEMP_4 + TEMP_8 + TEMP_10)
lm5d.summary = summary(lm5d)
lm5d.summary
lm5d.pvalues = lm3.summary$coefficients[,4]
lm5d.pvalues


'
Summary:
- Model 5 is the optimal model, i.e. all p values are below 0.01. 
- By progressively removing TEMP we see the impact on teh overall p-values.  
- The Final model is not close to the one demonstrated in part (c). 
- Oddly the adjusted R-Squared values are exactly the same. 
'


# e.) Apply Model developed in part(d)----------------------------------------------------------------------
'1.) Calculate the Euclidean distance
'
lm5d.predict = predict(lm5d, test)
Euclidean.dist.lm5d = sqrt(sum((test$FIELD - lm5d.predict)^2))

# Summary:
'Results:
  Euclidean.dist.lm1.poly     313.0903
  Euclidean.dist.lm2.poly     312.983
  Euclidean.dist.lm5d         313.0392

Commentary:
  - The model suggested by the homework (lm2) with fitted parameters "lm(train$FIELD ~ train$TEMP + TEMP_3 + TEMP_6)"
  produces the lowest RSS or Euclidean distance.  
  - lm2 would therefore be the best model to chose. '


' 2.) How do E1, E2 and E3 compare?
      See above under Summary'


'3.) Which model do you suggest to pick and why. 
     See above under Summary'


### PLEASE HAND IN YOUR CODE WITH A COMPREHENSIVE RESPONSE 
### TO EACH QUESTION


















