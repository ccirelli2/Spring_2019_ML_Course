####################################################################################
'Goal:  Predict heart health of patients in a hospital. 
 Data:  HeartData.csv, 13 features'
####################################################################################



####################################################################################
####################################################################################
####################################################################################
'PART A:    HEART RATE DATA:'
hr_data = read.csv('/home/ccirelli2/Desktop/GSU/2019_Spring/ML_Course/HW/HW2/HeartData.csv')
summary(hr_data)
head(hr_data)
response_var = hr_data$num       # 0= heart ok, 1= heart issues

# Split Data
data_train = hr_data[0:200,]
data_test = hr_data[201:297,]



'age:      int
 sex:      int (binary)   1= male 
 cp:       int            chest pain
 trestbps  int            resting blood pressure
 chol      int            cholesterol
 fbs       int (binary)   fasting blood sugar, 
 restecg   int            resting electrolites
 thalach   int            max heart rate
 exang     int (binary)   agina, 1=yes
 oldpeak   float          ST depression
 slope     int            sloap of peak exersise
 ca        int            number of major vessels
 thal      int            no explanation provided
 num       int            target variable, diagnosis of heart disease. '
####################################################################################


####################################################################################
'a.) SPLIT DATA INTO TRAINING AND TEST.
     Training:  1-200
     Test:      201-297
     
     i.) Models:
         1. Logistic Regression:  Report p-values.  Which are largest?  Use the test data
                                 to estimate the accuracy of your model. 
         2. LDA 
         3. QDA
         4. KNN Question:  Amongst the four models, which appears to be the most accurate?
'
####################################################################################


####################################################################################
'LOGIST REGRESSION-----------------------------------------------------------------'
## Logistic Regression Model
lr.m1.train = glm(num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, data = data_train, family = 'binomial')
lr.m1.train.summary = summary(lr.m1.train)
lr.m1.train.summary
confint(lr.m1.train)

# P_values 
lr.m1.train.pvalues = lr.m1.train.summary$coefficients[,4]          # index pvalues
lr.m1.train.pvalues.vsalpha = data.frame(lr.m1.train.pvalues); colnames(lr.m1.train.pvalues.vsalpha) <- c('pvalues') # dataframe p values
lr.m1.train.pvalue.greater.alpha = subset(lr.m1.train.pvalues, lr.m1.train.pvalues.vsalpha$pvalues < 0.05)  # limit to those < alpha
lr.m1.train.pvalue.greater.alpha

'Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept) -10.371131   3.716461  -2.791 0.005261 ** 
age          -0.007346   0.031121  -0.236 0.813396    
sex           1.816097   0.703311   2.582 0.009817 ** 
cp            0.964215   0.290113   3.324 0.000889 ***
trestbps      0.034062   0.014006   2.432 0.015019 *  
chol          0.007474   0.004721   1.583 0.113430    
fbs          -1.056263   0.653642  -1.616 0.106102    
restecg       0.462748   0.244284   1.894 0.058185 .  
thalach      -0.028514   0.014497  -1.967 0.049200 *  
exang         0.635760   0.523550   1.214 0.224624    
oldpeak       0.241558   0.260197   0.928 0.353218    
slope         0.569182   0.454463   1.252 0.210414    
ca            0.959092   0.316431   3.031 0.002438 ** 
thal          0.344809   0.128253   2.689 0.007177 ** 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 275.26  on 199  degrees of freedom
Residual deviance: 126.96  on 186  degrees of freedom
AIC: 154.96

COMMENTS------------------------------------------------------

Pvalues:         There are a number of features whose p-values are above alpha = 0.05
                 This may indicate a poorness of fit of our model.  Will re-run using lr.m1.train.pvalue.greater.alpha
Null Deviance:   275.26 including only the intercept.  This decreases by > 54% when taking into consideration the 14 features. 
                 This would indicate that our features do have an impact on predicting our response variable. 
'

# Re-run with sex, cp, trestbps, thalach, ca, thal
lr.m2.train = glm(num ~ sex + cp + trestbps + thalach + ca + thal, data = data_train, family = 'binomial')
lr.m2.train.summary = summary(lr.m2.train)
lr.m2.train.summary
'Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -4.99773    2.55680  -1.955 0.050621 .  
sex          1.13030    0.54064   2.091 0.036557 *  
cp           0.97239    0.25488   3.815 0.000136 ***
trestbps     0.03024    0.01175   2.574 0.010058 *  
thalach     -0.03743    0.01168  -3.203 0.001360 ** 
ca           0.81422    0.25815   3.154 0.001610 ** 
thal         0.39806    0.11442   3.479 0.000503 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 275.26  on 199  degrees of freedom
Residual deviance: 145.50  on 193  degrees of freedom
AIC: 159.5

Notes:  It appears that the first model is actually better as the Residual deviation is small and AIC larger. 
'

# Generate Prediction m1---------------------------------------------------------------------------------------------------------
lr.m1.prediction.train = predict(lr.m1.train, type = 'response')         # response calculates exp(predict) / (1+exp(predict))
lr.m1.prediction.test = predict(lr.m1.train, data_test, type = 'response')

# Convert Logistic Regression Output to Binary Predictions
library(lattice)
library(ggplot2)
library(reshape2)
library(caret)
df = data.frame(lr.m1.prediction.train)
df$lr.m1.prediction.train[df$lr.m1.prediction.train > 0.5] <- 1
df$lr.m1.prediction.train[df$lr.m1.prediction.train < 0.5] <- 0

# Generate Confusion Matrix
lr.m1.cm = confusionMatrix(table(df$lr.m1.prediction.train, data_train$num))
'Confusion Matrix and Statistics
     0  1
  0 97 14
  1 13 76
  Accuracy : 0.865'
lr.m1.cm.accuracy = lr.m1.cm$overall['Accuracy']


# Generate Prediction m2------------------------------------------------------------------------------------------------------
lr.m2.prediction.train = predict(lr.m2.train, type = 'response')         # response calculates exp(predict) / (1+exp(predict))
lr.m2.prediction.test = predict(lr.m2.train, data_test, type = 'response')

# Convert Logistic Regression Output to Binary Predictions
library(lattice)
library(ggplot2)
library(reshape2)
library(caret)
df = data.frame(lr.m2.prediction.train)
df$lr.m2.prediction.train[df$lr.m2.prediction.train > 0.5] <- 1
df$lr.m2.prediction.train[df$lr.m2.prediction.train < 0.5] <- 0

# Generate Confusion Matrix
lr.m2.cm = confusionMatrix(table(df$lr.m2.prediction.train, data_train$num))
'Confusion Matrix and Statistics
     0  1
  0 98 16
  1 12 74
  Accuracy : 0.86'
lr.m2.cm.accuracy = lr.m2.cm$overall['Accuracy']
lr.m2.cm                  # Accuracy .86
lr.m2.cm.accuracy
####################################################################################



####################################################################################
'LINEAR DISCRIMINANT ANALYSIS------------------------------------------------------'
library(MASS)
# Train Model 
lda.m1.train = lda(num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, data = data_train)
lda.m1.train

# Generate Prediction
'The predict() function returns a list with three elements. The first ele- ment, class, contains LDA’s predictions'
lda.m1.pred = predict(lda.m1.train, data_train)
lda.m1.pred$class
lda.m1.pred$posterior
lda.m1.train.cf = confusionMatrix(table(data_train$num, lda.m1.pred$class))
lda.m1.train.cf    # Accuracy 0.86
'   0  1
  0 97 13
  1 15 75
Accuracy : 0.86            '
####################################################################################



####################################################################################
'QUADRATIC DISCRIMINATION FUNCTION--------------------------------------------------'
qda.m1.train = qda(num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, data = data_train)
qda.m1.train
qda.m1.pred = predict(qda.m1.train, data_train)
qda.m1.train.cf = confusionMatrix(table(data_train$num, qda.m1.pred$class))
qda.m1.train.cf    # Accuracy 0.88
'   0  1
  0 98 12
  1 12 78
  Accuracy : 0.88 '
####################################################################################


####################################################################################
'KNN NEAREST NEIGHBOR--------------------------------------------------------------'

# Convert Response Variable to a Factor
set.seed (1)
feature.inds = c('age', 'sex', 'cp', 'trestbps', 'chol', 'fbs', 'restecg', 'thalach', 'exang', 'oldpeak', 'slope', 'ca', 'thal')
test = c(1,2,3,4,5)

# Get Accuracy
'Compare prediction to test data:  knn.pred.1 == data_test[,num];       '
knn.pred.1 =knn(data_train[ , feature.inds], data_test[, feature.inds], data_train[,'num'], k=1); print(mean(knn.pred.1==data_test[,'num']))
knn.pred.5 =knn(data_train[ , feature.inds], data_test[, feature.inds], data_train[,'num'], k=5); print(mean(knn.pred.5==data_test[,'num']))
knn.pred.10 =knn(data_train[ , feature.inds], data_test[, feature.inds], data_train[,'num'], k=10); print(mean(knn.pred.10==data_test[,'num']))
'KNN          ACCURACY
 1            0.628
 5            0.711
 10           0.659
' 
####################################################################################


####################################################################################
'Amongst logistic regression, LDA, QDA and KNN, which one has the best accuracy?

MODEL       ACCURACY
KNN         0.711
QDA         0.880
LDA         0.860
LR          0.865


- Of all of the models, QDA performed the best.  Therefore, we might assume that the relationship
  between our independent and dependent variables is not truley linear. '
####################################################################################


####################################################################################
####################################################################################
####################################################################################
'PART B:
 1.) Repeat all steps in Part A, but this time treating “sex”, “cp”, “fbs”, “slope”, “exang”, “ca” and “thal”
     as categorical variables. '

# DATA.TRAIN:  Convert sex, cp, fbs, slope, exang, ca and thal to categorical variables. 
data_train.cat       = data_train
data_train.cat$sex   = factor(data_train.cat$sex)
data_train.cat$cp    = factor(data_train.cat$cp)
data_train.cat$fbs   = factor(data_train.cat$fbs)
data_train.cat$slope = factor(data_train.cat$slope)
data_train.cat$exang = factor(data_train.cat$exang)
data_train.cat$ca    = factor(data_train.cat$ca)
data_train.cat$thal  = factor(data_train.cat$thal)
summary(data_train.cat)

# DATA.TEST:  Convert sex, cp, fbs, slope, exang, ca and thal to categorical variables. 
data_test.cat       = data_test
data_test.cat$sex   = factor(data_test.cat$sex)
data_test.cat$cp    = factor(data_test.cat$cp)
data_test.cat$fbs   = factor(data_test.cat$fbs)
data_test.cat$slope = factor(data_test.cat$slope)
data_test.cat$exang = factor(data_test.cat$exang)
data_test.cat$ca    = factor(data_test.cat$ca)
data_test.cat$thal  = factor(data_test.cat$thal)
summary(data_test.cat)


# LOGISTIC REGRESSION MODEL--------------------------------------------------------------------------------------------------------------

## Logistic Regression Model
lr.m1.train.cm = glm(num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, data = data_train.cat, family = 'binomial')
lr.m1.train.cm.summary = summary(lr.m1.train.cm)
confint(lr.m1.train.cm)

# P_values 
lr.m1.train.cm.pvalues = lr.m1.train.cm.summary$coefficients[,4]          # index pvalues
lr.m1.train.pvalues.vsalpha = data.frame(lr.m1.train.cm.pvalues); colnames(lr.m1.train.cm.pvalues) <- c('pvalues') # dataframe p values
lr.m1.train.pvalue.greater.alpha = subset(lr.m1.train.pvalues, lr.m1.train.pvalues.vsalpha$pvalues < 0.05)  # limit to those < alpha
lr.m1.train.pvalue.greater.alpha

lr.m1.prediction.train.cm = predict(lr.m1.train.cm, type = 'response')         # response calculates exp(predict) / (1+exp(predict))
lr.m1.prediction.test.cm = predict(lr.m1.train.cm, data_test.cat, type = 'response')

# Convert Logistic Regression Output to Binary Predictions
df = data.frame(lr.m1.prediction.train.cm)
df$lr.m1.prediction.train[df$lr.m1.prediction.train.cm > 0.5] <- 1
df$lr.m1.prediction.train[df$lr.m1.prediction.train.cm < 0.5] <- 0

# Generate Confusion Matrix
lr.m1.cm = confusionMatrix(table(df$lr.m1.prediction.train, data_train$num))
lr.m1.cm
'Confusion Matrix and Statistics
      0   1
  0 103  13
  1   7  77
 Accuracy : 0.9'  
lr.m1.cm.accuracy = lr.m1.cm$overall['Accuracy']
lr.m1.cm.accuracy 

'Comments-----------------------------------------------------------------------------------------------------------
 1.) By converting “sex”, “cp”, “fbs”, “slope”, “exang”, “ca” and “thal” to categorical variables we see a significant
     improvement in the accuracy score for our Logistic Regression Model'

# LINEAR DISCRIMINANT ANALYSIS---------------------------------------------------------------------------------------
library(MASS)
# Train Model 
lda.m1.train = lda(num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, data = data_train.cat)
lda.m1.train

# Generate Prediction
'The predict() function returns a list with three elements. The first ele- ment, class, contains LDA’s predictions'
lda.m1.pred = predict(lda.m1.train, data_train.cat)
lda.m1.pred$class
lda.m1.pred$posterior
lda.m1.train.cf = confusionMatrix(table(data_train.cat$num, lda.m1.pred$class))
lda.m1.train.cf    # Accuracy 0.86
'Confusion Matrix
0   1
0 102   8
1  14  76
Accuracy : 0.89             

Comments---------------------------------------------------------------------------------------------------------------
 1.) We see a similar improvement with the LDA model '


#QUADRATIC DISCRIMINATION FUNCTION-----------------------------------------------------------------------------------------------------
qda.m1.train = qda(num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, data = data_train.cat)
qda.m1.train
qda.m1.pred = predict(qda.m1.train, data_train.cat)
qda.m1.train.cf = confusionMatrix(table(data_train.cat$num, qda.m1.pred$class))
qda.m1.train.cf    # Accuracy 0.88
' Confusion Matrix
  0   1
  0 102   8
  1  10  80
  Accuracy : 0.91    

Comments---------------------------------------------------------------------------------------------------------------
 1.) We see a similar improvement with the QDA model '


#KNN-----------------------------------------------------------------------------------------------------------------------
# Convert Response Variable to a Factor
set.seed (1)
feature.inds = c('age', 'sex', 'cp', 'trestbps', 'chol', 'fbs', 'restecg', 'thalach', 'exang', 'oldpeak', 'slope', 'ca', 'thal')

# Get Accuracy
'Compare prediction to test data:  knn.pred.1 == data_test[,num];       '
knn.pred.1 =knn(data_train.cat[ , feature.inds], data_test.cat[, feature.inds], data_train.cat[,'num'], k=1); print(mean(knn.pred.1==data_test.cat[,'num']))
knn.pred.5 =knn(data_train.cat[ , feature.inds], data_test.cat[, feature.inds], data_train.cat[,'num'], k=5); print(mean(knn.pred.5==data_test.cat[,'num']))
knn.pred.10 =knn(data_train.cat[ , feature.inds], data_test.cat[, feature.inds], data_train.cat[,'num'], k=10); print(mean(knn.pred.10==data_test.cat[,'num']))
'KNN          ACCURACY
 1            0.628
 5            0.711
 10           0.659

Comments---------------------------------------------------------------------------------------------------------------
 1.) Oddly there is no change to our KNN model results
' 


####################################################################################
####################################################################################
####################################################################################


'PART C:  APPLICATION OF LOOCV & K-FOLD TECHNIQUES-------------------------------------------------------------------

(c) In lecture 4 and specifically the R example (CVExample.R) you learned how to apply
the LOOCV and K-Fold CV to regression problems. In this homework we would like to
apply the LOOCV and K-Fold CV to our logistic regression, LDA and QDA models. 

  1.) Using 10-fold CV and LOOCV fit the models and report the classification accuracy for the 3 models
     (logistic regression, LDA and QDA). For this question use num as the response variable and
     all the other variables as features (again consider the features in part (b) as categorical).

     Note that for this part you would need to use the entire data when performing the K-fold CV
     and LOOCV (no more test/training splitting). (Hint: you may find the R “caret” package
     and function “trainControl” useful for this problem)

  2.) Is there much difference between the test accuracies of the three models when it comes
      to LOOCV and K-fold CV?

'
####################################################################################

'DATA PREP-------------------------------------------------------------------------------------------------'

#LIBRARIES
rm(list=ls())
library(boot)
library(caret)

# DATA (note we use the entire dataset for LOOCV)
hr_data = read.csv('/home/ccirelli2/Desktop/GSU/2019_Spring/ML_Course/HW/HW2/HeartData.csv')

# DATA  Convert sex, cp, fbs, slope, exang, ca and thal to categorical variables. 
data.cat             = hr_data
data.cat$sex          = factor(data.cat$sex)
data.cat$cp          = factor(data.cat$cp)
data.cat$fbs         = factor(data.cat$fbs)
data.cat$slope       = factor(data.cat$slope)
data.cat$exang       = factor(data.cat$exang)
data.cat$ca          = factor(data.cat$ca)
data.cat$thal        = factor(data.cat$thal)
data.cat$num         = factor(data.cat$num)
summary(data.cat)

'PART I:  LOGISTIC REGRESSION------------------------------------------------------------------------------'

# Step1:  Set up training control
data_ctrl = trainControl(method = 'cv', number = 10, verboseIter = TRUE)
data_ctrl_loocv = trainControl(method = 'LOOCV', number = 10, verboseIter = TRUE)

# Step2:  Fit Model and pass control parameters to model
lr.m1.cv =        train(num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, # model to fit
                  data = data.cat,                                                       # data
                  method = 'glm',                                                        # general linear model
                  family = 'binomial',                                                   # binomial for logistic regression
                  trControl = data_ctrl                                                  # pass training control parameters. 
                  )                                                                      

lr.m1.cv.loocv = train(num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, # model to fit
                       data = data.cat,                                                       # data
                       method = 'glm',                                                        # general linear model
                       family = 'binomial',                                                   # binomial for logistic regression
                       trControl = data_ctrl_loocv                                                  # pass training control parameters. 
                       )


# Step3:  Examine Model Predictions
lr.m1.cv
'Generalized Linear Model 

297 samples
13 predictor

No pre-processing
Resampling: Cross-Validated (10 fold) 
Summary of sample sizes: 267, 267, 268, 267, 267, 268, ... 

Resampling results:
RMSE       Rsquared   MAE      
0.3348804  0.5389519  0.2124769

RMSE:  Root mean squared error
'

lr.m1.cv.loocv
'Resampling results:
  Accuracy   Kappa    
  0.8484848  0.6933719
'

# Step4:  Investigate Final Model
lr.m1.cv$finalModel
mean(lr.m1.cv$resample$Rsquared)

# Step5:  Examine Model Predictions (R2) For Each Fold
lr.m1.cv$results
plot(lr.m1.cv$resample$RMSE, type='bar')
plot(lr.m1.cv$resample$Rsquared, type ='bar')
'  RMSE      Rsquared       MAE Resample
1  0.3834342 0.3760102 0.2551092   Fold01
2  0.2462384 0.7679812 0.1418298   Fold02
3  0.2883371 0.6654906 0.1819425   Fold03
4  0.3843226 0.4146836 0.2703684   Fold04
5  0.2544984 0.6929842 0.1385979   Fold05
6  0.4383896 0.2876839 0.2712754   Fold06
7  0.2951954 0.6752157 0.1955739   Fold07
8  0.4287678 0.3222896 0.2793528   Fold08
9  0.3222558 0.5595530 0.2296693   Fold09
10 0.3073651 0.6276266 0.1610502   Fold10
'


'LINEAR DISCRIMINATION ANALYSIS-------------------------------------------------'
data_ctrl = trainControl(method = 'cv', number = 10, verboseIter = TRUE)

lda.m1.cv =        train(num ~., # model to fit
                         data = data.cat,                                                       # data
                         method = 'lda',                                                        # general linear model
                         family = 'binomial',                                                   # binomial for logistic regression
                         trControl = data_ctrl                                                  # pass training control parameters. 
                         )                                                                      

lda.m1.cv$results


lda.m1.cv.loocv =        train(num ~., # model to fit
                         data = data.cat,                                                       # data
                         method = 'lda',                                                        # general linear model
                         family = 'binomial',                                                   # binomial for logistic regression
                         trControl = data_ctrl_loocv                                                  # pass training control parameters. 
                         )                                                                      

lda.m1.cv.loocv
'  Accuracy   Kappa    
  0.8451178  0.6863924'


'QUADRATIC DISCRIMINATION ANALYSIS-------------------------------------------------'
data_ctrl = trainControl(method = 'cv', number = 10, verboseIter = TRUE)

qda.m1.cv =        train(num ~., # model to fit
                         data = data.cat,                                                       # data
                         method = 'qda',                                                        # general linear model
                         family = 'binomial',                                                   # binomial for logistic regression
                         trControl = data_ctrl                                                  # pass training control parameters. 
                          )

qda.m1.cv.loocv =        train(num ~., # model to fit
                         data = data.cat,                                                       # data
                         method = 'qda',                                                        # general linear model
                         family = 'binomial',                                                   # binomial for logistic regression
                         trControl = data_ctrl_loocv                                                  # pass training control parameters. 
                          )                                                                      
qda.m1.cv.loocv
'  Accuracy   Kappa    
  0.8080808  0.6124224'



'RESULTS FROM THE LOGISTIC REGRESSION / LDA / QDA MODELS-------------------------------------'
lr.m1.cv$results
lda.m1.cv$results
qda.m1.cv$results
'
> lr.m1.cv$results
  parameter      RMSE  Rsquared       MAE     RMSESD RsquaredSD      MAESD
1      none 0.3348804 0.5389519 0.2124769 0.06936803  0.1735635 0.05548378
> lda.m1.cv$results
  parameter  Accuracy     Kappa AccuracySD   KappaSD
1      none 0.8048276 0.6067778 0.09193774 0.1838833
> qda.m1.cv$results
  parameter  Accuracy     Kappa AccuracySD  KappaSD
1      none 0.7911494 0.5774905 0.07905553 0.161233


Question:  Is there much difference between the test accuracies of the three models when it comes
to LOOCV and K-fold CV?



'













