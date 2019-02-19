# R CODING IN CLASS---------------------------------------------------------------------------

# Clear namespace
rm(list = ls())

# Load Dataset
'Target variable:   binary, up or down
 Lag1-Lag5:         is the value of the stock 1-5 days prior. 
 Volume:            Stock you own
 Today:             value of stock today. 
'
library(ISLR)
stock_data = Smarket
head(stock_data)

# Fit Logistic Regression Model
glm1 = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = stock_data, family=binomial)
glm1_summary = summary(glm1)
'Interpretation:
- Pvalues are very high.  To be expected because predicting the stock value is extremey hard. 
                          Most of the stock prices are random. 
                          Professor said that when you see high p-values it could mean different things. 
                          Cant always jump to the conclusion that the features are meaningless.  
                          It could be the case that the model was incorrectly set up. 
- '

# Generate a Prediction 

glm.prediction = predict(glm1, type='response')
glm1.probs = glm.prediction[1:10]
glm1.probs
contrasts(stock_data$Direction)     # Contrast:  Set and view the contrasts associated with a factor. 
                                    # Contrast(x, contrasts = True, sparse = Fasle)
glm1.pred = rep("Down", 1250)       # rep replicates the values in x.  rep.int(x, times)

glm1.pred[glm.probs>.5]="Up"        # It looks like he is defining any prob > 0.5 as 'up'
table(glm1.pred, stock_data$Direction)  # Built confusion matrix 
print(mean(glm1.pred==stock_data$Direction)) 


