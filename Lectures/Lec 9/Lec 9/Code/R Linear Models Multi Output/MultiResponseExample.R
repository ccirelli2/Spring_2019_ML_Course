rm(list = ls()) 
graphics.off()


library(ISLR)

MyData <- read.csv(file="Advertising.csv", header=TRUE,  sep=",")
attach(MyData)

# Removing the first row from the data file
MyData <- MyData[-1]

lm.fit <- lm(sales ~ TV + radio + newspaper ,data=MyData)

# getting a full summary of the fit:
print(summary(lm.fit))

# What if we have multiple outputs? For example having sales and newspaper both as the response variables

x1 = MyData[,'TV']
x2 = MyData[,'radio']

y1 = MyData[,'newspaper']
y2 = MyData[,'sales']

# model 
lm.fit2 <- lm(cbind(y1, y2) ~ x1 + x2) 
print(summary(lm.fit2))
# generating a synthetic test file from the training data
test = data.frame(MyData)
colnames(test) <- c("x1", "x2", "y1", "y2")
Yhat = predict(lm.fit2,test)