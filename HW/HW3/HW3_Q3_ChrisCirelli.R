# Q3 - Non-Linear Models:  Polynomials, Splines and GAMs-------------


# Load Data
setwd('/home/ccirelli2/Desktop/Repositories/Spring_2019_ML_Course/HW/HW3/HW3')
data_auto = read.csv('Auto.csv')
head(data_auto)
length(data_auto[,1])
train = data_auto[1:313, ]
test = data_auto[314:392, ]

# a1.) Plot mpg(response) in terms of horsepower
m1.lr = lm(mpg ~ horsepower, data = train)
#plot(m1.lr)
#plot(data_auto$mpg ~ data_auto$horsepower)

# a2.) Fit mpg to a polynomial of order 6 in terms of horsepower
m2.lr = lm(mpg ~ poly(train$horsepower, degree = 6), data = train)
m2.summary = summary(m2.lr)
m2.coef = coef(m2.summary)
m2.pred = predict(m2.lr, list(test$horsepower), se=TRUE)
se.bands=cbind(m2.pred$fit+2*m2.pred$se.fit,m2.pred$fit-2*m2.pred$se.fit)

# b.) Fit mpg to a natural splie with 6 degrees of freedom
library(splines)

# natural splines 
m3.spline = lm(mpg ~ ns(train$horsepower, df =6),data=train) # ns = natural spline
pred2=predict(m3.spline, newdata= list(test$horsepower),se=T)
plot(mpg,horsepower,col="gray")
m3.summary = summary(m3.spline)
m3.coef = coef(m3.summary)
m2.pred = predict(m3.spline, list(test$horsepower), se=TRUE)
se.bands=cbind(m2.pred$fit+2*m2.pred$se.fit,m2.pred$fit-2*m2.pred$se.fit)




