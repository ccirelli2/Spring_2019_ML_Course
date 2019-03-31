# Chapter 8 Lab: Decision Trees


rm(list = ls()) 
graphics.off()


library(tree)
library(ISLR)

# Fitting Regression Trees

library(MASS)
# Boston dataset 
# CRIM - per capita crime rate by town
# ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
# INDUS - proportion of non-retail business acres per town.
# CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
# NOX - nitric oxides concentration (parts per 10 million)
# RM - average number of rooms per dwelling
# AGE - proportion of owner-occupied units built prior to 1940
# DIS - weighted distances to five Boston employment centres
# RAD - index of accessibility to radial highways
# TAX - full-value property-tax rate per $10,000
# PTRATIO - pupil-teacher ratio by town
# B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# LSTAT - % lower status of the population
# MEDV - Median value of owner-occupied homes in $1000's


set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)
# pruning the tree!
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
mean((yhat-boston.test)^2)

# Bagging and Random Forests
# Recall that bagging is simply a special case of a random forest with m = p. 
# Therefore, the randomForest() function can be used to perform bagging
library(randomForest)
set.seed(1)
# mtry: number of variables to try
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
mean((yhat.bag-boston.test)^2)

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

# IncMSE: decrease of accuracy in predictions on the out of bag samples when a given variable is excluded from the model
# IncNodePurity: total decrease in node impurity (variations of a node output) that results from splits over that variable,
# averaged over all trees
importance(rf.boston)
varImpPlot(rf.boston)

# Boosting

library(gbm)
set.seed(1)
# we set distribution="gaussian" for regression problems
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)

yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
# shrinkage is the lamda in boosting 
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

