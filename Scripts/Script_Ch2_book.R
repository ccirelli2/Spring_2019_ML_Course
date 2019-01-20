# BOOK:  CHAPTER 2

# Clear Namespace

rm(list=ls())

# Set Working Directory & Load DataSet
setwd('/home/ccirelli2/Desktop/GSU/2019_Spring/Datasets')
dat <- read.csv('Advertising.csv', header = TRUE)

# Plot Sales Versus TV Spending
plot(dat$sales ~ dat$TV)
# Add Regression Line
abline(lm(dat$sales ~ dat$TV))

# Syntax Assign Values
a = 1
b <- 2

### Create a Matrix
#   Inputs:  data = c(), rows = #, ncol = #'''

x = matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)


### Sqrt() function
x.2 = sqrt(x)
x.2

### Exponent
x.pwr.2 = x^2
x.pwr.2

### Random Normal Value Generator
x = rnorm(50)       # rnorm(n, mean = 0, sd = 1)
y = rnorm(50)
cor(x,y)            #cor() calculates the correlation between these two sets
cor(dat$sales, dat$TV)
?cor
# Pearsons correlation coefficient explained: https://study.com/academy/lesson/pearson-correlation-coefficient-formula-example-significance.html

### Seed - Used to Generate the Same Set of Randome Variables
seed = set.seed(1303)     # can be any number that we pass to this seed
rnorm(50)


### Plotting:  Use plot()
x = rnorm(50)
y = rnorm(50)

# Save a plot
pdf('Figure.pdf')      # Start pdf function
test_plot = plot(x,y)  # call plot, which will be passed to the pdf function. 
dev.off()              # notify R that we are finished creating the plot


### Converting Numberical Data to qualitative 
as.factor(x)


### Plot a Histogram
hist(x)


### Scatter Plot Matrix:  pairs()
pairs(dat)











