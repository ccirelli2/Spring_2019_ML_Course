###############################################################################################
#Load Libraries
library(boot)
library(ISLR)
###############################################################################################



###############################################################################################
# A.) 
'Note:  It is not clear from the explanation for which statistic we are calculating the MLE.
        Assume that the professor is asking for mu.'

###############################################################################################




###############################################################################################
# B.) Consider n = 10 and the below independent samples. 

# b.1:  Use Equation #1 to calculate the MLE for Theta. 
q1b.samples = c(0.3057, 0.7227, 1.1566, 2.8622, 1.3588, 0.5377, 0.4336, 0.3426, 3.5784, 2.7694)
q1b.df = data.frame("Samples" = q1b.samples) 
q1b.df.index = as.numeric(rownames(q1b.df))
q1b.length = length(q1b.samples)
q1b.mle = (4*length(q1b.samples)) / sum(q1b.samples)
q1b.mle   # 2.843393

# b.2:  Use the bootstrap technique with B = 50,000 to est the stdv for your calculated ml statistic
'Notes
 boot:    Both parametric and nonparametric resamplings are possible. 
          boot(data, statistic, R)
          data: either a vector, matrix or dataframe. 
          statistic:  A function, which when applied to the data returns a vector containing the statistic(s) of interest. 
          R:  The number of boostrap replicates. 
          
'
# Create Function to Pass to Bootstrap
q1b.function = function(df, indices) {
  d = df[indices, ]
  ml = 4*length(d) / sum(d)
  return(ml) }
  
# R = 50000
set.seed(1)
q1b.boot.50k = boot(q1b.df, q1b.function, R = 50000)
q1b.boot.50k  
plot(q1b.boot.50k)         # Not normal based on teh plot of the Q plot. Mean appears to be around 2.8. 
'Bootstrap Statistics :
    original    bias    std. error
t1* 2.843393 0.2270191   0.9267939
'

# R = 1000
q1b.boot.1k = boot(q1b.df, q1b.function, R = 1000)
q1b.boot.1k  
plot(q1b.boot.1k)
'Bootstrap Statistics :
    original    bias    std. error
t1* 2.843393 0.2051607   0.8694669'

## (C) In part (b) how much is your stdv est if you use B = 100?
#      Do you see a lot of difference in the calculated values?
'Its not a huge difference, 0.05.  I guess it would depend on the experiment the extent to which this sd er is significant'


















