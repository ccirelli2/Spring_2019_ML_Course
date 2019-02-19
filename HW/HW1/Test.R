
data(cars)

lm1 = lm(dist ~ speed, data = cars)
summary(lm1)

speed2 = speed^2
lm2 = lm(dist ~ speed + I(speed^2), data = cars)
summary(lm2)

plot(cars$dist, cars$dist)
abline(lm1)
abline(lm2)


lung.data = read.csv('LungCapData.txt', header = FALSE, sep = '')
lung.data$