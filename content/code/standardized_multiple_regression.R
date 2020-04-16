rm(list = ls())

link <- url("http://maryclare.github.io/stat525/content/homework/dwaine.RData")
load(link)
close(link) 

# Let's take a look at the data we've loaded
data
# Data collected by a photo studio that specializes
# in portraits of children
# Each observation corresponds to a city 
# Y_i is the sales in city i
# X1_i is the number of persons age 16 or younger in city i
# X2_i is the per capita disposable income in city i

# Let's fit a linear model using the R command lm
linmod <- lm(Y~X1+X2, data = data)
summary(linmod)

# Let's standardize the response and covariates, and repeat
data$Y.t <- (data$Y - mean(data$Y))/(sd(data$Y))
data$X1.t <- (data$X1 - mean(data$X1))/(sd(data$X1))
data$X2.t <- (data$X2 - mean(data$X2))/(sd(data$X2))

linmod.t <- lm(Y.t~X1.t+X2.t, data = data)
summary(linmod.t)

# We can reconstruct the untransformed regression coefficients
g0 <- linmod.t$coefficients[1]
g1 <- linmod.t$coefficients[2]
g2 <- linmod.t$coefficients[3]

b1 <- g1*(sd(data$Y)/sd(data$X1))
b2 <- g2*(sd(data$Y)/sd(data$X2))
b0 <- mean(data$Y) - b1*mean(data$X1) - b2*mean(data$X2)

c(b0, b1, b2)
linmod$coef

