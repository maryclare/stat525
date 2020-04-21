rm(list = ls())

link <- url("http://maryclare.github.io/stat525/content/homework/perfectly_correlated.RData")
load(link)
close(link) 

# Take a look at the data
data
# Meaning of X1, X2, and Y not provided

pairs(data)

# We can clearly see that X1 and X2 are linearly related
cor(data$X1, data$X2)

# Let's try fitting a linear model to this data
linmod <- lm(Y~X1 + X2, data = data)
summary(linmod.X1X2) # We get some warnings, and can't estimate
                     # all of the parameters

# We have b0 = 3, b1 = 1, corresponding fitted values:
linmod$fitted.values

# What if we try different values of the regression coefficients?
-87 + data$X1 + 18*data$X2
-7 + 9*data$X1 + 2*data$X2
# These all give the same answer!
# There is not a unique set of b0, b1, and b2 that give the best fits
