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

# Now let's reconstruct the regression coefficients
# manually
n <- nrow(data) # Record the # of observations
X <- cbind(rep(1, n), # Makes a vector of 1's, predictor corresponding to \beta_0
           data[, c("X1", "X2")]) # Extracts multiple columns from a data frame
X <- as.matrix(X) # For some reason R will work better if we ensure that X is being
# treated as a matrix (instead of a data frame)
X
Y <- data$Y
Y

# Matrix multiplication!
t(X)%*%X # Computing X'X
# Could compute each element one at a time
# This is a lot of tedious work but builds intuition
sum(X[, 1]*X[, 1]) 
sum(X[, 1]*X[, 2]) 
sum(X[, 1]*X[, 3]) 
sum(X[, 2]*X[, 2]) 
sum(X[, 2]*X[, 3]) 
sum(X[, 3]*X[, 3]) 

t(X)%*%Y # Computing X'Y
# Again, could tediously compute one at a time to
# build intuition
sum(X[, 1]*Y)
sum(X[, 2]*Y)
sum(X[, 3]*Y)

# Matrix inversion! This is *not* something
# we can simplify much more very easily
solve(t(X)%*%X) # (X'X)^(-1) 

# Compute the least squares estimates using the 
# matrix formula!
solve(t(X)%*%X)%*%t(X)%*%Y # b = (X'X)^(-1) X'Y
linmod$coef
b <- linmod$coef

# We can construct the fitted values or extract them
# from the linmod object
cbind(X%*%b, linmod$fitted.values)

# We can construct the fitted values or extract them
# from the linmod object
cbind(Y - X%*%b, linmod$residuals)

par(mfrow = c(2, 2))
plot(linmod$fitted.values, linmod$residuals,
     xlab = "Fitted", ylab = "Residual", pch = 16)
plot(data$X1, linmod$residuals,
     xlab = "Targtpop", ylab = "Residual", pch = 16)
plot(data$X2, linmod$residuals,
     xlab = "Dispoinc", ylab = "Residual", pch = 16)
plot(data$X1*data$X2, linmod$residuals,
     xlab = expression(paste(X[1], X[2], sep = "")), 
     ylab = "Residual", pch = 16)


