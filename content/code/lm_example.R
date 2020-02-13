# As usual, clear our workspace before we start
rm(list = ls())

# Load the data
load("~/Dropbox/Teaching/STAT525-2020/stat525/content/homework/crime.RData")

str(data) # This tells us that data is what R calls a data frame

# We can look at a data frame just like we would if it were in Excel or
# another type of spreadsheet software
# This lets us see the names of the variables in the data frame - X and Y
View(data)
# We could also just look at the first few rows of data
head(data)
# We can record the number of observations we have n
n <- nrow(data)

# Typing the name of a data frame, then a dollar sign, then
# the name of a variable lets us extract that variable
x <- data$X 
y <- data$Y

# Note - if we just have a vector the number of observations would be
n <- length(y)

# First let's compute b1 by hand
x.bar <- mean(x)
y.bar <- mean(y)

num <- sum((x - x.bar)*(y - y.bar))
den <- sum((x - x.bar)^2)

b1 <- num/den

# Going forward, we don't always need to do computation by hand
# We can use the lm function!
linmod <- lm(y~x)

# We can pull b0 and b1 out of linmod
b0 <- linmod$coef[1]
b1 <- linmod$coef[2]

# We can also grab the residuals
e <- y - (b0 + b1*x)
e <- linmod$residuals

# And we can also get estimate of the standard deviation, s
ssq <- sum(e^2)/(n - 2)
sqrt(ssq)
summary(linmod)$sigma

# Last we can get fitted vaules
y.hat <- linmod$fitted.values
y.hat <- b0 + b1*x

# And check some properties of the residuals that we have
# talked about in class
mean(e)
sum(e*x)
sum(e*y.hat)



