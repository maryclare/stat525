rm(list = ls())

link <- url("http://maryclare.github.io/stat525/content/homework/dwaine.RData")
load(link)
close(link) 

# Let's take a look at the data we've loaded
head(data)
# Data collected by a photo studio that specializes
# in portraits of children
# Each observation corresponds to a city 
# Y_i is the sales in community i
# X1_i is the number of persons age 16 or younger in city i
# X2_i is the per capita disposable income in city i

# Let's look at some scatterplots of the data
par(mfrow = c(1, 3))
plot(data$X1, data$Y, 
     xlab = expression(X[1]),
     ylab = "Y")
plot(data$X2, data$Y,
     xlab = expression(X[2]),
     ylab = "Y")
plot(data$X2, data$X1,
     xlab = expression(X[2]),
     ylab = expression(X[1]))

# A quicker way to make scatterplots 
# of every variable in a data frame is to use the
# "pairs" command
pairs(data)

# Now let's try out some matrix algebra
X <- as.matrix(data[, c("X1", "X2")]) # Extracts multiple columns from
                                      # a data frame
head(X)
Y <- data$Y

# We need to know the dimensions to 
# work with matrices
dim(X) # Gives the number of rows and columns
length(Y) # Gives number of elements in a vector

# The "t" function transposes a matrix
dim(X)
dim(t(X))

# Matrix multiplication!
t(X)%*%Y
t(X)%*%X

# Matrix inversion!
solve(t(X)%*%X)

# Compute the least squares estimates using the 
# matrix formula!
solve(t(X)%*%X)%*%t(X)%*%Y

# We could alternatively do this using lm instead
# (This is what we'll actually do most of the time)
summary(lm(Y~X)) 
summary(lm(Y~X1+X2, data = data)) # This is another way of doing the
                                  # same thing as summary(lm(Y~X)) 
