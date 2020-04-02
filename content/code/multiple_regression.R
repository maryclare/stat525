rm(list = ls())

link <- url("http://maryclare.github.io/stat525/content/homework/dwaine.RData")
load(link)
close(link) 

# Let's take a look at the data we've loaded
head(data)
# Data collected by a photo studio that specializes
# in portraits of children
# Each observation corresponds to a city 
# Y_i is the sales in city i
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
n <- nrow(data)
X <- cbind(rep(1, n), # Makes a vector of 1's, predictor corresponding to \beta_0
           data[, c("X1", "X2")]) # Extracts multiple columns from a data frame
X <- as.matrix(X) # For some reason R will work better if we ensure that X is being
                  # treated as a matrix (instead of a data frame)
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
t(X)%*%Y # Computing X'Y
t(X)%*%X # Computing X'X

# Matrix inversion!
solve(t(X)%*%X) # (X'X)^(-1) 

# Compute the least squares estimates using the 
# matrix formula!
b <- solve(t(X)%*%X)%*%t(X)%*%Y # b = (X'X)^(-1) X'Y

# We could alternatively do this using lm instead
# (This is what we'll actually do most of the time)
summary(lm(Y~X1+X2, data = data))
summary(lm(Y~X-1)) 

linmod <- lm(Y~X1+X2, data = data)
linmod$coefficients
b0 <- linmod$coefficients[1]
b1 <- linmod$coefficients[2]
b2 <- linmod$coefficients[3]

X1 <- data$X1
X2 <- data$X2
Y.hat <- b0 + b1*X1 + b2*X2 # Y.hat <- X%*%b, or Y.hat <- X%*%linmod$coefficients, linmod$fitted.values
e <- Y - Y.hat # linmod$residuals
s.sq <- sum(e^2)/(n - 3)
summary(linmod)
sqrt(s.sq) # summary(linmod)$sigma

# Compute sums of squares
sse <- sum(e^2)
ssr <- sum((Y.hat - mean(Y))^2)
ssto <- sum((Y - mean(Y))^2)

mse <- sse/(n - 3)
msr <- ssr/(3 - 1)
msto <- ssto/(n - 1)
        
F.star <- msr/mse # summary(linmod)$fstatistic

F.star
pf(F.star, 3 - 1, n - 3, lower.tail = FALSE)

summary(linmod)

R.sq <- ssr/ssto # summary(linmod)$r.squared
R.sq.a <- 1 - mse/msto # summary(linmod)$adj.r.squared

# Estimated variance-covariance matrix of the regression coefficient
# estimates
cov.b <- s.sq*solve(t(X)%*%X)
sqrt(diag(cov.b)) # summary(linmod)$coef[, "Std. Error"]
summary(linmod)
