rm(list = ls())

link <- url("http://maryclare.github.io/stat525/content/homework/plutonium.RData")
load(link)
close(link) 

# Let's take a quick look at the data
data

X <- data$X
Y <- data$Y
n <- length(Y)

plot(X, Y, pch = 16,
     xlab = "Plutonium Activity", ylab = "Alpha Count Rate")

# Do we see any evidence of problems??

# Let's check out the outlier
View(data[data$X == 0 & data$Y > 0.1, ])
# We can record the index of the outlier, so we can reference it later
outlier <- which(data$X == 0 & data$Y > 0.1)
outlier

# Turns out we can exclude it - BECAUSE - further investigation 
# indicated that something went wrong in the experiment for this point
X <- X[-outlier]
Y <- Y[-outlier]
n <- length(Y)

plot(X, Y, pch = 16,
     xlab = "Plutonium Activity", ylab = "Alpha Count Rate")

# What other issues do we see?
plot(X, log(Y), pch = 16,
     xlab = "Plutonium Activity", ylab = "Log Alpha Count Rate")
plot(X, sqrt(Y), pch = 16,
     xlab = "Plutonium Activity", ylab = "Square Root Alpha Count Rate")

# Let's try fitting the model and seeing how the fit looks
linmod <- lm(I(sqrt(Y))~X)
abline(a = linmod$coef[1], b = linmod$coef[2], col = "blue")
e <- linmod$residuals
plot(X, e, xlab = "Plutonium Activity", pch = 16,
     ylab =  "Residuals")
abline(h = 0, lty = 3)

# It seems like we're going to need to transform the predictor as well
plot(X^2, sqrt(Y), pch = 16,
     xlab = "Squared Plutonium Activity", 
     ylab = "Square Root Alpha Count Rate")
plot(sqrt(X), sqrt(Y), pch = 16,
     xlab = "Square Root Plutonium Activity", 
     ylab = "Square Root Alpha Count Rate")

linmod.new <- lm(I(sqrt(Y))~I(sqrt(X)))
abline(a = linmod.new$coef[1], b = linmod.new$coef[2], col = "blue")
e.new <- linmod.new$residuals
plot(X, e.new, xlab = "Plutonium Activity", pch = 16,
     ylab =  "Residuals")
abline(h = 0, lty = 3)

MSE <- sum(e^2)/(n - 2)
MSE.new <- sum(e.new^2)/(n - 2)

par(mfrow = c(1, 2))
qqnorm(e/sqrt(MSE), main = "Untransformed X")
qqnorm(e.new/sqrt(MSE.new), main = "Transformed X")

# The residual plots confirm our suspicions! We should transform X as well as Y
par(mfrow = c(1, 1))
plot(X, Y)
points(X, linmod.new$fitted.values^2, col = "blue")
