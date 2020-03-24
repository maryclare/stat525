rm(list = ls())

link <- url("http://maryclare.github.io/stat525/content/homework/sales.RData")
load(link)
close(link) 

# Let's take a quick look at the data
data

X <- data$X
Y <- data$Y
n <- length(Y)

plot(X, Y, pch = 16,
     xlab = "Days", ylab = "Performance",
     ylim = c(0, 140), xlim = c(0, 3),
     main = "Scatter Plot")

linmod0 <- lm(Y~X)
e0 <- linmod0$residuals
plot(X, residuals(linmod), pch = 16)
abline(h = 0, lty = 3)


plot(sqrt(X), Y, pch = 16,
     xlab = expression(sqrt(X)), 
     ylab = "Performance",
     ylim = c(0, 140), 
     xlim = c(0.6, 1.6),
     main = expression(paste("Scatter Plot Against ", sqrt(X), sep = "")))

linmod <- lm(Y~I(sqrt(X)))
e <- linmod$residuals
plot(sqrt(X), e, pch = 16,
     xlab = expression(sqrt(X)), 
     ylab = "Residual",
     ylim = c(-11, 11), 
     xlim = c(0.7, 1.6),
     main = expression(paste("Residual Plot Against ", sqrt(X), sep = "")))
abline(h = 0, lty = 3)

par(mfrow = c(1, 2))
MSE <- sum(e^2)/(n - 2)
Ee <- sqrt(MSE)*qnorm((1:n - 0.375)/(n + 0.25))
plot(Ee, sort(e), pch = 16, 
     xlim = c(-10, 10),
     ylim = c(-10, 10),
     xlab = "Expected",
     ylab = "Residual",
     main = "Normal Probability Plot")

MSE0 <- sum(e0^2)/(n - 2)
Ee0 <- sqrt(MSE0)*qnorm((1:n - 0.375)/(n + 0.25))
plot(Ee0, sort(e0), pch = 16, 
     xlab = "Expected",
     ylab = "Residual",
     main = "Normal Probability Plot")

# If we want future plots to be on their own
par(mfrow = c(1, 1)) # Or dev.off()
