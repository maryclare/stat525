rm(list = ls())

link <- url("http://maryclare.github.io/stat525/content/homework/plasma.RData")
load(link)
close(link) 

# Let's take a quick look at the data
data

X <- data$X
Y <- data$Y
n <- length(Y)

# cbind(data[, 3], log(Y, base = 10))

plot(X, Y, pch = 16,
     xlab = "Age", ylab = "Plasma Level",
     ylim = c(0, 20), xlim = c(0, 4),
     main = "Scatter Plot")

plot(X, log(Y, base = 10), pch = 16,
     xlab = "X", 
     ylab = "Y'",
     ylim = c(0.6, 1.3), 
     xlim = c(0, 4),
     main = expression(paste("Scatter Plot with Y'=", 'log'[10](Y), sep = "")))

linmod <- lm(I(log(Y, base = 10))~X)
e <- linmod$residuals
plot(X, e*10^2, pch = 16,
     xlab = "X", 
     ylab = expression(paste("Residual x ", 10^2, sep = "")),
     ylim = c(-12, 17), 
     xlim = c(0, 4),
     main = expression(paste("Residual Plot Against ", X, sep = "")))
abline(h = 0, lty = 3)

MSE <- sum(e^2)/(n - 2)
Ee <- sqrt(MSE)*qnorm((1:n - 0.375)/(n + 0.25))
plot(Ee*10^2, sort(e)*10^2, pch = 16, 
     xlim = c(-15, 17),
     ylim = c(-15, 17),
     xlab =  expression(paste("Expected x ", 10^2, sep = "")),
     ylab =  expression(paste("Residual x ", 10^2, sep = "")),
     main = "Normal Probability Plot")
