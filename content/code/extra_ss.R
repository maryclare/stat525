rm(list = ls())

link <- url("http://maryclare.github.io/stat525/content/homework/body_fat.RData")
load(link)
close(link) 

# Let's take a look at the data we've loaded
head(data)
Y <- data$Y
X1 <- data$X1
X2 <- data$X2
X3 <- data$X3

pairs(data)

linmod.X1 <- lm(Y~X1)
linmod.X1X2 <- lm(Y~X1+X2)
linmod.X1X2X3 <- lm(Y~X1+X2+X3)

ssr.X1 <- sum((linmod.X1$fitted.values - mean(Y))^2)
ssr.X1X2 <- sum((linmod.X1X2$fitted.values - mean(Y))^2)
ssr.X1X2X3 <- sum((linmod.X1X2X3$fitted.values - mean(Y))^2)

sse.X1 <- sum((linmod.X1$residuals)^2)
sse.X1X2 <- sum((linmod.X1X2$residuals)^2)
sse.X1X2X3 <- sum((linmod.X1X2X3$residuals)^2)

ssto <- sum((Y - mean(Y))^2)

ssto

ssr.X1 + sse.X1
ssr.X1X2 + sse.X1X2
ssr.X1X2X3 + sse.X1X2X3

ssr.X2.X1 <- ssr.X1X2 - ssr.X1
ssr.X3.X1X2 <- ssr.X1X2X3 - ssr.X1X2

anova(linmod.X1X2X3)
ssr.X1
ssr.X2.X1
ssr.X3.X1X2
sse.X1X2X3

# Order matters!
anova(lm(Y~X2+X1+X3))
anova(lm(Y~X3+X2+X1))
