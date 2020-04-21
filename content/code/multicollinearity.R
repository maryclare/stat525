rm(list = ls())

link <- url("http://maryclare.github.io/stat525/content/homework/body_fat.RData")
load(link)
close(link) 

n <- nrow(data)

# Replicates Figure 7.3 from the test
pairs(data[, c("X1", "X2", "X3")])
cor(data[, c("X1", "X2", "X3")])

lm.X1 <- lm(Y~X1, data = data)
lm.X2 <- lm(Y~X2, data = data)
lm.X1X2 <- lm(Y~X1 + X2, data = data)
lm.X1X2X3 <- lm(Y~X1 + X2 + X3, data = data)

# How does this affect the coefficient estimates?
b.ests <- data.frame("b1" = c(lm.X1$coefficients["X1"],
                              NA,
                              lm.X1X2$coefficients["X1"],
                              lm.X1X2X3$coefficients["X1"]),
                     "b2" = c(NA,
                              lm.X2$coefficients["X2"],
                              lm.X1X2$coefficients["X2"],
                              lm.X1X2X3$coefficients["X2"]))
b.ests

# How does this effect the extra sums of squares?
anova(lm(Y~X2, data = data))
anova(lm(Y~X1 + X2, data = data))
anova(lm(Y~X2 + X1, data = data))

# How does this affect our uncertainty about regression coefficients?
s.bs <- data.frame("b1" = c(summary(lm.X1)$coef["X1", "Std. Error"],
                              NA,
                              summary(lm.X1X2)$coef["X1", "Std. Error"],
                              summary(lm.X1X2X3)$coef["X1", "Std. Error"]),
                     "b2" = c(NA,
                              summary(lm.X2)$coef["X2", "Std. Error"],
                              summary(lm.X1X2)$coef["X2", "Std. Error"],
                              summary(lm.X1X2X3)$coef["X2", "Std. Error"]))

# How does this affect our residual errors?
mse.X1 <- sum(lm.X1$residuals^2)/(n - 2)
mse.X1X2 <- sum(lm.X1X2$residuals^2)/(n - 3)
mse.X1X2X3 <- sum(lm.X1X2X3$residuals^2)/(n - 4)

# How does this affect our Y.hats and their standard errors?
X.h <- data.frame("X1" = 25, "X2" = 50, "X3" = 29)
pred.X1 <- predict(lm.X1, X.h, se.fit = TRUE)
pred.X1X2 <- predict(lm.X1X2, X.h, se.fit = TRUE)
pred.X1X2X3 <- predict(lm.X1X2X3, X.h, se.fit = TRUE)
c(pred.X1$fit, pred.X1$se.fit)
c(pred.X1X2$fit, pred.X1X2$se.fit)
c(pred.X1X2X3$fit, pred.X1X2X3$se.fit)
