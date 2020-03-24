# As usual, clear our workspace before we start
rm(list = ls())

# Load the data
load("~/Dropbox/Teaching/STAT525-2020/stat525/content/homework/toluca.RData")

# We can record the number of observations we have n
n <- nrow(data)

# Typing the name of a data frame, then a dollar sign, then
# the name of a variable lets us extract that variable
x <- data$X 
y <- data$Y

# We can use the lm function!
linmod <- lm(y~x)

# R will partition the sums of squares for us with anova!
an <- anova(linmod)
an

# Let's check these numbers!
y.hat <- fitted(linmod)
b0 <- linmod$coef[1]
b1 <- linmod$coef[2]
y.hat <- b0 + b1*x
sse <- sum((y - y.hat)^2) # SSE 
sse
ssr <- sum((y.hat - mean(y))^2) # SSR
ssr
an$`Sum Sq`

# Performing an F-test of the null hypothesis that \beta_1 = 0
f.star <- (ssr/1)/(sse/(n - 2))
an$`F value`

# If \beta_1 = 0, then f.star has an F_1,(n-2) distribution
alpha <- 0.05
qf(1 - alpha, df1 = 1, df2 = n - 2)
f.star # We reject at level alpha = 0.05
# What's the p-value? Pr(x > f.star) if x has a F_1,(n-2) distribution
pf(f.star, df1 = 1, df2 = n - 2, lower.tail = FALSE)
an$`Pr(>F)`

# How is this related to the t- or z-test of \beta_1 = 0?
b1 <- summary(linmod)$coef["x", "Estimate"]
sb1 <- summary(linmod)$coef["x", "Std. Error"]
t.star <- b1/sb1 # Under the null \beta_1 = 0, t.star has a t distribution with n-2 df
t.star^2 
f.star
# There is a nice fact that tells us that the quantiles of a squared t random variable with 
# n - 2 degrees of freedom are the same as the qunatiles of an F random variable
# with 1,(n-2) degrees of freedom

# R.squared
ssto <- sum((y - mean(y))^2)
ssto
sse + ssr
summary(linmod)
# The next two lines compute R^2
ssr/ssto 
1 - sse/ssto
