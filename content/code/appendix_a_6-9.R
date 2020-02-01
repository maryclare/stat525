rm(list = ls())
# This clears everything that's already in the workspace

set.seed(1) 
# This makes sure anything that relies on random number generation will
# be the same each time you run the code - can give any number

## A.6
# Example 1
# Find 95% confidence interval for mu
n <- 10
y.bar <- 20
s <- 4

alpha <- 0.05

s.y.bar <- s/sqrt(n)
qt(1 - alpha/2, df = n - 1)

y.bar + qt(1 - alpha/2, df = n - 1)*s.y.bar # Upper 
y.bar + qt(alpha/2, df = n - 1)*s.y.bar # Lower
y.bar - qt(1 - alpha/2, df = n - 1)*s.y.bar # Another way to compute lower

# Example 2
# Test Null: mu <= 20 vs. Alternative mu > 20
n <- 15
y.bar <- 24
s <- 6

alpha <- 0.05

s.y.bar <- s/sqrt(n)
t.star <- (y.bar - 20)/s.y.bar
# Intuition: If the mean is really 20, then we expect t.star has a t_{n-1}
# distribution
# Also if the mean is really 20 or lower,
# then we would expect t.star to be very small, and we would
# consider large t.star's suggest the alternative is more plausible
# To build some intuition let's look at the t-density
vals <- seq(-10, 10, length.out = 200)
plot(vals, dt(vals, df = n - 1), type = "l")
# We want to reject when t is far to the right
t.star
qt(1 - alpha, df = n - 1)
abline(v = qt(1 - alpha, df = n - 1), col = "blue", lty = 2)
abline(v = t.star, col = "black", lty = 2)
# We reject the null - the mean is bigger than we'd expect 
# if the true mean were less than 20

# Example 3
# Test Null mu=10 vs. Alternative mu=/=10
n <- 25
y.bar <- 5.7
s <- 8

alpha <- 0.02

s.y.bar <- s/sqrt(n)

t.star <- (y.bar - 10)/s.y.bar
# Intuition: If the mean is really 10, we would expect t.star to be close to 0,
# not too big and not too small
# Furthermore, if the mean is really 10, then t.star has a t-distribution with 
# n-1 degrees of freedom, so we can determine the cutoffs by examining what values 
# are likely (and unlikely) if a random variable has a t_{n-1} distribution
abs(t.star)
qt(c(alpha/2, 1 - alpha/2), df = n - 1)
# We reject the null again - our test statistic is outside of the interval
# we would expect 95% of t.stars to be in if the true mean were 10

# Another way to perform the test - compute the 1 - alpha CI, and check if
# 10 is contained in it
y.bar + qt(1 - alpha/2, df = n - 1)*s.y.bar
y.bar + qt(alpha/2, df = n - 1)*s.y.bar # Nope! Not contained

# Example 4
# Go back to example 2, and compute the p-value
n <- 15
y.bar <- 24
s <- 6

s.y.bar <- s/sqrt(n)
t.star <- (y.bar - 20)/s.y.bar
# The p-value gives us the probability of observing a y.bar more extreme 
# (in this case, even bigger than our test statistic) 
# than the one we observed, if the null were true (if the mean were really <= 20)
1 - pt(t.star, df = n - 1)
pt(t.star, df = n - 1, lower.tail = FALSE)
# This probability is low - makes sense! y.bar is bigger than 20, and the standard
# error of the mean is pretty small, so it does seem like mu is likely bigger than 20

# Example 5
# Go back to Example 3, compute a p-value
n <- 25
y.bar <- 5.7
s <- 8

s.y.bar <- s/sqrt(n)

t.star <- (y.bar - 10)/s.y.bar
# For a two-sided test, the p-value is going to be the probability that we would observe
# a t.star even smaller than *or* even larger than what we observed, assuming the null
# were true and mu were really 10
2*pt(abs(t.star), n - 1, lower.tail = FALSE)

## A.7
# Example 6
# Get a 95% confidence interval for mu1 - mu2,
# assuming the variance is the same in both groups
n1 <- 10
y.bar <- 14
yiybarsq <- 105

n2 <- 20
z.bar <- 8
zizbarsq <- 224

alpha <- 0.05

ssq <- (yiybarsq + zizbarsq)/(n1 + n2 - 2)

t.star <- (y.bar - z.bar)/sqrt(ssq/sqrt(n1 + n2))
pt(-abs(t.star), df = n1 + n2 - 2) + 
  pt(abs(t.star), df = n1 + n2 - 2, lower.tail = FALSE)
2*pt(-abs(t.star), df = n1 + n2 - 2)

1 - pt(abs(t.star), df = n1 + n2 - 2)
pt(abs(t.star), df = n1 + n2 - 2, lower.tail = FALSE)

qt(c(alpha/2, 1 - alpha/2), df = n1 + n2 - 2)

y.bar - z.bar + qt(c(alpha/2, 1 - alpha/2), 
                   df = n1 + n2 - 2)*sqrt(ssq/n1 + ssq/n2)


# Example 7 - Continuation of Example 6 but with new alpha, 
# and now we want to test the null that mu1 = mu2 versus the alternative
# mu1 =/= mu2
alpha <- 0.1
t.star <- (y.bar - z.bar)/(sqrt(ssq/n1 + ssq/n2))
# Intuitively - we will reject when t.star is small
# We know that if the null is true,
# t.star is t distributed with n1 + n2 - 2 degrees of freedom
abs(t.star)
qt(c(alpha/2, 1 - alpha/2), 
   df = n1 + n2 - 2)
# We reject the null!

# Compute the one sided p-value - probability that
# the difference in means is even larger under the null?
pt(t.star, df = n1 + n2 - 2, lower.tail = FALSE)
# Compute the two sided p-value - probability that
# the difference in means is even larger or even smaller under the null?
2*pt(t.star, df = n1 + n2 - 2, lower.tail = FALSE)

## A.8
# Example 8
# Return to the data from Example 1
# Construct a 98% interval for the variance
n <- 10
y.bar <- 20
s <- 4
ssq <- s^2

alpha <- 0.02
# This is a little less intuitive - but we can construct a CI
# based because we know that (n-1)s^2/sigma^2 is 
# will be a chi-square random variable with n-1 degrees
# of freedom
(n - 1)*ssq/qchisq(c(1 - alpha/2, alpha/2), n - 1)

## A.9
# Example 9
# Compute a 90% interval for sigma1^2/sigma2^2
n1 <- 16
n2 <- 21

ssq1 <- 54.2
ssq2 <- 17.8

alpha <- 0.1

# We use the f distribution here, because 
# (ssq1/sigma1^2)/(ssq2/sigma2^2) has an F distribution
# with n1-1 and n2-1 degrees of freedom
ssq1/(ssq2)/qf(c(1 - alpha/2, alpha/2), n1 - 1, n2 - 1)

# Example 10 - Continuation of Example 9, perform a test of the
# null that the variances are equal versus the alternative that they are 
# not at level 0.02
alpha <- 0.02

t.star <- ssq1/(ssq2)
abs(t.star)
qf(c(alpha/2, 1 - alpha/2), n1 - 1, n2 - 1)
# Fail to reject the null

# Let's look at this on a plot just to see
vals <- seq(0, 4, length.out = 100)
plot(vals, df(vals, n1 - 1, n2 - 2),
     type = "l")
abline(v = qf(c(alpha/2, 1 - alpha/2), n1 - 1, n2 - 1),
       lty = 2, col = "blue")
abline(v = t.star, lty = 2)
