rm(list = ls())
# This clears everything that's already in the workspace

set.seed(1) 
# This makes sure anything that relies on random number generation will
# be the same each time you run the code - can give any number

## A.6
# Example 1
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
n <- 15
y.bar <- 24
s <- 6

alpha <- 0.05

s.y.bar <- s/sqrt(n)
t.star <- (y.bar - 20)/s.y.bar
t.star
qt(1 - alpha, df = n - 1)
# We reject the null

# Example 3
n <- 25
y.bar <- 5.7
s <- 8

alpha <- 0.02

s.y.bar <- s/sqrt(n)

t.star <- (y.bar - 10)/s.y.bar
abs(t.star)
qt(c(alpha/2, 1 - alpha/2), df = n - 1)
# We reject the null again

# Another way to perform the test - compute the 1 - alpha CI, and check if
# 10 is contained in it
y.bar + qt(1 - alpha/2, df = n - 1)*s.y.bar
y.bar + qt(alpha/2, df = n - 1)*s.y.bar # Nope! Not contained

# Example 4
# Go back to example 2, and compute the p-value
n <- 15
y.bar <- 24
s <- 6

alpha <- 0.05

s.y.bar <- s/sqrt(n)
t.star <- (y.bar - 20)/s.y.bar
1 - pt(t.star, df = n - 1)
pt(t.star, df = n - 1, lower.tail = FALSE)

# Example 5
# Go back to Example 3, compute a p-value
n <- 25
y.bar <- 5.7
s <- 8

alpha <- 0.02

s.y.bar <- s/sqrt(n)

t.star <- (y.bar - 10)/s.y.bar
2*pt(abs(t.star), n - 1, lower.tail = FALSE)

## A.7
# Example 6
n1 <- 10
y.bar <- 14
yiybarsq <- 105

n2 <- 20
z.bar <- 8
zizbarsq <- 224

alpha <- 0.05

ssq <- (yiybarsq + zizbarsq)/(n1 + n2 - 2)


y.bar - z.bar + qt(c(alpha/2, 1 - alpha/2), 
                   df = n1 + n2 - 2)*sqrt(ssq/n1 + ssq/n2)
# Example 7 - Continuation of Example 6 but with new alpha
alpha <- 0.1
t.star <- (y.bar - z.bar)/(sqrt(ssq/n1 + ssq/n2))
abs(t.star)
qt(c(alpha/2, 1 - alpha/2), 
   df = n1 + n2 - 2)
# Reject the null

# Compute the one sided p-value
pt(t.star, df = n1 + n2 - 2, lower.tail = FALSE)
# Compute the two sided p-value
2*pt(t.star, df = n1 + n2 - 2, lower.tail = FALSE)

## A.8
# Example 8
# Return to the data from Example 1
n <- 10
y.bar <- 20
s <- 4
ssq <- s^2

alpha <- 0.02
(n - 1)*ssq/qchisq(c(1 - alpha/2, alpha/2), n - 1)

## A.9
# Example 9
n1 <- 16
n2 <- 21

ssq1 <- 54.2
ssq2 <- 17.8

alpha <- 0.1

ssq1/(ssq2)/qf(c(1 - alpha/2, alpha/2), n1 - 1, n2 - 1)

# Example 10 - Continuation of Example 9 with different alpha
alpha <- 0.02

t.star <- ssq1/(ssq2)
abs(t.star)
qf(c(alpha/2, 1 - alpha/2), n1 - 1, n2 - 1)
# Fail to reject the null
