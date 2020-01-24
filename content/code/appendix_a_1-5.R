rm(list = ls())
# This clears everything that's already in the workspace

set.seed(1) 
# This makes sure anything that relies on random number generation will
# be the same each time you run the code - can give any number

# Some normal distribution examples!
# First, we're going to compute normal distribution quantiles,
# Here - let's compute P(z <= 2), for normal z with mean 0 and variance 1
zA1 <- 2 
help(pnorm) # This brings you to some information about the pnorm function,
# which takes a value and returns the corresponding percentile of a 
# normal distribution
pnorm(zA1, mean = 0, sd = 1) # Compare this what's in the textbook!

# Now let's look at P(z <= -2) for normal z with mean 0 and variance 1, 
# and explore the different ways we can compute a normal percentile 
zA2 <- -2
pnorm(zA2, mean = 0, sd = 1)
# Because the normal distribution is symmetric about the mean, which in 
# this case is zero, P(z <= -2) = P(z > 2) 
# And P(z > 2) = 1 - P(z <= 2)
1 - pnorm(zA1, mean = 0, sd = 1)
# We also get the same answer by computing P(z > 2) directly, using 
# lower.tail=FALSE
pnorm(zA1, mean = 0, sd = 1, lower.tail = FALSE)

# We can always check our understanding by **simulating**
nsim <- 100000 # Number of random variables to simulate
sims <- rnorm(nsim, mean = 0, sd = 1) # Simulated z's from a normal dist. with mean 0 and variance 1
# This gives us an approximation to P(z > -2)
mean(sims > -2) 
# Check it against the exact answer...
pnorm(zA2, mean = 0, sd = 1, lower.tail = FALSE)

# Now let's take a look at them
hist(sims, freq = FALSE)
vals <- seq(-4, 4, length.out = 100)
lines(vals, dnorm(vals, mean = 0, sd = 1),
      col = "blue")
# Draw a vertical line at the simulation mean
abline(v = mean(sims), col = "red")

#### This is where we stopped in class on Thursday 1/23 #### 

# Compute Chi-Square Quantiles
nu <- 5
qchisq(0.9, df = nu)

# Get t distribution quantiles
nu <- 10
qt(0.9, nu)
# We could also approimate this by simulating t random variables!
nsim <- 100000
sims1 <- rt(nsim, df = nu)
quantile(sims1, probs = 0.9)

# Note that we could also simulate t random variables in another way,
# by simulating normal and chi square random variables
nsim <- 100000
sims2 <- rnorm(nsim)/sqrt(rchisq(nsim, df = nu)/nu)
quantile(sims2, probs = 0.9) # Quantiles are the same!

hist(sims1, freq = FALSE, breaks = 200)
hist(sims2, freq = FALSE, add = TRUE, col = rgb(1, 0, 0, 0.5),
     breaks = 200) 
vals <- seq(-5, 5, length.out = 100)
lines(vals, dt(vals, df = nu), lwd = 2)
# They have the same distribution!

# Now let's try working with the F-distribution
nu1 <- 2
nu2 <- 3
qf(0.9, df1 = nu1, df2 = nu2)
# This is a situation where being able to simulate can be a good way to
# check that we understand how the qf function is parameterized - 
# we should get the same answer by simulating F random variables using
# independent chi-square random variables, and computing their
# 90th percentile
nsim <- 1000000
sims <- (rchisq(nsim, df = nu1)/nu1)/(rchisq(nsim, df = nu2)/nu2)
quantile(sims, probs = 0.9)
