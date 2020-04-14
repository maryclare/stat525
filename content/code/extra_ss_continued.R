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

n <- length(Y)

an <- anova(lm(Y~X1+X2+X3))
an

an <- anova(lm(Y~X2+X1+X3))
an

# Let's use an F test to test if \beta_3 = 0
an <- anova(lm(Y~X2+X1+X3))
an

F.star <- 11.55/1/(98.40/16)
alpha <- 0.05
qf(1 - alpha, 1, n - 4)
F.star

ssr.X3.X2X1 <- an$`Sum Sq`[3]
sse.X1X2X3 <- an$`Sum Sq`[4]
(ssr.X3.X2X1/1)/(sse.X1X2X3/(n - 4))

# Note - the order doesn't matter *as long as* X_3 is last!
an <- anova(lm(Y~X1+X2+X3))
an

an <- anova(lm(Y~X1+X3+X2))
an

# Let's use an F test to test if \beta_2 = \beta_3 = 0
an <- anova(lm(Y~X1+X2+X3))

F.star <- (44.72/2)/(98.40/16)
alpha <- 0.05
qf(1 - alpha, 2, n - 4)
F.star
pf(F.star, 2, n - 4, lower.tail = FALSE)


ssr.X2X3.X1 <- sum(an$`Sum Sq`[2:3])
sse.X1X2X3 <- an$`Sum Sq`[4]
(ssr.X2X3.X1/2)/(sse.X1X2X3/(n - 4))

pf((ssr.X2X3.X1/2)/(sse.X1X2X3/(n - 4)), 
   2, n - 4, lower.tail = FALSE)

# Again - order doesn't matter *as long as* X_2 and X_3 are last!
an <- anova(lm(Y~X1+X3+X2))

# Also - we could use ANOVA to compare the two models directly, 
# without having to do our own algebra!
an <- anova(lm(Y~X1), lm(Y~X1+X2+X3))
an
# If we do it this way, order doesn't matter
an <- anova(lm(Y~X1), lm(Y~X2+X3+X1))
an

