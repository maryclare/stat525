rm(list = ls())

link <- url("http://maryclare.github.io/stat525/content/homework/uncorrelated.RData")
load(link)
close(link) 

# Take a look at the data
data
# X1 is crew size
# X2 is bonus pay in dollars
# Y is crew productivity

pairs(data)

# Hard to visually tell what's going on, but
# X1 and X2 are not correlated at all
cor(data$X1, data$X2)

linmod.X1 <- lm(Y ~ X1, data = data)
linmod.X2 <- lm(Y ~ X2, data = data)
linmod.X1X2 <- lm(Y~ X1 + X2, data = data)
linmod.X2X1 <- lm(Y~ X2 + X1, data = data)

# Looking at the X1 and X2 one at a time...
anova(linmod.X1)
anova(linmod.X2)

# Looking at the X1 and X2 one at them together
# Results are the same as results of looking
# at them one at a time, and don't depend on order
anova(linmod.X1X2)
anova(linmod.X2X1)

summary(linmod.X1)
summary(linmod.X1X2)
summary(linmod.X2)
