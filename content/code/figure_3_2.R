rm(list = ls())

# Let's practice making some more plots
# and learn about using the residuals to check model assumptions
# We're going to replicate Figure 3.2 in the
# .pdf version of the text

# Again, we can load our data files directly from
# the course site - as long as we're connected to the 
# internet
# Step 1: Tell R what the link is and tell R it's a "url"
link <- url("http://maryclare.github.io/stat525/content/homework/toluca.RData")
# Step 2: Load the data into the session
load(link)
# Step 3: Close the link! I'm not sure why we have to do this step but
# the internet says we should so we will.
close(link) 

# Now let's make X and Y variables that are easier to work with
# by extracting them from the "data" data frame object
X <- data$X
Y <- data$Y
n <- length(Y)

# Let's construct the residuals for a simple
# linear model, there are at least two ways to
# do this:
# 1) By hand!
b1 <- sum((X - mean(X))*(Y - mean(Y)))/(sum((X - mean(X))^2))
b0 <- mean(Y) - mean(X)*b1
e <- Y - b0 - b1*X
# 2) Using lm!
e <- lm(Y~X)$residuals

# First - plot the residuals against the lot size
plot(X, e, pch = 16, 
     xlab = "Lot Size", 
     ylab = "Residual", 
     xlim = c(0, 150),
     ylim = c(-100, 150))
abline(h = 0, lty = 3)

# Now make a sequence plot of the residuals!
plot(e, 
     type = "b", # This makes a plot made of dots with lines connecting them
     pch = 16, # This makes the points solid instead of empty circles, my personal preference
     ylim = c(-100, 150), # This lets us control the bounds of the y-axis by hand
     # We will make them match the ones in the book plot
     xlim = c(0, 30), # This lets us control the bounds of the x-axis by hand
     # We will make them match the ones in the book plot
     xlab = "Run", ylab = "Residual")

# Now make a box plot!
boxplot(e,
        horizontal = TRUE, # By default, R makes vertical boxplots
        # If we want a horizontal boxplot like the book,
        # 
        xlab = "Residual", 
        axes = FALSE # If we want to replicate the book plot we
        # don't want a y-axis
)
# We can make the x-axis by hand!
axis(1, at = seq(-70 - 35, 105 + 35, by = 35),
     labels = c("", seq(-70, 105, by = 35), ""))

# Now for a tricky one.
MSE <- sum(e^2)/(n - 2)
Ee <- sqrt(MSE)*qnorm((1:n - 0.375)/(n + 0.25), 
                       mean = 0, sd = 1)
plot(Ee, sort(e), axes = FALSE,
     xlim = c(-100, 100),
     ylim = c(-100, 150),
     pch = 16,
     xlab = "Expected",
     ylab = "Residual")
axis(1, at = seq(-150, 150, by = 50),
     labels = c("", seq(-100, 100, by = 50), ""))
axis(2, at = seq(-150, 200, by = 50),
     labels = c("", seq(-100, 150, by = 50), ""))
abline(a = 0, b = 1, lty = 3)
# Note - this does pretty much
# the same thing as qqnorm(e/sqrt(MSE)),
qqnorm(e/sqrt(MSE))
qqline(e/sqrt(MSE))

