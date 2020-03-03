rm(list = ls())

# Let's practice making some plots!
# We're going to replicate Figure 3.1 in the
# .pdf version of the text

# We can load our data files directly from
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

# First - make a dot plot.
# This just shows the number of observations per
# X value
# People don't really make dotplots anymore - 
# what you're probably more familiar with is a bar chart
barplot(table(X),
        xlab = "Lot Size", # Adds a label to the x-axis
        ylab = "Number of Units" # Adds a label to the y-axis
        )
# Now make a sequence plot!
plot(X, 
     type = "b", # This makes a plot made of dots with lines connecting them
     pch = 16, # This makes the points solid instead of empty circles, my personal preference
     ylim = c(0, 150), # This lets us control the bounds of the y-axis by hand
                       # We will make them match the ones in the book plot
     xlim = c(0, 30), # This lets us control the bounds of the x-axis by hand
                     # We will make them match the ones in the book plot
     xlab = "Run", ylab = "Lot Size")

# Make a stem and leaf plot
stem(X, scale = 3) 
# The scale determines the plot length, although to be honest
# I don't really understand how. I just picked different values
# of scale until I got something that looked like the book's
# plot
# I don't think R has a nice way to annotate the median,  
# 25%, and 75% quantiles (hinges)

# Now make a box plot!
boxplot(X,
        horizontal = TRUE, # By default, R makes vertical boxplots
                           # If we want a horizontal boxplot like the book,
                           # 
        xlab = "Lot Size", 
        axes = FALSE # If we want to replicate the book plot we
                     # don't want a y-axis
        )
# We can make the x-axis by hand!
axis(1, at = seq(0, 140, by = 20),
     labels = c("", seq(20, 120, by = 20), ""))

