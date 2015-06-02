# Scatterplots with boxplots

# Load necessary package
library(car)

# Create fake data:
x <- rnorm(100,10,5)
e <- rnorm(100,20,20)
y <- 1 + 2*(x^2) + e

# Make a scatterplot
scatterplot(x,y, smoother=loessLine,
            xlab = "x variable", ylab = "y variable")
