#################################
### Rescaling a variable in R ###
#################################

# Suppose we have a variable X. We want to rescale that variable
# to have mean equal to 30 and standard deviation equal to 10.
# How can we do it?

# The answer can be found with very simple calculations.
# First, we create a fake variable
set.seed(123456)
x <- rnorm(100, 20, 5)
summary(x)
sd(x)

# Then, we standardise x
x.stand <- (x - mean(x)) / sd(x) 
summary(x.stand)
sd(x.stand)

# And finally we use Y=a+bx to convert the scores directly to what is required
x.final <- 30 + 10*x.stand
summary(x.final)
sd(x.final)

# Comparing the distributions
plot(density(x))
plot(density(x.final))
