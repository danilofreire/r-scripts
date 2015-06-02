# It is very easy to calculate the inverse logit function, 
# which transform logit coefficients into probabilities.

# For instance, if we have the logistic equation:

# Pr(y) = 0.61 - 0.62x + e

# The intercept (0.61) can be interpreted as
# logit^-1 (.61) = .648
# Thus, the model estimate a probability of about 65% when X = 0.

# To calculate the function, just use the following command:

library(boot)
inv.logit(x)

# In this case,
inv.logit(.61) = 0.6479408

# If x = 3, the probability decreases to about 22%
inv.logit(.61 - (.62*3)) = 0.2227001
