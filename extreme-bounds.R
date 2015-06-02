###############################
### Extreme Bounds Analysis ###
###############################

# It is very easy to use Extreme Bounds Analysis in R.
# EBA allows the research to assess the sensitivy and robustness
# of a given series of results. It has more or less the same
# function of Bayesian Model Averaging.

# The "ExtremeBounds" package is extremely flexible and easy to use.

# Let's install it
install.packages("ExtremeBounds")

# Load the package
library(ExtremeBounds)

# First data set
data(mtcars)

# As I've just said, EBA is very flexible. It can estimate several 
# models, allows the inclusion of interactions and non-linearities,
# and enables the researcher to separate the variables he wants to 
# test from "permanent" control variables in the model.
# The syntax is y ~ free | focus | doubtful. Free variables
# are included in all models, focus are the variables of interest,
# and doubtful are possible confounders. If nothing is specified,
# the package will assume that all variables belong to "focus".

# Let's estimate our first model. All variables are "focus".
 naive.eba <- eba(formula = mpg ~ cyl + carb + disp + hp + vs + drat + wt
 					        + qsec + gear + am, data = mtcars, k = 0:9)

summary(naive.eba)
hist(naive.eba)

# A more sophisticated EBA. Now, wt is a free variable, some variables
# that may be collinear will be in the "exclusive" part, only three
# variables of interest will be estimated at a time, and the model
# will use robust standard errors. Finally, rore weight will be given
# to models that give better estimations ("lri").

# Standard errors:
library("sandwich")

se.robust <- function(model.object) {
model.fit <- vcovHC(model.object, type = "HC")
out <- sqrt(diag(model.fit))
return(out)
}

# Estimation
sophisticated.eba <- eba(formula = mpg ~ wt | cyl + carb + disp + hp |
						vs + drat + wt + qsec + gear + am,
						data = mtcars,
						exclusive = ~ cyl + carb + disp + hp | am + gear,
						vif = 7, se.fun = se.robust, weights = "lri")

summary(sophisticated.eba)
hist(sophisticated.eba, variables = c("cyl", "carb", "disp", "hp"),
     main = c(cyl = "number of cylinders", carb = "number of carburetors",
	   disp = "engine displacement", hp = "gross horsepower"),
	   normal.show = TRUE)
