# Robust standard errors for linear models
# Create the following function:

cluster   <- function(data, model, cluster){
           require(sandwich, quietly = TRUE)
           require(lmtest, quietly = TRUE)
           M <- length(unique(cluster))
           N <- length(cluster)
           K <- model$rank
           dfc <- (M/(M-1))*((N-1)/(N-K))
           uj  <- apply(estfun(model),2, function(x) tapply(x, cluster, sum));
           vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
           coeftest(model, vcovCL) }


# After running the code above, you can run your regression with clustered standard errors as follows:

# Run a plain linear regression
regt = lm(nevermar ~ impdum, data = nmar)
 
# apply the 'cl' function by choosing a variable to cluster on.
# here, we are clustering on state.
cluster (nmar, regt, nmar$state)

#######################

# Robust SE for logistic regression:

# http://stackoverflow.com/questions/16498849/logistic-regression-with-robust-clustered-standard-errors-in-r

# You might want to look at the rms (regression modelling strategies) package. 
# So, 'lrm' is logistic regression model, and if 'fit' is the name of your output, you'd have something like this:

library(rms)

fit=lrm(disease ~ age + study + rcs(bmi,3), x=T, y=T, data=dataf)

fit

robcov(fit, cluster=dataf$id) # this is very similar to Stata's logit Y X1 X2, vce(cluster X)" syntax

bootcov(fit,cluster=dataf$id)

# You have to specify x=T, y=T in the model statement. rcs indicates restricted cubic splines with 3 knots.
