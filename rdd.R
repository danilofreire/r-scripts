## Regression discontinuity designs in R

# RDDs are currently very popular in political science. 
# Their estimation are also quite simple using the rdd and RDDtools packages.
# In this simple tutorial I'll show how to use the packages's
# main functions. For an overview of the technique, check:
# www.alnap.org/pool/files/regression-discontinuity-full.pdf

# First, install and run the rdd package
install.packages("rdd")
library(rdd)

# Now, install RDDtools (via Github)
install.packages("devtools")
library(devtools)
install_github(repo = "RDDtools", username = "MatthieuStigler",
				       subdir = "RDDtools")
library(RDDtools)
data(Lee2008)

# Then we start with creating some fake data and perform the McCrary Test
# for potential endogenous sorting (rdd package)

# No discontinuity
x <- runif(1000,-1,1)
DCdensity(x, 0)

# Discontinuity
x <- runif(1000, -1, 1)
x <- x + 2*(runif(1000, -1, 1) > 0 & x < 0)
DCdensity(x, 0) # contrast the values

# Next, we use the Imbens-Kalyanaraman Optimal Bandwidth Calculation.
x <- runif(1000, -1, 1)
cov <- rnorm(1000)
y <- 3 + 2*x + 3*cov + 10*(x >= 0) + rnorm(1000)
IKbandwidth(x, y, cutpoint = 0, kernel = "triangular")

# Finally, we show how to use the package's main command, RDestimate.
x <- runif(1000, -1, 1)
cov <- rnorm(1000)
y <- 3 + 2*x + 3*cov + 10*(x >= 0) + rnorm(1000)
summary(est1 <- RDestimate(y ~ x))
plot(est1)

# Efficiency gains can be made by including covariates
summary(est2 <- RDestimate(y ~ x | cov))
plot(est2)

# Now it's time for us to test RDDtools. It seems more complete than
# rdd and has many interesting functions.

# Declare the data to be a RDDdata object:
Lee2008_rdd <- RDDdata(y = Lee2008$y, x = Lee2008$x, cutpoint = 0)
summary(Lee2008_rdd)
plot(Lee2008_rdd)

# First, parametric estimations with a 4th order polynomial:
summary(reg_para <- RDDreg_lm(RDDobject = Lee2008_rdd, order = 4))
plot(reg_para)

# Non-parametric: a simple local regression, using the Imbens and 
# Kalyanaraman 2012 bandwidth:
bw_ik <- RDDbw_IK(Lee2008_rdd)
summary(reg_nonpara <- RDDreg_np(RDDobject = Lee2008_rdd, bw = bw_ik))
plot(reg_nonpara)

# McCrary test
dens_test(reg_nonpara)

# One can easily check the sensitivity of the estimate to different bandwidths:
plotSensi(reg_nonpara, from = 0.05, to = 1, by = 0.1)

# Or run the Placebo test, estimating the RDD effect based on fake cutpoints:
plotPlacebo(reg_nonpara)

# Discontinuity comes from covariates: covariates balance tests
# Two tests available:
# equal means of covariates: covarTest_mean()
# equal density of covariates: covarTest_dens()
# We need here to simulate some data, given that the Lee (2008) dataset contains 
# no covariates. We here simulate three variables, with the second having a different
# mean on the left and the right.
set.seed(123)
n_Lee <- nrow(Lee2008)
Z <- data.frame(z1 = rnorm(n_Lee, sd = 2),
				        z2 = rnorm(n_Lee, mean = ifelse(Lee2008 < 0, 5, 8)),
    			      z3 = sample(letters, size = n_Lee, replace = TRUE))
Lee2008_rdd_Z <- RDDdata(y = Lee2008$y, x = Lee2008$x, covar = Z, cutpoint = 0)

covarTest_mean(Lee2008_rdd_Z, bw = 0.3)
covarTest_dis(Lee2008_rdd_Z, bw = 0.3)

# Tests correctly reject equality of the second, and correctly do not reject equality
# for the first and third.
