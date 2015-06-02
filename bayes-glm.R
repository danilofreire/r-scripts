#
#
########## Bayesian Regression / Generalized Linear Models ##########
#
#

# Generate some "simulated" data for the examples below. 

n <- 500
int <- -5
open <- rnorm(n, 50, 7)
agree <- rnorm(n, 100, 15)
social <- rnorm(n, 25, 4)
e <- rnorm(n, 0, 1)
extro <- int + (1.2 * open) + (.9 * agree) + (.4 * social) + e
data1 <- data.frame(extro, open, agree, social)
summary(data1)
cor(data1)
rm(n, int, open, agree, social, e, extro)
ls()

# Confirm / take a look at the core Linear Model (lm) [NOTE: using the optional 
# arguments "x = TRUE" and "y = TRUE" allows us to access the design matrix "x" 
# and the response vector "y"; both of which are used in the Bayesian Linear 
# Regression below].

model.1 <- lm(extro ~ open + agree + social, data = data1)
summary(model.1)

# Same as above; using the Generalized Linear Model (glm) function, specifying the default 
# Gaussian (normal) family distribution. 

model.2 <- glm(extro ~ open + agree + social, data = data1, family = "gaussian")
summary(model.2)

# One of the benefits of the Bayesian perspective (for any analysis) is that it allows us to make 'credible 
# interval' statements. Credible intervals are similar to confidence intervals, but in the Bayesian 
# framework, the interval REALLY IS believed to contain the true population parameter. 
#      For instance: a 95% credible interval for a parameter being estimated, from the Bayesian perspective, 
#      is interpreted as; there is a 95% probability that the actual parameter is included in that interval. 
# This is because the interval is constructed based on information from the posterior distribution; of for instance, 
# one of the predictor's coefficient posterior distribution (e.g. the open variable's coeffient posterior distribution). 

### Bayesian Linear Regression.

# Load the package 'LearnBayes' (Albert, 2010). 

library(LearnBayes)

# Conduct the Bayesian Linear Regression using the 'blinreg' function; which returns $beta "a matrix 
# of simulated draws from the marginal posterior of Beta, where each row is a simulated draw", and 
# $sigma, which "is a vector of simulated draws from the marginal posterior of Sigma". 

joint.posterior.samples <- blinreg(model.1$y, model.1$x, 5000)

oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
hist(joint.posterior.samples$beta[,1], main = "Intercept", xlab = "beta 0")
hist(joint.posterior.samples$beta[,2], main = "Open Predictor", xlab = "beta 1")
hist(joint.posterior.samples$beta[,3], main = "Agree Predictor", xlab = "beta 2")
hist(joint.posterior.samples$beta[,4], main = "Social Predictor", xlab = "beta 3")
par(oldpar)

# To display the 95% credible intervals (and medians) from the distributions, use an 'apply' function. 

apply(joint.posterior.samples$beta, 2, quantile, c(.025, .500, .975))

### Bayesian Generalized Linear Modeling. 

# Load the required package: 'arm' (Gelman, et al., 2010).

library(arm)

# Conduct the Bayesian Generalized linear model (here family = Gaussian, as is default).

model.3 <- bayesglm(extro ~ open + agree + social, family = "gaussian", data = data1, 
                     prior.scale=Inf, prior.df=Inf)
display(model.3, detail = TRUE)
coefplot(model.3)

# The 'bayesglm' function represents a kind of short cut of the Bayesian approach to inference. Typically, 
# the posterior is not used directly for making inferences. Instead, an empirical distribution is constructed 
# based on draws from the posterior and that empirical distribution is what informs the inference(s). Here, we 
# are using the 'bayesglm' as a proxy for doing the added empirical distribution. With the 'bayesglm' we 
# get a distribution of 'simulates' which are used in place of an actual empirical distribution (which will 
# be covered further below). 

# Retrieve the posterior distributions of the coefficients for the intercept and all three predictors. 

simulates <- coef(sim(model.3, n.sims=1000))
head(simulates)

# Extract just the posterior distribution of the 'open' variable's coefficient. 

posterior.open <- simulates[,2]
head(posterior.open)

# Take a look at the posterior distribution of the open variable's coefficient (normally a histogram 
# would not be used, it is used here simply as a graphical reference).

hist(posterior.open)
plot(density(posterior.open), main = "", xlab = "Posterior.open", ylab = "Density")

# Retrieve the 95% credible interval for the open variable's coefficient.

quantile(posterior.open, c(.025, .975))

# To see the entire list of optional arguments (there are many) for the 'bayesglm' function:

help(bayesglm)

########################################################################

# Going further to actually creating an empirical distribution based on iterative draws from the 
# posterior. The 'MCMCregress' function in the package "MCMCpack" provides us with the Markov Chain
# Monte Carlo simulation method of creating the empirical distribution; which itself provides us 
# with the descriptive statistics used for inference. Meaning, the mode, median, or mean of the 
# empirical MCMC simulates' distribution is the 'maximum likelihood' (i.e. top of a density function)
# estimate of the population parameter. And, the credible interval which includes the actual population 
# parameter value. 

library(MCMCpack)

# Notice, the model formula is the same, but here we have some new options. The 'burnin' is used 
# because MCMC iterates are sensitive to their initial start values, so the first few (i.e. 3000) 
# iterations are discarded. The 'mcmc' simply issues how many (post-burnin) iterations will be 
# used to build the empirical distribution. The 'thin' defaults to 1 and represents a control 
# on convergence, such that once approximate convergence has been reached it can be beneficial 
# to keep only a few simulates and discard the rest to conserve computer resources (Gelman, Carlin, 
# Stern, & Rubin, 2004). The verbose option (by default is off) simply does or does not print 
# the iteration history as the function runs. The seed argument simply allows the user to set 
# the random number generator seed. The 'beta.start' argument allows the user to set a start 
# value for the beta vector. 

model.4 <- MCMCregress(extro ~ open + agree + social, data = data1, burnin = 3000, mcmc = 10000,
                     thin = 1, verbose = 0, seed = NA, beta.start = NA)
summary(model.4)

# Notice in the summary, we get the coefficient estimates and credible intervals (in the second 
# part of the output, labeled "Quantiles for each variable:"). 


# To see all the optional arguments and their explanations in MCMCregress; 

help(MCMCregress)

### References / Resources ###

# Albert, J. (2007). Bayesian Computation with R. New York: Springer Science+Business Media, LLC.

# Albert, J. (2010). Package 'LearnBayes'. Available at CRAN: 
# http://cran.r-project.org/web/packages/LearnBayes/index.html

# Berry, D. A. (1996). Statistics: A Bayesian perspective. Belmont, CA: Wadsworth Publishing Company. 

# Bolker, B. M. (2008). Ecological Models and Data in R. Princeton, NJ: Princeton University Press. 

# Bolstad, W. M. (2004). Introduction to Bayesian statistics. Hoboken, NJ: John Wiley & Sons, Inc.

# Gelman, A., Carlin, J. B., Stern, H. S., & Rubin, D. B. (2004). Bayesian Data Analysis (2nd ed.). 
# Boca Raton, FL: Chapman & Hall/CRC.

# Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y. (2009).  A Weakly Informative Default Prior 
# Distribution For Logistic And Other Regression Models. The Annals of Applied Statistics, 2(4), 
# 1360-1383.  

# Gelman, A., Su, Y., Yajima, M., Hill, J., Pittau, M. G., Kerman, J., & Zheng, T. (2010). Package 'arm'. 
# Available at: 
# http://cran.r-project.org/web/packages/arm

# Hoff, P. D. (2009). A First Course in Bayesian Statistical Methods. New York: Springer Science+Business 
# Media, LLC.

# Martin, A. D., Quinn, K. M., & Park, J. H. (2010). Package 'MCMCpack'. Available at:
# http://cran.r-project.org/web/packages/MCMCpack/MCMCpack.pdf



# END: Updated Mar. 8, 2011 (added 'blinreg' function and associated operations. 
