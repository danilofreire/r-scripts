#####################################
### Bayesian Model Averaging in R ###
#####################################

# Install packages
install.packages(c("BMS", "BMA"))

# Load packages
library(BMS)
library(BMA)

# We'll start with the BMS package.

# Loda data. Here we'll reproduce the models estimated by
# Fernández, Ley and Steel (JAE 2001).
# http://bms.zeugner.eu/tutorials/fls/
data(datafls)
str(datafls) # dependent variable must be in the first column

# Estimate the model
mfls <- bms(datafls, burn = 100000, iter = 200000, 
		    g = "BRIC", mprior = "uniform", mcmc = "bd",
		    nmodel = 2000)

# One can also combine different chains
mfls1 <- bms(datafls, burn = 100000, iter = 200000, 
	 	     g = "BRIC", mprior = "uniform", mcmc = "rev.jump",
	 	     start.value = 0, nmodel = 2000)

mfsl.comb <- c(mfls, mfls1)
summary(mfsl.comb)
plot(mfsl.comb)

coef(mfsl.comb, exact 	 = TRUE)
coef(mfsl.comb, std.coef = TRUE,
	order.by.pip 		 = FALSE, 
	include.constant 	 = TRUE) # show standardised coefficients

# Density plots
density(mfsl.comb, "GDP60")
density(mfsl.comb, "Muslim", addons = "mlzpEebl")

# Inform quantiles
quantile(density(mfsl.comb, "GDP60"), c(0.025, 0.975))

# Plot model size
plotModelsize(mfsl.comb)

# Plot the differences between MCMC frequencies and likelihoods
plotConv(mfsl.comb)

# Correlation between MCMC frequencies and likelihoods
cor(pmp.bma(mfsl.comb))

# Plot the first 50 best models. The best are in the left-hand side
image(mfsl.comb[1:50]) # the best has 10 predictors, the 2nd best has 9 (removed Protestants)

# Check which models perform best
topmodels.bma(mfsl.comb)[, 1:3]

# Predictive densities
mfsl.pred <- bms(datafls[1:70, ],  mprior = "uniform",
				 burn = 100000, iter = 200000, mcmc = "rev.jump")

pdens <- pred.density(mfsl.pred, newdata = datafls[71:72, ])
plot(pdens) # Zambia and Zimbabwe
quantile(pdens, c(0.05, 0.95))
pdens$fit - datafls[71:72,1]. # forecast error
plot(pdens, "ZM", realized.y = datafls["ZM", 1]) 

# Now let's take a look at BMA

# Load data
library(Zelig)
data(turnout)

# Create variable age squared
turnout$age.squared <- turnout$age^2

# Formula
f <- formula(vote ~ income + educate + race +  age + age.squared)

# Estimation
m1 <- bic.glm(f, glm.family = "binomial", OR = 20, strict = FALSE,
			  data = turnout)

# Results
summary(m1)
plot(m1)
imageplot.bma(m1)
