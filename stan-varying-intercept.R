##########################################
### Varying-Intercept Models with Stan ###
##########################################

# Stan is a modeling language for full Bayesian inference. It uses Hamiltonian 
# Monte Carlo to overcome some issues of Gibbs sampling and is very
# flexible. You can fit any kind of model using its basic framework. 
# The manual has several examples: http://mc-stan.org/manual.html

# Here I present a simple multilevel linear model with varying intercepts.
# Since I intended this script to be used by beginners, it probably has
# more comments than necessary, but I guess it's more instructive like this.

# Let's get started!

# How can we run a multilevel/hierarchical model in Stan?

# If you don't have Stan for R in your computer, uncomment and run the following lines:
# source('http://mc-stan.org/rstan/install.R', echo = TRUE, max.deparse.length = 2000)
# install_rstan()

# Load necessary packages
library(rstan)    # Stan interface for R
library(Zelig)    # We'll use a data set from this package
library(parallel) # Parallel computing
library(coda)     # Graphs and tests
library(ggmcmc)   # Graphs
library(lme4)     # Multilevel models (for comparison)


# Data set
data(macro)
str(macro)

# We have to create a list with the variables used in the model, transforming 
# a factor variable in integers
data.list <- list(N = nrow(macro), gdp = (macro$gdp),
                  trade = macro$trade, 
                  capmob = macro$capmob,
                  unem = macro$unem, 
                  country = as.numeric(macro$country))
str(data.list)

# Check what are the factors in the country variable
summary(macro$country)

# First I want to estimate the following model, with the intercept varying by group:
# GDP_i = alpha_ij + b1*trade_i + b2*unemployment_i + b3*capital mobility_i + error_ij
# Where J = 14 countries. Let's get it running!

# Model code
code <- '
data {                          
  int<lower=0> N;                        # number of observations
  int<lower=1, upper=14> country[N];     # number of countries (J = 14)
  vector[N] gdp;                         # dependent variable
  vector[N] trade;                       # independent variable 1
  vector[N] unem;                        # independent variable 2
  vector[N] capmob;                      # independent variable 3
}
parameters {
  vector[14] alpha;                      # intercept estimated with 14 countries       
  real<lower=0,upper=100> sigma_alpha;   # standard error for the intercept
  real<lower=0,upper=100> sigma_gdp;     # standard error for gdp
  real mu_alpha;                         # mean for intercepts
  real b1_trade;                         # betas 1, 2, and 3
  real b2_unem;
  real b3_capmob;
}
transformed parameters {
  vector[N] gdp_hat;                     # create one variable with predictions for each observation
  for (i in 1:N)                         # loop for all cases
      gdp_hat[i] <- alpha[country[i]] + b1_trade * trade[i] + b2_unem * unem[i] + b3_capmob * capmob[i];
}
model {
  b1_trade ~ normal(0,100);              # priors
  b2_unem ~ normal(0,100);
  b3_capmob ~ normal(0,100);
  mu_alpha ~ normal(0, 100);
  alpha ~ normal(mu_alpha, sigma_alpha);
  gdp ~ normal(gdp_hat, sigma_gdp);      # model
}
'

# Translate the model into C++
model1 <- stan(model_code = code, data = data.list, iter = 1, chains = 1)

# Running 4 parallel chains in a 4-cores PC
posterior <- mclapply(1:4, mc.cores = 4, 
                      function(i) stan(fit = model1, data = data.list,
                                       seed = 12345, chains = 1,
                                       chain_id = i, refresh = -1))

model1 <- sflist2stanfit(posterior) # collect the results back into a single Stan fit object

# Summarise the model, plot some graphs and assess convergence with the coda package.
# We can convert a Stan fit object to coda with the following function
# (http://jeromyanglim.tumblr.com/post/91434443911/how-to-convert-a-stan-fit-object-to-work-with-coda-and)
stan2coda <- function(model1) {
        mcmc.list(lapply(1:ncol(model1), function(x) mcmc(as.array(model1)[,x,])))
}

fit.mcmc <- stan2coda(model1)

# Subsetting the Stan object to have only the coefficients for the 14 countries plus
# sigma_alpha, sigma_gdp, mu_alpha and the 3 betas
fit.mcmc <- fit.mcmc[,1:20]
summary(fit.mcmc)

# Compare the results with lmer estimations
model1.lmer <- lmer(gdp ~ trade + unem + capmob + (1 | country), data = macro)
summary(model1.lmer)
coef(model1.lmer)

# I personally like the codamenu() function for tests. Just type:
codamenu()
2 # Use an mcmc object
fit.mcmc

# Using ggmcmc() to plot graphs

# Random effects
random.fx <- fit.mcmc[,1:14] # selecting only the country intercepts

P <- data.frame(Parameter = c("alpha[1]", "alpha[2]", "alpha[3]", "alpha[4]",
                              "alpha[5]", "alpha[6]", "alpha[7]", "alpha[8]",
                              "alpha[9]", "alpha[10]", "alpha[11]", "alpha[12]", 
                              "alpha[13]", "alpha[14]"),
                Label = c("AT", "BE", "CA", "DK",
                          "FI", "FR", "IT", "JP",
                          "NE", "NO", "SE", "UK",
                          "US", "DE"))
                          
random.fx1 <- ggs(random.fx, par_labels = P)

# Some plots
ggs_traceplot(random.fx1) + ggtitle("Trace Plots") + theme_bw() +
        facet_wrap(~ Parameter, ncol = 2)

ggs_density(random.fx1) + ggtitle("Random Effects -- Intercepts") + 
        xlab("Intercepts") + ylab("Density") + xlim(0,10) +
        theme_bw() + facet_wrap(~ Parameter, ncol = 2)

ggs_caterpillar(random.fx1) + ggtitle("Random Effects per Country") +
        xlab("Intercept Coefficients") + ylab("") + xlim(0,10) +
        theme_bw()

# Fixed effects
fixed.fx <- fit.mcmc[,c(17:20, 15, 16)] # reordering the columns

P2 <- data.frame(Parameter = c("mu_alpha", "b1_trade", "b2_unem",
                               "b3_capmob", "sigma_alpha", "sigma_gdp"),
                Label = c("Intercept Mean", "Trade", "Unemployment",
                          "Capital Mobility", "SD Intercept", "SD Model"))
fixed.fx1 <- ggs(fixed.fx, par_labels = P2)

# More plots
ggs_traceplot(fixed.fx1) + ggtitle("Trace Plots") + theme_bw() +
        facet_wrap(~ Parameter, ncol = 2)

ggs_density(fixed.fx1) + ggtitle("Fixed Effects") + 
        xlab("Coefficients") + ylab("Density") + xlim(-2,10) +
        theme_bw()

ggs_caterpillar(fixed.fx1) + ggtitle("Fixed Effects") +
        xlab("Coefficients") + ylab("") + xlim(-2,10) +
        theme_bw()

