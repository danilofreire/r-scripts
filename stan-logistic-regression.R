##################################
### Logistic Models with RStan ###
##################################


# Stan is a modeling language for full Bayesian inference. It uses Hamiltonian 
# Monte Carlo to overcome some issues of Gibbs sampling and is very flexible.
# The manual has several examples: http://mc-stan.org/manual.html

# Here I present a simple logistic model. Since I intended this script to be used by beginners,
# it probably has more comments than necessary, but I guess it's more instructive like this.

# Let's get started!

# How do we run a simple logistic model in Stan?

# If you don't have Stan for R in your computer, uncomment and run the following lines:
# source('http://mc-stan.org/rstan/install.R', echo = TRUE, max.deparse.length = 2000)
# install_rstan()

# Load necessary packages
library(rstan)  # Stan interface for R
library(Zelig)  # Simulations for frequentist inference, data

# Data set
data(turnout)
str(turnout)

# Now we fit a logistic model with several predictors. 
# The model I want to estimate is:
# Pr(vote = 1) = intercept + educate + income + age + age^2 + error, in which vote is a binary variable.
# The age^2 variable is not in the original data set, so it will be created by Stan.
# I assume the same weakly informative prior for the intercept and the coefficients.
# I also want to simulate the probability of voting for an individual with the following characteristics:
# educate = 10, income = 15, age = 40 and age^2 = 1600. This is done with the 'generated quantities' block.
# Don't forget the ' at the beginning and end of the function

m1 <- '
data {                          
int<lower=0> N;                # number of observations
int<lower=0,upper=1> vote[N];  # setting the dependent variable (vote) as binary
vector[N] educate;             # independent variable 1
vector[N] income;              # independent variable 2
vector[N] age;                 # independent variable 3
}
transformed data {
vector[N] age_sq;              # create new variable (4), age squared (no dots in the variable name)
age_sq <- age .* age;          # formula for the variable, do not forget the . before multiplication
}
parameters {
real alpha;                    # intercept
real b_educate;                # beta for educate, etc
real b_income; 
real b_age;
real b_age_sq; 
}
model {
alpha ~ normal(0,100);         # you can set priors for all betas
b_educate ~ normal(0,100);     # if you prefer not to, uniform priors will be used
b_income ~ normal(0,100);
b_age ~ normal(0,100);
b_age_sq ~ normal(0,100);
vote ~ bernoulli_logit(alpha + b_educate * educate + b_income * income + b_age * age + b_age_sq * age_sq); # model
}
generated quantities {         # simulate quantities of interest
real y_hat;                    # create a new variable for the predicted values
y_hat <- inv_logit(alpha + b_educate * 10 + b_income * 15 + b_age * 40 + b_age_sq * 1600); # model
}
'

# Create a list with the chosen variables
data.list <- list(N = nrow(turnout), vote = turnout$vote, educate = turnout$educate,
                   income = turnout$income, age = turnout$age)
str(data.list)

# Estimate the model
fit <- stan(model_code = m1, data = data.list, iter = 1000, chains = 4)
print(fit, digits = 3)

# Compare with frequentist estimation (with Zelig)
z1 <- zelig(vote ~ educate + income + age + I(age^2), model = "logit", data = turnout)
summary(z1)
s1 <- setx(z1, educate = 10, income = 15, age = 40)
print(sim(z1, x = s1))

# Using coda and ggmcmc to plot graphs
library(coda)
library(ggmcmc)

# Plotting some graphs and assessing convergence with the coda package.
# We can convert a fit object to coda with the following function
# (http://jeromyanglim.tumblr.com/post/91434443911/how-to-convert-a-stan-fit-object-to-work-with-coda-and)
stan2coda <- function(fit) {
        mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[,x,])))
}

fit.mcmc <- stan2coda(fit)

# I personally like the codamenu() function. It has all tests and plots one may
# want. Just type:
codamenu()
2 # Use an mcmc object
fit.mcmc

# And follow the menu instructions. A Stan fit object can also be transformed into a ggmcmc
# object with ggs(). We can also change the parameters' labels.
# First, we'll remove the y_hat and lp__ columns
fit.mcmc <- fit.mcmc[,1:5]

# Now add labels
P <- data.frame(Parameter = c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]"),
                Label = c("Intercept", "Educate", "Income", "Age", "Age Squared"))
fit.ggmcmc <- ggs(fit.mcmc, par_labels = P)

# Some plots
ggs_traceplot(fit.ggmcmc) + ggtitle("Trace Plots") + theme_bw()
ggs_density(fit.ggmcmc) + ggtitle("Logistic Estimations for Voter Turnout") + 
        xlab("Estimate") + ylab("Density") + theme_bw()
ggs_caterpillar(fit.ggmcmc) + ggtitle("Coefficient Plot") +
        xlab("HPD") + ylab("Parameter") + theme_bw()

# You may also check Stan's Github repository, it has many examples:
# https://github.com/stan-dev/example-models
# Several other ggmcmc() options here: http://xavier-fim.net/packages/ggmcmc/

# Lastly, if we want to estimate a full marginal distribution for a given
# predictor in the model, we employ the workhorse apply() function as follows:
# (from: http://bit.ly/1y0OMyC)
margins <- apply(as.matrix(fit), MARGIN = 2, FUN = quantile, probs = (1:100) / 100)
head(margins, 10)

# Plot the marginal distribution of educate (2nd column)
par(mfrow=c(2,2))
plot(jitter(margins[,2]), pch=20, xlab = "Education Years - Marginal Distribution (%)", 
     ylab = "Probability of Voting", main = "Predicted Values", axes=FALSE)
axis(1) # adds x axis
axis(2) # adds y axis
plot(jitter(margins[,3]), pch=20, xlab = "Income in US$1000 - Marginal Distribution (%)", 
     ylab = "Probability of Voting", main = "Predicted Values", axes=FALSE)
axis(1) 
axis(2) 
plot(jitter(margins[,4]), pch=20, xlab = "Age - Marginal Distribution (%)", 
     ylab = "Probability of Voting", main = "Predicted Values", axes=FALSE)
axis(1) 
axis(2) 
plot(jitter(margins[,5]), pch=20, xlab = "Age Squared - Marginal Distribution (%)", 
     ylab = "Probability of Voting", main = "Predicted Values", axes=FALSE)
axis(1)
axis(2) 
