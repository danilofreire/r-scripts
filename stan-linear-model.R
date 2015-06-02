#########################################
### (Robust) Linear Models with RStan ###
#########################################

# If you don't have Stan for R in your computer, uncomment and run the following lines:
# source('http://mc-stan.org/rstan/install.R', echo = TRUE, max.deparse.length = 2000)
# install_rstan()

# Load necessary packages
library(rstan)  # Stan interface for R

# Data set
library(Zelig)
data(macro)
str(macro)

# Model

code <- '
data {
    int<lower=0> N;              # number of observations
    vector[N] gdp;               # dependent variable - gdp in US$1000
    vector[N] unem;              # independent variable - unemployment
    vector[N] capmob;            # independent variable - capital mobility
    vector[N] trade;             # independent variable - trade (gdp %)
}
parameters {
    real alpha;                  # intercept
    real coef_unem;              # coefficient for unemployment
    real coef_capmob;            # coefficient for capital mobility
    real coef_trade;             # coefficient for trade
    real<lower=0> sigma;         # residual standard error
}
model {
    alpha ~ normal(0,100);       # weakly informative prior for the intercept
    coef_unem ~ normal(-2,1);    # informative prior 
    coef_capmob ~ normal(0,100); # weakly informative prior
    coef_trade ~ normal(.2, .1); # informative prior

    gdp ~ normal(alpha + coef_unem * unem + coef_capmob * capmob + coef_trade * trade, sigma); # model
}
'
# If you want to estimate a robust regression, say, a t-distribution with 4 degrees of freedom,
# just change the sampling distribution to:
# gdp ~ student_t(4, alpha + coef_unem * unem + coef_capmob * capmob + coef_trade * trade, sigma);

# Prepare the data list
data.list <- list(N = nrow(macro), gdp = macro$gdp, unem = macro$unem,
                  capmob = macro$capmob, trade = macro$trade)
str(data.list)

# Estimate the model
model <- stan(model_code = code, data = data.list, iter = 1000, chains = 4)
print(model, digits = 2)

# You can also compile the model only once then pass it to the sampling() command
# It is useful if you want to pass the same model to different data sets
# http://stackoverflow.com/questions/28077161/how-to-use-a-distinct-data-set-per-chain-in-stan
model1  <- stan_model(model_code = code)
m1 <- sampling(object = model1, data = data.list, chains = 2, iter = 2000)
m2 <- sampling(object = model1, data = data.list, chains = 2, iter = 2000)
f1 <- sflist2stanfit(list(m1, m2))
print(f1, digits = 2)

# One can also calculate the chains using parallelising packages and functions
model1 <- stan(model_code = code, data = data.list, chains = 0) # only to translate the model to C++

# Parallel computing, running 4 chains in a 4-cores PC
library(parallel) 
posterior <- mclapply(1:4, mc.cores = 4, 
                      function(i) stan(fit = model1, data = data.list, 
                                       seed = 12345, 
                                       chains = 1, chain_id = i, 
                                       refresh = -1))

model1 <- sflist2stanfit(posterior) # collect the results back into a single Stan fit object
print(model1)


# Extract marginal distributions
marg.dist <- apply(model1, MARGIN = "parameters", FUN = quantile, probs = (1:100) / 100)
head(marg.dist)

# Compare with frequentist estimation
summary(lm(gdp ~ unem + capmob + trade, macro))

# Extract Stan object
f1 <- extract(model1)
str(f1)
plot(density(f1$coef_capmob), main = "Posterior -- Capital Mobility")  # draw posterior
quantile(f1$coef_capmob, c(.025, .975))                                # 95% interval

# Load coda and ggmcmc to plot graphs
library(coda)
library(ggmcmc)

# Plotting some graphs and assessing convergence with the coda package.
# We can convert a Stan fit object to coda with the following function
# (http://jeromyanglim.tumblr.com/post/91434443911/how-to-convert-a-stan-fit-object-to-work-with-coda-and)

stan2coda <- function(model1) {
        mcmc.list(lapply(1:ncol(model1), function(x) mcmc(as.array(model1)[,x,])))
}

fit.mcmc <- stan2coda(model1)

# I personally like the codamenu() function. It has all tests and plots one may want.
# Just type:
codamenu()
2 # Use an mcmc object
fit.mcmc

# And follow the menu instructions. A Stan fit object can also be transformed into
# a ggmcmc object with ggs(). We can also change the parameters' labels.
P <- data.frame(Parameter = c("alpha", "coef_unem", "coef_capmob", "coef_trade", "sigma"),
                Label = c("Intercept", "Unemployment", "Capital Mobility", "Trade", "Residual Standard Error"))
fit.ggmcmc <- ggs(fit, par_labels = P)

# Some plots
ggs_traceplot(fit.ggmcmc) + ggtitle("Trace Plots") + theme_bw()
ggs_density(fit.ggmcmc) + ggtitle("GDP per capita (US$ 1000)") + 
        xlab("Estimate") + ylab("Density") + theme_bw()
ggs_caterpillar(fit.ggmcmc) + ggtitle("Coefficient Plot") +
        xlab("HPD") + ylab("Parameter") + theme_bw()
