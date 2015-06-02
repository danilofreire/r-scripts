###############################
### Robit Models with RStan ###
###############################

# Here we'll estimate a robust logistic model. Firstly, I would like to thank
# all Stack Overflow users who contributed and provided code for this script.
# Please take a look at the link below for more information: 
# http://stackoverflow.com/questions/26447512/how-to-run-a-robit-model-in-stan
# For an introduction to robit models, please refer to: 
# http://www.stat.purdue.edu/~chuanhai/teaching/Stat598A/robit.pdf

# Load necessary packages
library(rstan)  # Stan interface for R
library(Zelig)  # Data

# Load data set
data(turnout)

df <- list(N = nrow(turnout), vote = turnout$vote, 
           educate = turnout$educate, income = turnout$income,
           age = turnout$age, agesq = turnout$age^2,
           nu = 4)                 # adjust nu as desired degree of freedom

# Model code
mod_string <- "
data {                          
    int<lower=0> N;                # number of observations
    int<lower=0,upper=1> vote[N];  # setting the dependent variable (vote) as binary
    vector[N] educate;             # independent variable 1
    vector[N] income;              # independent variable 2
    vector[N] age;                 # independent variable 3
    vector[N] agesq;               # independent variable 4
    real nu;                       # degrees of freedom
}
parameters {
    real alpha;                    # intercept
    real b_educate;                # betas for educate, etc
    real b_income; 
    real b_age;
    real b_agesq; 
}
model {
    vector[N] pi;

    for(i in 1:N){
      pi[i] <- student_t_cdf(alpha + b_educate * educate[i] + b_income * income[i] + b_age * age[i] + b_agesq * agesq[i], nu, 0, 1);
      vote[i] ~ bernoulli(pi[i]);
    }

    alpha ~ normal(0,100);         # you can set priors for all betas
    b_educate ~ normal(0,100);     # if you prefer not to, uniform priors will be used
    b_income ~ normal(0,100);
    b_age ~ normal(0,100);
    b_agesq ~ normal(0,100);
}
"
# Run the model
fit1 <- stan(model_code = mod_string, data = df, chains = 4, iter = 1000)
print(fit1)
