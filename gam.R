########################################
### Generalised Additive Models in R ###
########################################

# In their widely-cited paper, Jackman and Beck (1998) argue that
# there's no reason for political scientists to limit themselves to
# linear models. Generalised additive models (GAMs for short) 
# allow us to capture nonlinearities and can be easily understood
# with plots. 

# We'll estimate two models, one with a continuous dependent variable
# and another one with a dichotomous measure.

# First, we load the necessary packages
library(Zelig) 	# data set
library(mgcv) 	# models
library(rjags)  # Bayesian models

# Data 
data(macro)
str(macro)

# Model -- continuous dependent variable.
m1 <- gam(unem ~ s(trade) + capmob + gdp, data = macro) # s() = smooth
summary(m1)
gam.check(m1)

plot(m1, residuals = TRUE, pch = 19, all.terms = TRUE)

# One can also model random effects
m2 <- gam(unem ~ s(trade) + capmob + gdp + 
	  s(country, bs = "re"), # bs = "re" = random effects
	  data   = macro, 
	  method = "REML")
summary(m2)

plot(m2, residuals = TRUE, pch = 19, all.terms = TRUE)

# The package can be used with JAGS, too!  
jags.ready <- jagam(unem ~ s(trade) + capmob + gdp,
	            data = macro, 
       		    file = "test.jags") 	# save the file - required

readLines("/home/sussa/Desktop/test.jags")  # inspect the model

jm <-jags.model("test.jags",
		data 	 = jags.ready$jags.data,
		inits 	 = jags.ready$jags.ini,
		n.chains = 4)
list.samplers(jm)

sam <- jags.samples(jm, c("b","rho","scale"),
		    n.iter 	= 10000,
		    thin	= 10)

jam <- sim2jam(sam, jags.ready$pregam)

print(jam)
plot(jam, pages = 1)

# Logistic model
data(turnout)
str(turnout)

# Model 3. Here we run a separate estimation of age by race.
m3 <- gam(vote ~ s(income) +  educate + s(age, by = race),
	  data 	 = turnout, 
	  family = "binomial") 
summary(m3)

plot(m3, all.terms = TRUE, trans = plogis) # logistic probabilities
vis.gam(m3, theta = -35, color = "heat", view = c("age", "income"))
vis.gam(m3, theta = -35, color = "heat", cond = list(race = "others"))

# Interactions:
m4 <- gam(vote ~ te(income, educate) + # te = interaction
	  s(age, by = race),           # gams by race
	  data   = turnout, 
	  family = "binomial") 
summary(m4)

plot(m4, all.terms = TRUE, scheme = 1, trans = plogis) # try other schemes
vis.gam(m4, theta = -35, color = "heat", view = c("educate", "income"))

# Logistic model with interaction in JAGS.
jags.ready2 <- jagam(vote ~ te(income, educate) + # te = interaction
		     s(age, by = race),           # gams by race
		     data   = turnout, 
		     family = "binomial", 
		     file   = "test2.jags")  	# save the file - required

readLines("/home/sussa/Desktop/test2.jags")  # inspect the model
 
jm2 <-jags.model("test2.jags",
	  	 data 	  = jags.ready2$jags.data,
		 inits 	  = jags.ready2$jags.ini,
		 n.chains = 4)
list.samplers(jm2)

sam2 <- jags.samples(jm2, c("b","rho"),
		     n.iter = 10000,
		     thin   = 10) # it takes a long time...

jam2 <- sim2jam(sam2, jags.ready2$pregam)

print(jam2)
plot(jam2, scheme = 2, pages = 1, trans = plogis) 
