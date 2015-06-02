############################################
### Estimating Randomised Responses in R ###
############################################

# As the package description states:
# "rr implements methods developed by Blair, Imai, and Zhou (2015) such as
# multivariate regression and power analysis for the randomized response 
# technique. The current version of this package conducts multivariate 
# regression analyses for the sensitive item under four standard randomized
# response designs: mirrored question, forced response, disguised response, 
# and unrelated question. Second, it generates predicted probabilities of 
# answering affirmatively to the sensitive item for each respondent. Third, 
# it also allows users to use the sensitive item as a predictor in an outcome
# regression under the forced response design. Additionally, it implements power
# analyses to help improve research design."
# More info: http://imai.princeton.edu/research/files/randresp.pdf

# Load the package
library(rr)

# Load the data set
data(nigeria)

# rrreg() is used to conduct multivariate regression analyses of survey data
# using randomized response methods.
set.seed(1)

# Define design parameters
p  <- 2/3  # probability of answering honestly in Forced Response Design
p1 <- 1/6  # probability of forced 'yes'
p0 <- 1/6  # probability of forced 'no'

# Fit linear regression on the randomized response item of whether
# citizen respondents had direct social contacts to armed groups
rr.q1.reg.obj <- rrreg(rr.q1 ~ cov.asset.index + cov.married + I(cov.age/10) +
					             I((cov.age/10)^2) + cov.education + cov.female,
				          	   data = nigeria, p = p, p1 = p1, p0 = p0,
				          	   design = "forced-known")

summary(rr.q1.reg.obj)


# Define design parameters
set.seed(44)

p  <- 2/3  # probability of answering honestly in Forced Response Design
p1 <- 1/6  # probability of forced 'yes'
p0 <- 1/6  # probability of forced 'no'

# rrreg.predictor() fits a joint model of responses to an outcome regression of 
# joining a civic group and the randomized response item of having a militant 
# social connection

rr.q1.pred.obj <- rrreg.predictor(civic ~ cov.asset.index + cov.married +
					                			  I(cov.age/10) + I((cov.age/10)^2) +
		                						  cov.education + cov.female + 
	                							  rr.q1, rr.item = "rr.q1",
	                							  parstart = FALSE, estconv = TRUE, 
	                							  data = nigeria, verbose = FALSE,
	                							  optim = TRUE, p = p, p1 = p1, p0 = p0,
	                							  design = "forced-known")

summary(rr.q1.pred.obj)

# Generate predicted probabilities for the likelihood of joining
# a civic group across respondents using quasi-Bayesian simulations.
rr.q1.rrreg.predictor.pred <- predict(rr.q1.pred.obj, 
							                   		  avg = TRUE, quasi.bayes = TRUE,
					                  				  n.sims = 10000)

print(rr.q1.rrreg.predictor.pred)
