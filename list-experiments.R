#######################################
### Analysing List Experiments in R ###
#######################################

# As the abstract says: " "list" is a publicly available R package that allows
# researchers to conduct a multivariate statistical analysis for the item count
# technique. This survey methodology is also known as the list experiment or the
# unmatched count technique is an alternative to the commonly used randomized 
# response method. The package implements the methods described in Imai (2011) 
# and Blair and Imai (2012), a Bayesian MCMC implementation of regression for the
# standard and multiple sensitive item list experiment designs including ceiling
# and floor effects and a random effects setup, and a Bayesian MCMC hierarchical
# regression model with up to three hierarchical groups."

# Load the package
library(list)

# We shall use two data sets to illustrate the commands. They are included
# in the "list" package
data(affirm)
data(race)

str(affirm)
str(race)

# First, we'll use the ict.test() function. According to the manual, it says
# "Function to conduct a statistical test with the null hypothesis that there is no
# "design effect" in a list experiment, a failure of the experiment." 
# In the example below, J = the number of non-sensitive (control) survey items,
# gms indicates if generalized moment selection procedure should be used,
# and y and treat shows, respectively, the vectors with the dependent and
# treatment variables. The function returns a good description of the test. 

test.value.affirm <- ict.test(affirm$y, affirm$treat, J = 3, gms = TRUE)
print(test.value.affirm)

test.value.race <- ict.test(race$y, race$treat, J = 3, gms = TRUE)
print(test.value.race)

# Next, the ictreg() function. It perform regression analysis on data from
# the item count technique, also known as the list experiment and the unmatched
# count technique. 

# Calculate list experiment difference in means

diff.in.means.results <- ictreg(y ~ 1, data = race,
	treat = "treat", J = 3, method = "lm")

summary(diff.in.means.results)

# Fit linear regression
# Replicates Table 1 Columns 1-2 Imai (2011); note that age is divided by 10

lm.results <- ictreg(y ~ south + age + male + college, data = race,
	treat = "treat", J = 3, method = "lm")

summary(lm.results)

# Fit two-step non-linear least squares regression
# Replicates Table 1 Columns 3-4 Imai (2011); note that age is divided by 10

nls.results <- ictreg(y ~ south + age + male + college, data = race,
	treat = "treat", J = 3, method = "nls")

summary(nls.results)

# There are several other models that can be estimated with ictreg().
# Please check the manual. 

# One can also plot the results of ictreg() models.

data(race)
race.south <- race.nonsouth <- race
race.south[, "south"] <- 1
race.nonsouth[, "south"] <- 0

# Fit EM algorithm ML model with constraint

ml.constrained.results <- ictreg(y ~ south + age + male + college,
	data = race, treat = "treat", J = 3, method = "ml",
	overdispersed = FALSE, constrained = TRUE)

# Calculate average predictions for respondents in the South
# and the the North of the US for the MLE model, replicating the
# estimates presented in Figure 1, Imai (2011)

avg.pred.south.mle <- predict(ml.constrained.results,
	newdata = race.south, avg = TRUE, interval = "confidence")

avg.pred.nonsouth.mle <- predict(ml.constrained.results,
	newdata = race.nonsouth, avg = TRUE, interval = "confidence")

# A plot of the two estimates and their confidence intervals
# use c() to combine more than one predict object for plotting

plot(c(avg.pred.south.mle, avg.pred.nonsouth.mle), labels = c("South", "Non-South"))

# We can also use ictregBayes(), which performs model estimations
# using Bayesian models. The function requires the "MASS" package.
# To be honest, it didn't work when I tested it, but who knows? :)

# Well, that's all for today. Check the package's documentation
# for more information!
