####################################
### Endorsement Experiments in R ###
####################################

# Now we'll learn how to use the "endorsement" package in R.
# The package implements the methods presented in Bullock, Imai, and Shapiro
# (2011; Political Analysis) to analyze endorsement experiments. As the authors
# say, "Endorsement experiments are a survey methodology for eliciting truthful
# responses to sensitive questions. This methodology is helpful when measuring 
# support for socially sensitive political actors such as militant groups. 
# The model is fitted with the Markov chain Monte Carlo algorithm and produces
# the output containing draws from the posterior distribution."

# The package is available on CRAN.
install.packages("endorse") 

# Load the package
library(endorse)

# The endorse() function estimates the model described above. It has lots
# of parameters. Please take a look at them 
?endorse

# Now let's run the command. We'll use the "pakistan" data set.
data(pakistan)

str(pakistan)

# Dependent variable
Y <- list(Q1 = c("Polio.a", "Polio.b", "Polio.c", "Polio.d", "Polio.e"),
	Q2 = c("FCR.a", "FCR.b", "FCR.c", "FCR.d", "FCR.e"),
	Q3 = c("Durand.a", "Durand.b", "Durand.c", "Durand.d", "Durand.e"),
	Q4 = c("Curriculum.a", "Curriculum.b", "Curriculum.c", "Curriculum.d",
	 "Curriculum.e"))

## Varying-lambda non-hierarchical model without covariates
endorse.out <- endorse(Y = Y, data = pakistan, identical.lambda = FALSE,
	covariates = FALSE, hierarchical = FALSE)

summary(endorse.out$beta)

# Varying-lambda non-hierarchical model with covariates
indiv.covariates <- formula( ~ female + rural)

endorse.out <- endorse(Y = Y, data = pakistan, identical.lambda = FALSE,
	covariates = TRUE, formula.indiv = indiv.covariates,
	hierarchical = FALSE)

# Common-lambda non-hierarchical model with covariates
indiv.covariates <- formula( ~ female + rural)

endorse.out <- endorse(Y = Y, data = pakistan, identical.lambda = TRUE,
	covariates = TRUE, formula.indiv = indiv.covariates,
	hierarchical = FALSE)

# Varying-lambda hierarchical model without covariates
div.data <- data.frame(division = sort(unique(pakistan$division)))

div.formula <- formula(~ 1)

endorse.out <- endorse(Y = Y, data = pakistan, data.village = div.data,
	village = "division", identical.lambda = FALSE,
	covariates = FALSE, hierarchical = TRUE,
	formula.village = div.formula)

# Varying-lambda hierarchical model with covariates

endorse.out <- endorse(Y = Y, data = pakistan, data.village = div.data,
	village = "division", identical.lambda = FALSE,
	covariates = TRUE,
	formula.indiv = indiv.covariates,
	hierarchical = TRUE,
	formula.village = div.formula)

# Common-lambda hierarchical model with covariates

endorse.out <- endorse(Y = Y, data = pakistan, data.village = div.data,
	village = "division", identical.lambda = TRUE,
	covariates = TRUE,
	formula.indiv = indiv.covariates,
	hierarchical = TRUE,
	formula.village = div.formula)
