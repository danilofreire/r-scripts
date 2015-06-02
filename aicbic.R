# The Akaike Information Criterion and Bayesian Information Criterion are tools
# to evaluating model fit.
# The smaller the number, the better.
# It does not say why or how the model is bad,
# but it serves as a good comparison tool.

# Let's run a model using the 'turnout' dataset in Zelig

library(Zelig)

data(turnout)

model.1 <- glm(vote ~ income + educate, family = "binomial"(link = "logit"),
               data = turnout)
model.2 <- glm(vote ~ income + educate + age, family = "binomial"(link = "logit"),
               data = turnout)
model.3 <- glm(vote ~ income + educate + age + I(age^2) + race,
               family = "binomial"(link = "logit"), data = turnout)

AIC(model.1)
AIC(model.2)
AIC(model.3)

# The command shows us the last model is better. Now using BIC.

BIC(model.1)
BIC(model.2)
BIC(model.3)

# Which tells us the same result. It favours model.3

# As one smart dude said: "AIC is better in situations when a false negative finding
# would be considered more misleading than a false positive, and BIC is better 
# in situations where a false positive is as misleading as, or more misleading
# than, a false negative." (http://methodology.psu.edu/eresources/ask/sp07)
