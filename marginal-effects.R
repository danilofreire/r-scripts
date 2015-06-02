# Marginal effects using the package "effects"
# The syntax is quite simple and it follows the base graphs package.
# Let us estimate a logistic model with the "turnout" data from Zelig

library(effects)
library(Zelig)
data(turnout)

model.1 <- glm(vote ~ income + educate + age + I(age^2) + race + race*educate,
               family = "binomial" (link = "logit"), data = turnout)
summary(model.1)

# Plot the effect of income:
plot(effect(term = "income", mod = model.1, default.levels = 20),
     rescale.axis = FALSE, multiline = FALSE,
     ylab = "Predicted Probability of Voting", xlab = "Income",
     main = "Marginal Effect of Income on Voting")
