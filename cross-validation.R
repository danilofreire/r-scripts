# One good idea to check the fit of a model is to use cross-validation.
# It removes one point (or a series of points) from the original model,
# runs the model again and tries to predict the missing points.


# You can do it with the boot package using the function cv.glm
# Let's test 3 models using the 'turnout' data set from the Zelig package

library(Zelig)
library(boot)

data(turnout)

model.1 <- glm(vote ~ income + educate, family = "binomial"(link = "logit"),
               data = turnout)
model.2 <- glm(vote ~ income + educate + age, family = "binomial"(link = "logit"),
               data = turnout)
model.3 <- glm(vote ~ income + educate + age + I(age^2) + race,
               family = "binomial"(link = "logit"), data = turnout)

cv.linear.model.1 <- cv.glm(data = turnout, glmfit = model.1)
cv.linear.model.2 <- cv.glm(data = turnout, glmfit = model.2)
cv.linear.model.3 <- cv.glm(data = turnout, glmfit = model.3)

# The interesting statistics to look at is DELTA: the smaller, the better.
# Delta is the prediction error

cv.linear.model.1$delta
cv.linear.model.2$delta
cv.linear.model.3$delta

# It shows us that the last model has a (marginally) smaller prediction error,
# thus being the best model.
