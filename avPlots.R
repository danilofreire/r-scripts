# The avPlots function creates a scatterplot after controlling for other variables
# Testing with data from the Zelig package

library(car)

library(Zelig)
data(macro)

# The function requires lm or glm. If no specification is given,
# glm runs a lm with the max.likelihood method.

model1 <- glm(gdp ~ trade + unem, data = macro)

avPlots(model1)
