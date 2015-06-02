# "Whatif?": Software for Evaluating Counterfactuals (Stoll, King and Zeng, 2010)
# http://gking.harvard.edu/whatif
# http://gking.harvard.edu/files/abs/counterft-abs.shtml Read!
# http://gking.harvard.edu/files/abs/counterf-abs.shtml Read it too!

# Not all counterfactuals should be evaluated. As the authors say:
# "[...] when the counterfactuals posed are too far from the data at hand, conclusions drawn
# from well-specified statistical analyses become based on speculation and convenient
# but indefensible model assumptions rather than empirical evidence. Unfortunately,
# standard statistical approaches assume the veracity of the model rather than 
# revealing the degree of model-dependence, and so this problem can be hard to detect."

# WhatIf is a package that helps to identify such invalid counterfactuals
# It checks if the counterfactual data are in convex hull

# An example:
library(WhatIf)  # the package itself
library(Zelig)   # model estimation

# Load the dataset
data(turnout)
turnout$race1 <- ifelse(turnout$race=="white",0, 1) # recode treatment as non-white

# Estimate a simple model
model1 <- zelig(vote ~ race1 + age + educate + income, data = turnout, model = "logit")
summary(model1)

# Evaluate a counterfactual where race = 0
sim1 <- setx(model1, race1 = 0)

# Using WhatIf
whif1 <- whatif(data = model1, cfact = sim1)
plot(whif1, type = "f") # cumulative frequencies of the distance
plot(whif1, type = "l") # LOWESS smoothing of cumulative frequencies
plot(whif1, type = "b") # both 
summary(whif1)

# Here we see that the counterfacul is in convex hull, thus being plausible
# Let's try a different counterfactual, where income is 10 times the sample mean

sim2 <- setx(model1, income = 10 * mean(turnout$income))
whif2 <- whatif(data = model1, cfact = sim2)
plot(whif2, type = "b") 
summary(whif2)

# Or more generally:
# Create a new dataset with the quantities of interest of the covariates (no intercept!)
summary(whatif(data = model1, cfact = data.frame(race1 = 1, 
                                                 age = mean(turnout$age),
                                                 educate = mean(turnout$educate),
                                                 income = 10*mean(turnout$income))))

# The test correctly identified an improper counterfactual
