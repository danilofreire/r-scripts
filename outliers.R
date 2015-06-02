# DFBETAS is a good command to assess the influence of outliers.
# It calculates what beta would change if a given data point is excluded.

# Let us use the macro data from Zelig
library(Zelig)
data(macro)

model.1 <- lm(gdp ~ trade + unem, data = macro)

# Now using the command

df.betas.1 <- dfbetas(model.1)
head(df.betas.1)

# This nice plot shows the influence of any particular data point on the regression line:
# the CEX command calls a bubble, which can change in size according to df.betas.1.
plot(macro$gdp ~ macro$trade, cex=abs(10*(df.betas.1[,2])),
     xlab = "Trade (% GDP)", ylab = "GDP", main = "Trade on GDP")
abline(lm(gdp ~ trade, data = macro), col = "red")

# One can also check the dfbetaPlots() in the car package.
library(car)
dfbetaPlots(model.1)

# The avPlots.R script in this repository also shows how to assess the presence of
# outliers with graphs. Take a look! :)
