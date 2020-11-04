# http://www.statmethods.net/stats/rdiagnostics.html
# Regression Diagnostics

# Assume that we are fitting a multiple linear regression
# on the MTCARS data
library(car)
fit <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)

# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots 
av.plots(fit)

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)

# Influence Plot 
influencePlot(fit,id.method="identify", main="Influence Plot",
              sub="Circle size is proportial to Cook's Distance")

# distribution of studentized residuals
library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid), max(sresid), length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)

# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)

# Evaluate Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)

# Ceres plots 
ceresPlots(fit)

# Test for Autocorrelated Errors
durbinWatsonTest(fit)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit) 
summary(gvmodel)
