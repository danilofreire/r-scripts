#
#
############### Bayesian Model Averaging examples ###############
#
# This script assumes you have worked through all the previous notes from 
# the web page and you have downloaded, installed, and updated all available
# R packages. 

### Upload the SAS SEM Example data ("SEMData.sav") which is SIMULATION DATA.

library(foreign)

exsem <- read.spss("http://www.unt.edu/rss/class/Jon/R_SC/Module10/SEMData.sav", use.value.labels=TRUE, 
  max.value.labels=Inf, to.data.frame=TRUE)

summary(exsem)
head(exsem)

cor(exsem)

# The goal of the this example is to choose the best set of predictor variables for a linear (OLS) prediction 
# model with Extroversion (extro) as the outcome. 

# Basic linear (OLS) regression model. 

reg.1 <- lm(extro~abstruse+agree+block+cognitive+common+cultural+open+physical+series+sets+social+vocab, data=exsem)
summary(reg.1)

# The BMA (Bayesian Model Averaging) package/library was designed specifically to use Bayesian Model 
# Averaging to address the variable selection problem in the context of several types of models (e.g. 
# GLM, LM, and Survival Models. 
 
library(BMA)

# The function for conducting BMA with respect to linear regression is 'bicreg'. 
# The 'bicreg' function requires a matrix of predictor variables as input. 

attach(exsem)
predictors <- as.matrix(cbind(open, agree, social, cognitive, physical, cultural, vocab, abstruse, 
                              block, common, sets, series))
detach(exsem)

# Conduct the BMA using the 'bicreg' function by submitting the matrix of predictors (predictors) 
# and the outcome variable (exsem$extro).

bma1 <- bicreg(predictors, exsem$extro)
summary(bma1)

# Based on the first column of the output we can see that "open", "agree", and "series" are the most 
# important variables; the column 'p!=0' indicates the percentage/probability that the coefficient 
# for a given predictor is NOT zero. We can also see that the first model 'model 1' (which includes 
# only 'open', 'agree', & 'series') is the best because it has the lowest BIC and the largest 
# posterior probability. 

# The 'ols' part of the output (not printed by default) gives a matrix, with each model as a row and 
# each predictor variable as a column; listing the estimated (OLS) coefficient for each variable in 
# a given model. 

bma1$ols

# Likewise, the 'se' part of the output produces a similar matrix, with the standard errors for each 
# coefficient (for each variable/model combination). 

bma1$se

# The 'postmean' part of the output (not printed by default) contains the average posterior coefficient 
# for each predictor. The 'postsd' provides the standard deviation of each average posterior coefficient.

bma1$postmean

bma1$postsd

# The 'which' part of the output (not provided by default) contains a matrix, with each model as a row and 
# each predictor variable as a column; listing whether a variable was included in each model. 

bma1$which

# The BMA package also contains a plot function for displaying the posterior distributions of the 
# coefficients. 

plot(bma1)

# For a complete description of the 'bicreg' function:

help(bicreg)

# Bayesian model averaging can also be conducted when attempting to identify the best set of predictors 
# for a Generalized Linear Model. The 'bic.glm' function is very similar to 'bicreg'; you must 
# supply a matrix or data frame of the independent variables (predictors) and the outcome variable. 
# Results of the following model mirror those above. However, the obvious benefit of the 'bic.glm' function 
# is the ability to specify non-normal error distributions (i.e. non-Gaussian; e.g. binomial).

bma2 <- bic.glm(predictors, exsem$extro, glm.family = "gaussian")
summary(bma2)

# Notice that when specifying "Gaussian" the estimation of the posterior standard 
# deviations is slightly off; therefore, it is best to use 'bicreg' when family = 
# "Gaussian".

bma1$postsd
bma2$postsd

plot(bma2)

# For a complete description of the 'bic.glm' function and its arguments:

help(bic.glm)

### Example of Binomial Logistic Regression using 'bic.glm'.

# Read in the data:

logreg <- read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module9/logreg1.txt",
          header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
summary(logreg)

# Create a matrix of the predictor variables.

attach(logreg)
predictors.logist <- as.matrix(cbind(x1,x2,x3,x4))
detach(logreg)

# Run the 'bic.glm' function specifying the binomial family. 

bma2 <- bic.glm(predictors.logist, logreg$y, glm.family = "binomial")
summary(bma2)

plot(bma2)

### Example of Multinomial Logistic Regression using library 'mlogitBMA' and function 'bic.mlogit'.

library(mlogitBMA)

# Read in the data from the web (data is an SPSS.sav file, so the 'foreign' package is necessary). 

library(foreign)
mdata1 <- 
  read.spss("http://www.unt.edu/rss/class/Jon/R_SC/Module9/MultiNomReg.sav", 
  use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
summary(mdata1)

# Apply the 'bic.logit' function; supplying the formula in standard format, choices represent 
# the choices on the outcome variable (i.e. the categories of the outcome). 

mlog.1 <- bic.mlogit(y ~ x1 + x2 + X3, data = mdata1, choices = 1:3)
summary(mlog.1)

# To see all the arguments of the 'bic.mlogit' function 

help(bic.mlogit)
