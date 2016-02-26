# From: http://sgsong.blogspot.co.uk/2016/02/multiple-imputation-using-r.html

# R has a long list of packages for multiple imputation. The main problem is integration:
# statistical procedures in other packages may or may not work with the imputation procedures.
# I have been using Amelia together with Zelig. Because they were written by the same group,
# they work well together. However, I have been having trouble with making multiple imputation
# to work with the plm package. After searching the internet, here comes the solution:

# Impute the missing data using Amelia or Mice.
# Estimate the model on each imputed data.
# Use the mitools package to extract and combine results. 
# For example, here is a simple example:

# ...

imp <- mice(d)

mydata <- imputationList(lapply(1:5, complete, x = imp))

fit <- lapply(mydata$imputations, function(x){
 plm(cog3pl ~ oc + grade9 + boy + han + ruralbirth, data = x,
   index = c("schids"), model = "pooling")})
   
betas <- MIextract(fit, fun = coef)

vars <- MIextract(fit, fun = vcov)

summary(MIcombine(betas, vars))

# I bet this will work for most, if not all, estimation procedures in R.
