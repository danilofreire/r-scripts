# A script for the Amelia II package (data imputation): http://gking.harvard.edu/amelia

# Load the package
library(Amelia)

# Now we load a data set with missing values
library(Zelig)
data(freetrade)
summary(freetrade)

# Imputing the values for the missing observations. We simulate 5 data sets
# (the number the authors recommend), but one can simulate more.
# TS is a time variable, and CS is an id variable. ORDS are ordinal variables
# and NOMS, nominal ones. One can also add variables with lag or lead.
# One can also incorporate informative priors to the imputation,
# see: http://r.iq.harvard.edu/docs/amelia/amelia.pdf pp.24
# If you don't want anything like that, just ignore what you've just read :)


a.out <- amelia(freetrade, m = 5, ts = "year", cs = "country",
                ords = "polity", noms = "signed", p2s = 0)

# Checking out the ordinal and nominal variables (in the first simulated data set):
View(a.out$imputations[[1]])
table(a.out$imputations[[1]]$polity)
table(a.out$imputations[[1]]$signed)

# Transforming an imputed variabled
a.out <- transform(a.out, lgdp = log(gdp.pc))

# Estimating the first model with missing data:
model1 <- zelig(tariff ~ polity + pop + gdp.pc + year + country,
                model = "ls", data = freetrade)

summary(model1)

# Estimating the second model with 5 imputed data sets:
model2 <- zelig(tariff ~ polity + pop + gdp.pc + year + country,
                model = "ls", data = a.out)

summary(model2)

# Estimating the same model but now only using the first 3 imputed data sets
summary(model2, subset = 1:3)

# Running the models with one data set at a time
print(summary(model2), subset = 1:5)

# Done! That's it!

# Save the imputed data sets in R format:
save(a.out, file = "imputations.RData")

# Or in Stata format:
write.amelia(obj=a.out, file.stem = "outdata", format = "dta")
