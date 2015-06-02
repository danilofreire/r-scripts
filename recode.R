# How to recode a variable in categories:

# We will use the funcion `recode' available in the "car" package
library(car)

# General syntax: recode(var, recodes, as.factor.result, as.numeric.result=TRUE, levels)
# Create a simple vector
var <- c(1,2,1,2,3,3,3,NA)

# Recoding 1 and 2 to A, 3 to B and NA to zero
y <- Recode(var, "c(1,2)='A'; 3='B'; NA='0'")

# Similarly,
y <- Recode(var, "1:2='A'; 3='B'; NA='0'")
