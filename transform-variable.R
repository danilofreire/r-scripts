# Just create a simple function (here ``transf.function'') to do the recoding

# For instance, to subtract 1 from the original variable, square them and
# save as ``variable1'' in a dataset ``d1''

transf.function <- function(x) (x-1)^2
d1$variable <- trans.function(d1[['variable']])
