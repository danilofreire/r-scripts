# How to use the simple (but very useful) ifelse command
# Ifelse returns a value for true or false
# in a given vector. It is used as

# ifelse(test, true, false)

# Let us give an example

x <- rnorm(100)
y <- runif(100)
df <- data.frame(x,y)
df$test <- ifelse(df$x > .5, 1, 0)
df$test1 <- ifelse(df$y > .5, 1, 0)

# It also works with & (AND) and | (OR) statements

df$test2 <- ifelse(df$x > .5 & df$y > .5, 1, 0)
df$test3 <- ifelse(df$x > .5 | df$y > .5, 1, 0)
View(df)

# How to "do nothing" if the condition is false.
# Using an example from turnout: 
# If the person didn't vote, we'll classify his income as NA
# If she did vote, we'll keep the values.
# Adding the variable forces the ifelse not to change the values

library(Zelig)
data(turnout)
turnout$test <- ifelse(turnout$vote < 1, NA, turnout$income)
View(turnout)

# Ifelse can be a useful substitute for loops too.
# Imagine you want to create a simple loop to square 1000 values

foo <- seq(1,1000)
foo.sq <- ifelse(foo, foo^2, foo)
View(foo.sq)

# This will do the job. The first is the dataset,
# the second is the true condition (squares the whole dataset)
# and the last condition does nothing. Of course, you can
# use any complex formula for the true value.
