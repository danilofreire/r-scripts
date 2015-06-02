# Functions are a very useful tool in R. 
# They can be saved as objects, nested and passed as arguments to other functions.
# They are created with the function() command. For instance, the function

f <- function(x){
x = (x/(5+x)) # the number dictates the steepness of the curve, 3, 4, 5, etc
}

plot(f1(a))
abline(h = .5, v = 5) # the probability of .5 is always equal to a + 1

# Logistic function

f1 <- function(x){
  x = 1/(1+exp(-b*x)) # b dictates the steepness of the curve, 3, 4, 5, etc
}

a <- seq(0,20) 
b <- .2 # controls the steepness of the curve

plot(f1(a))
abline(h = .5, v = 5) # the probability of .5 is always equal to a + 1

# Squares any value of X. You can try with the following example

var <- seq(1:10)
print(var)

f  <- function(x){
x = x^2
}

var2 <- f(var)
print(var2)

# Another, more complex example:

x <- seq(1:10)
y <- rep(20,10)
z <- c(1,2,2,2,2,2,3,4,4,5)


f <- function(x, y, z = 2) {
        x1 <- x + rnorm(length(x), z)
        y + x1
}

f(x,y,z)

# The function here takes all three values, x, y and z.


# The ... argument indicate a variable number of arguments that are usually passed on 
# to other functions.
# ... is often used when extending another function and you don’t want to copy
# the entire argument list of the original function.

# For instance, you want to extend the original plot function:

myplot <- function(x, y, type = "l", ...) {
plot(x, y, type = type, ...)
}

# If you want to pass the argument to a vector, you have to use ifelse
# You can add as many ifelse's you want to

calc <- function(data){
  test <- ifelse(data > 0, (data^2)-1, data)
  test <- ifelse(data == 0, NA, data)
  test <- ifelse(data == -1, -99, data)
  print(test)
}
data <- c(-10:10)
calc(data)

# return a single value

mean1 <- function(x){
  test <- ifelse(data, sd(data), sd(data))
  print(test[1])
}

data <- c(1:100)
mean1(a)

