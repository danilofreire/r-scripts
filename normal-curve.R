#
#
########### Generating a Normal Curve ###########
#

Z <- function(x){(1/sqrt(2*pi))*(exp(x^2/-2))}
Z

#################################################

n <- 100
x <- seq(-3, 3, length = n)
hist(x, col = "lightgreen", prob = TRUE)
lines(density(x), col = "blue")

plot(x,Z(x))
sum(Z(x)/sum(Z(x)))

#################################################

n <- 2000
x <- rnorm(n)
hist(x, col = "lightgreen", prob = TRUE)
lines(density(x), col = "blue")

plot(x,Z(x), pch = ".")
sum(Z(x)/sum(Z(x)))

rm(n, x, Z); ls()

#################################################

norm.fun <- function(x){
  ( 1/(1*sqrt(2*pi)) ) * exp( -.5*((x - 0)/1)^2 )
  }

x <- seq(-3, 3, by = .02)
head(x)
length(x)

norm.fun(x = x[1])
norm.fun(x = x[2])

y <- norm.fun(x)
plot(x,y)

rm(x,y,norm.fun); ls()

#################################################

x <- rnorm(1000)
y <- density(x)
head(y$y, 25)

hist(x, prob = T, col = "lightgreen")
lines(density(x), col = "blue")

rm(x, y); ls()

#################################################

mu <- 0
sigma <- 1
xlow <- mu - 3*sigma
xhigh <- mu + 3*sigma
dx <- .02
x <- seq(xlow, xhigh, by = dx)
y <- (1/(sigma*sqrt(2*pi))) * exp(-.5 * ((x - mu)/sigma)^2)
plot(x,y, col = "lightgreen", type = "h", lwd = 1, cex.axis = 1.5,
     xlab = "x", ylab = "p(x)", cex.lab = 1.5,
     main = "Normal Probability Density", cex.main = 1.5)
lines(x,y, col = "blue")

rm(dx, mu, sigma, x, xhigh, xlow, y); ls()


# End
