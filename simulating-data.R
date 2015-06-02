#
#
###### Generation of simulated data ######
#

# Simulated data is often very useful for exploring how models change 
# based on the relationships among the variables and whether certain 
# variables should be included in the model(s) or not. 


# The following data includes several types of variables with several 
# types/shapes of distributions. 

# The 'car' package/library contains the 'recode' function. 

library(car)
n <- 1000
id <- seq(1:n)
sex <- seq(1:2)
sex <- sample(sex, n, replace = TRUE)
sex <- recode(sex, "1 = 'Female'; 2 = 'Male'")
class.level <- seq(1:4)
class.level <- sample(class.level, n, replace = TRUE)
class.level <- ordered(class.level, levels = c(1,2,3,4), 
               labels = c("Freshman", "Sophomore", "Junior", "Senior"))
age <- c(18:25)
age <- sample(age, n, replace = TRUE)
income <- rbeta(n, 3, 18)
income <- income * 200000

detach("package:car")
detach("package:survival")
detach("package:splines")
detach("package:nnet")

# Using the 'MASS' library to generate some multivariate normal, correlated
# variables; each with a mean of 100. 

Sigma<-matrix(c(1.0, .80, .50, .20,
                .80, 1.0, .05, .05,
                .50, .05, 1.0, .05,
                .20, .05, .05, 1.0), ncol = 4)
x <- mvrnorm(n, Sigma, mu=c(100,100,100,100))
detach("package:MASS")

x4 <- rnorm(n, 100)
x5 <- rexp(n, 4) + 100
hist(x5, col = "lightgreen", prob = TRUE)
lines(density(x5), col = "blue")

x6 <- seq(-10, 10, length.out = n)
x6 <- plogis(x6, location = 0, scale = 1)
plot(x6)

# Using package 'arm' to create a logistic regression model; accurately  
# simulate the binomial outcome variable (y2).

library(arm)
x7 <- rnorm(n)
x8 <- rbinom(n, 1, 0.50)
x9 <- rexp(n, 6)
b0 <- 1
b7 <- 1.5
b8 <- 2
b9 <- 0.5
y2 <- rbinom(n, 1, invlogit(b0 + b7*x7 + b8*x8 + b9*x9))

df.1 <- data.frame(id, sex, class.level, age, income, x, x4, x5, x6, y2, x7, x8, x9)
names(df.1)[6] <- "y1"
names(df.1)[7] <- "x1"
names(df.1)[8] <- "x2"
names(df.1)[9] <- "x3"
head(df.1)

summary(df.1)
rm(n, Sigma, id, sex, class.level, age, income, x, x4, x5, x6, y2, b0, b7, b8, b9, x7, x8, x9)
ls()

################################################################################

# Assessing the data below.

cor(df.1[,6:12])
pairs(df.1[,6:12])

cor(df.1[,13:16])
pairs(df.1[,13:16])

library(BMA)
y1.predictors <- as.matrix(df.1[,7:12])
bma.1 <- bicreg(y1.predictors, df.1$y1)
summary(bma.1)

detach("package:BMA")
detach("package:leaps")
detach("package:arm")
detach("package:R2WinBUGS")
detach("package:coda")
detach("package:lme4")
detach("package:Matrix")
detach("package:lattice")
detach("package:car")
detach("package:nnet")
detach("package:survival")
detach("package:splines")
rm(y1.predictors, bma.1)

mod.4 <- lm(y1 ~ x1 + x2 + x3 + x4 + x5 + x6, df.1)
summary(mod.4)

mod.3 <- lm(y1 ~ x1 + x2 + x3 + x4 + x5, df.1)
summary(mod.3)

mod.2 <- lm(y1 ~ x1 + x2 + x3 + x4, df.1)
summary(mod.2)

mod.y1 <- lm(y1 ~ x1 + x2 + x3, df.1)
summary(mod.y1)

mod.y2 <- glm(y2 ~ x7 + x8 + x9, df.1, family = binomial(logit))
summary(mod.y2)

# Standardized coefficients (Beta coefficients) regression models (lm).

library(QuantPsyc)
lm.beta(mod.4)
lm.beta(mod.3)
lm.beta(mod.2)
lm.beta(mod.y1)
detach("package:QuantPsyc")

rm(mod.4, mod.3, mod.2)

# Global test of linear model assumptions. 

library(gvlma)
global.test <- gvlma(mod.y1)
summary(global.test) 
plot(global.test)
rm(global.test)
detach("package:gvlma")

# Standard regression diagnostic plots. 

oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(mod.y1)
par(oldpar)
rm(oldpar)
ls()






# END; last updated Feb. 7, 2012.
