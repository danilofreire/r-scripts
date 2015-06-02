# Code to create 'coefficient plots' as an alternative to
#   tables for regression models.
# June 29, 2012
# Written by Carlisle Rainey.
# http://www.carlislerainey.com/
# If any errors are discovered, please let me know.
# To make the comments readable, make sure to display the 
#   code in a text editor with a full screen view.

library(arm)  # load the arm package (contains the coefplot function)
library(alr3) # load the alr3 package (contains data set for illustration)
data(highway) # load the highway data set from alr3
d <- highway  # call the data set d for convenience

m <- lm(Rate ~ Lwid + Shld + Lane + Len + ADT + Trks, data = d)  # estimate the normal linear model

# create a vector to store the variable names
var.names <- c("lane width, in feet", "width in feet of outer\nshoulder on the roadway",
  "total number of lanes\nof traffic", "length of the highway\nsegment in miles",
  "average daily traffic\ncount in thousands", "truck volume as a percent\nof the total volume")

### Plot 1 - A Quick Way

coefplot(m)

### Plot 2 - Labels outside graph

# set the graphical parameters
par(
  family = "serif",  # I don't plot in anything but serif
  oma = c(0,0,0,0),  # Since it is a single plot, I set the outer margins to zero.
  mar = c(5,10,4,2)  # Inner margins are set through a little trial and error.
  )

# create an empty plot for total customization
plot(NULL,                              # create empty plot
  xlim = c(-2, 2),                      # set xlim by guessing
  ylim = c(.7, length(var.names) + .3), # set ylim by the number of variables
  axes = F, xlab = NA, ylab = NA)       # turn off axes and labels

# add the data
est <- coef(m)[-1]                                                    # conveniently store the estimates (minus the constant)
se <- sqrt(diag(vcov(m)))[-1]                                         # conveniently store the std. errors (minus the constant)
for (i in 1:length(est)) {                                            # loop over a counter the length of the estimate vector
  points(est[i], i, pch = 19, cex = .5)                               # add the points to the plot
  lines(c(est[i] + 1.64*se[i], est[i] - 1.64*se[i]), c(i, i))         # add the 90% confidence intervals
  lines(c(est[i] + .67*se[i], est[i] - .67*se[i]), c(i, i), lwd = 3)  # add the 50% confidence intervals
  text(-2.9, i, var.names[i], xpd = T, cex = .8)                      # add the variable names
}

# add axes and labels
axis(side = 1)                                                                                          # add bottom axis
abline(v = 0, lty = 3, col = "grey")                                                                    # add verticle line
mtext(side = 1, "Linear Regression Coefficient", line = 3)                                              # label bottom axis
mtext(side = 3, "Linear Regression Model of\n the Accident Rate per Million Vehicle Miles", line = 1)   # add title
box()                                                                                                   # add lines around the plot


### Plot 3 - Labels inside graph

par(
  family = "serif",
  oma = c(0,0,0,0),
  mar = c(5,2,4,2)  # margins adjusted to reflect changing the locations of the labels
  )

plot(NULL,                              
  xlim = c(-2, 2),                      
  ylim = c(.7, length(var.names) + .3), 
  axes = F, xlab = NA, ylab = NA)       


est <- coef(m)[-1]              
se <- sqrt(diag(vcov(m)))[-1]   
for (i in 1:length(est)) {      
  points(est[i], i, pch = 19, cex = .5)
  lines(c(est[i] + 1.64*se[i], est[i] - 1.64*se[i]), c(i, i))
  lines(c(est[i] + .67*se[i], est[i] - .67*se[i]), c(i, i), lwd = 3)
  text(est[i], i, var.names[i], xpd = T, cex = .8, pos = 3)           # add variable labels above the points
}
  
axis(side = 1)
abline(v = 0, lty = 3, col = "grey")
mtext(side = 1, "Linear Regression Coefficient", line = 3)
mtext(side = 3, "Linear Regression Model of\nthe Accident Rate per Million Vehicle Miles", line = 1)
box()

### Plot 4 - Standardized Coefficients

# compute quantiles
d0 <- m[[12]][-1]                 # pull out predictors from the data set
lo <- apply(d0, 2, quantile, 0)   # calculate the minimum of the predictors
md <- apply(d0, 2, quantile, .5)  # calculate the median of the predictors
hi <- apply(d0, 2, quantile, 1)   # calculate the maximum of the predictors

# simulate from the posterior
sims <- coef(sim(m, n = 1000))

par(
  family = "serif",
  oma = c(0,0,0,0),
  mar = c(5,2,4,2)
  )

# create an empty plot for total customization
plot(NULL,                            
  xlim = c(-8, 8),                    
  ylim = c(.7, length(var.names) + .3), 
  axes = F, xlab = NA, ylab = NA)     

# add the data
for (i in 1:(length(coef(m)) - 1)) { 
  x.lo <- md; x.lo[i] <- lo[i]; x.lo <- c(1, x.lo)           # Set the predictor of interest at its minimum and others at their median.
  x.hi <- md; x.hi[i] <- hi[i]; x.hi <- c(1, x.hi)           # Set the predictor of interest at its maximum and others at their median.
  sim.lo <- sims%*%x.lo  				     # Compute predicted values from simulations.
  sim.hi <- sims%*%x.hi                                      # Compute predicted values from simulations.
  sims.dif <- sim.hi - sim.lo                                # Compute the differences.
  q <- quantile(sims.dif, c(.05, .25, .5, .75, .95))         # Find the quantiles of interest.
  points(q[3], i, pch = 19, cex = .5)                        # Plot the medians of the difference.
  lines(c(q[1], q[5]), c(i,i))                               # Plot the 90% confidence intervals.
  lines(c(q[2], q[4]), c(i,i), lwd = 3)                      # Plot the 50% confidence intervals.
  text(q[3], i, var.names[i], xpd = T, cex = .8, pos = 3)    # Add the variable names.
}
  
axis(side = 1)
abline(v = 0, lty = 3, col = "grey")
mtext(side = 1, "First Difference as a Variable\nMoves from Its Minimum to Its Maximum", line = 3)
mtext(side = 3, "Linear Regression Model of\nthe Accident Rate per Million Vehicle Miles", line = 1)
box()

### Plot 5 - Logistic Regression


d <- read.csv("http://www.carlislerainey.com/Files/anes1992.csv")  # call the data set d for convenience

m <- glm(Turnout ~ PartyID.Folded + Age + Female + Black + Union.Member + Education, 
  family = binomial, data = d, x = T)

var.names <- c("partisan strength", "age, in years",
  "female", "African-American", "union member", "education level")


d0 <- m[["x"]][, -1] 
lo <- apply(d0, 2, quantile, 0)
md <- apply(d0, 2, quantile, .5)
hi <- apply(d0, 2, quantile, 1)

sims <- coef(sim(m, n = 1000))

par(
  family = "serif",
  oma = c(0,0,0,0),
  mar = c(5,2,4,2)
  )
# create an empty plot for total customization
plot(NULL,                            
  xlim = c(-.5, 1),                    
  ylim = c(.7, length(var.names) + .3), 
  axes = F, xlab = NA, ylab = NA)     

# add the data
for (i in 1:(length(coef(m)) - 1)) {  # loop over a counter the length of the estimate vector
  x.lo <- md; x.lo[i] <- lo[i]; x.lo <- c(1, x.lo)
  x.hi <- md; x.hi[i] <- hi[i]; x.hi <- c(1, x.hi)
  sim.lo <- plogis(sims%*%x.lo)                        # be sure to convert the linear predictor using the link function
  sim.hi <- plogis(sims%*%x.hi)                        # be sure to convert the linear predictor using the link function
  sims.dif <- sim.hi - sim.lo
  q <- quantile(sims.dif, c(.05, .25, .5, .75, .95))
  points(q[3], i, pch = 19, cex = .5)
  lines(c(q[1], q[5]), c(i,i))
  lines(c(q[2], q[4]), c(i,i), lwd = 3)
  text(q[3], i, var.names[i], xpd = T, cex = .8, pos = 3)
}
  
axis(side = 1)
abline(v = 0, lty = 3, col = "grey")
mtext(side = 1, "First Difference as a Variable\nMoves from Its Minimum to Its Maximum", line = 3)
mtext(side = 3, "Logistic Regression Model of\n(Self-Reported) Voting", line = 1)
box()
