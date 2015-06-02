# How to use simulation to represent uncertainty in regression coefficients:
# An informal Bayesian approach (Gelman & Hill, 2007, p. 140)

# Load necessary packages
library(Zelig) # data sets
library(arm)   # simulations

# Load data set 
data(turnout)

# Model
m1 <- bayesglm(vote ~ educate + income + race + race*income + age + I(age^2),
               binomial("logit"), turnout)
display(m1)

# Simulate
sim1 <- arm::sim(m1)
str(sim1)

# Quantities of interest
educate_sim1 <- sim1@coef[,2]
mean(educate_sim1)                   # mean estimate
hist(educate_sim1)
quantile(educate_sim1, c(.025,.975)) # 95% interval

# Simulations also make interactions easy to understand and interpret 
interaction_inc_race <- sim1@coef[,5]
mean(interaction_inc_race)
hist(interaction_inc_race)
quantile(interaction_inc_race, c(.025,.975))

# Plots
plot(jitter(turnout$educate, 2), jitter(turnout$vote, .05),
     pch=20,
     cex = .6, xlab = "Education Years", 
     ylab = "Probability of Voting",
     main = "Predicted Values",
     axes=FALSE, 
     xlim=c(0,20),
     ylim=c(0,1))

axis(1) # adds x axis
axis(2) # adds y axis

# Add curves. If you're using this code for a linear model, just remove invlogit()
for(i in 1:100){
        curve(invlogit(sim1@coef[i,1] +
                              sim1@coef[i,2]*x +
                              sim1@coef[i,3]*mean(turnout$income) +
                              sim1@coef[i,4]*0 + 
                              sim1@coef[i,5]*0 +
                              sim1@coef[i,6]*mean(turnout$age) +
                              sim1@coef[i,7]*mean(I(turnout$age^2))), 
              col="gray", add=TRUE)
}
curve(invlogit(m1$coefficients[1] +
                              m1$coefficients[2]*x +
                              m1$coefficients[3]*mean(turnout$income) +
                              m1$coefficients[4]*0 +
                              m1$coefficients[5]*0 +
                              m1$coefficients[6]*mean(turnout$age) +
                              m1$coefficients[7]*mean(I(turnout$age^2))),
      col="dark red", lwd=4, add=TRUE)


# Plot the interaction "income*race"


# Plots
plot(jitter(turnout$income, 2), jitter(turnout$vote, .05), pch=20,
     cex = .6, xlab = "Income*Race (Blue = 1)", 
     ylab = "Probability of Voting", main = "Predicted Values", axes=FALSE, 
     xlim=c(0,20), ylim=c(0,1))

axis(1) # adds x axis
axis(2) # adds y axis

# Add curves
for(i in 1:100){
        curve(invlogit(sim1@coef[i,1] +
                               sim1@coef[i,2]*mean(turnout$educate) +
                               sim1@coef[i,3]*x +
                               sim1@coef[i,4]*0 +
                               sim1@coef[i,5]*0*x +
                               sim1@coef[i,6]*mean(turnout$age) +
                               sim1@coef[i,7]*mean(I(turnout$age^2))), 
              col="indianred", add=TRUE)
}

for(i in 1:100){
        curve(invlogit(sim1@coef[i,1] +
                               sim1@coef[i,2]*mean(turnout$educate) +
                               sim1@coef[i,3]*x +
                               sim1@coef[i,4]*1 +
                               sim1@coef[i,5]*1*x +
                               sim1@coef[i,6]*mean(turnout$age) +
                               sim1@coef[i,7]*mean(I(turnout$age^2))), 
              col="light blue", add=TRUE)
}

curve(invlogit(m1$coefficients[1] +
                       m1$coefficients[2]*mean(turnout$educate) + 
                       m1$coefficients[3]*x +
                       m1$coefficients[4]*0 +
                       m1$coefficients[5]*0*x +
                       m1$coefficients[6]*mean(turnout$age) +
                       m1$coefficients[7]*mean(I(turnout$age^2))),
      col="dark red", lwd=4, add=TRUE)

curve(invlogit(m1$coefficients[1] +
                       m1$coefficients[2]*mean(turnout$educate) +
                       m1$coefficients[3]*x +
                       m1$coefficients[4]*1 +
                       m1$coefficients[5]*x +
                       m1$coefficients[6]*mean(turnout$age) +
                       m1$coefficients[7]*mean(I(turnout$age^2))),
      col="dark blue", lwd=4, add=TRUE)
