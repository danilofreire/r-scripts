# Matching is an statistical procedure that finds observations that are
# "similar enough" and tries to evaluate the effect of a given treatment
# over non-experimental data. Amongst others, Gelman and Hill (2003), and 
# Gary King et al (2011) have talked about it.  A few examples:
# Giligan and Sergenti "Do UN Interventions Cause Peace?" (QJPS, 2008:3); 
# Nielsen et al "Foreign Aid Shocks as a Cause of Violent Armed Conflict",
# (AJPS, 2011:55(2)). 

# Matching cases with R is very simple. We load the "MatchIt" and "cem" packages
# written by Gary King and his colleages. There are several types of matching
# algorithms, here I use only 2 of them. We always have to check which one is 
# best suited for the job. A review of each matching algorithm included in
# "MatchIt" can be found on Gary King's website (http://gking.harvard.edu/matchit)

library(MatchIt) # matching package
library(rgenoud) # genetic matching
library(cem)     # coarsened exact matching
library(Zelig)   # turnout data set, model estimation
library(arm)     # simulations


# Now we'll estimate the effect of race on voter turnout, trying to find pairs
# that can be meaninfully compared. 

# First, we run the matchit() command on the treatment variable, including the
# other pretreatment covariates that will also appear in the regression model.
data(turnout)
turnout <- data.frame(na.omit(turnout)) #remove NA's
turnout$race1 <- ifelse(turnout$race == "white", 0, 1) # recode treatment as non-white

# Using genetic matching
match1 <- matchit(race1 ~ age + educate + income, data = turnout,  method = "genetic")
summary(match1)

# One can check the matched matrix. The first column are treated subjects and second
# column are control subjects.
match1$match.matrix

# Using coarsened exact matching:
# (http://gking.harvard.edu/files/political_analysis-2011-iacus-pan_mpr013.pdf)
match2 <- matchit(race1 ~ age + educate + income, data = turnout,  method = "cem")
summary(match2)

# One can easily see that the data set is now more balanced. The means of
# the control and the treatment groups are indeed very similar. Some plots:
plot(match2)
plot(match2, type = "hist")
plot(match2, type = "jitter", interactive = FALSE)

# Saving the object into a data frame
m.data <- match.data(match2)
head(m.data)

# To save only the treatment or control groups (for whatever reason),
# simply specify the option "group" as follows:
m.data2 <- match.data(match2, group = "treat")
head(m.data2)
m.data3 <- match.data(match2, group = "control")
head(m.data3)

# Running the model and forcing the data.frame directly in the command
model1 <- glm(vote ~ race1 + age + educate + income, data = m.data, family = binomial("logit"))
summary(model1)

# Then we simulate the effects when race1 equals 0 and 1.
sim0 <- data.frame(intercept = 1, race1 = 0, age = mean(turnout$age),
                   educate = mean(turnout$educate), income = mean(turnout$income))
sims <- arm::sim(model1, n = 1000)

y_sim0 <- rbinom(n = 1000, size = 1, prob = plogis(sims@coef %*% t(as.matrix(sim0))))
mean(y_sim0)
sd(y_sim0)
quantile(y_sim0, c(.025, .975))

sim1 <- data.frame(intercept = 1, race1 = 1, age = mean(turnout$age),
                   educate = mean(turnout$educate), income = mean(turnout$income))
sims <- arm::sim(model1, n = 1000)

y_sim1 <- rbinom(n = 1000, size = 1, prob = plogis(sims@coef %*% t(as.matrix(sim1))))
mean(y_sim1)
sd(y_sim1)
quantile(y_sim1, c(.025, .975))

# Comparing both
mean(y_sim1) - mean(y_sim0)
