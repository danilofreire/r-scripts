# Differences-in-differences estimation in R
# By Kevin Goulding
# Code: https://thetarzan.wordpress.com/2011/06/20/differences-in-differences-estimation-in-r-and-stata/

# Here unemployment is the dependent variable
# and we assume that Japan received the treatment in 1980
library(Zelig)
data(macro)
macro.sub <- subset(macro, country == c("Japan", "Belgium"))

# Create dummies for treatment
macro.sub$year1980 <- as.numeric(macro.sub$year >= 1980)
macro.sub$japan <- as.numeric(macro.sub$country == "Japan")

# Compute the four data points:
a <- sapply(subset(macro.sub, year1980 == 0 & japan == 0, select = unem), mean)
b <- sapply(subset(macro.sub, year1980 == 0 & japan == 1, select = unem), mean)
c <- sapply(subset(macro.sub, year1980 == 1 & japan == 0, select = unem), mean)
d <- sapply(subset(macro.sub, year1980 == 1 & japan == 1, select = unem), mean)

# Compute differences-in-differences
(d-c)-(b-a)        

# Regression
macro.sub$japan.year1980 <- macro.sub$year1980*macro.sub$japan # interaction
reg1 <- glm(unem ~ japan + year1980 + japan.year1980, data = macro.sub)
summary(reg1)

# Complete data set, matching before differences-in-differences
library(MatchIt)
library(cem)

# Dummies
macro$year1980 <- as.numeric(macro$year >= 1980)
macro$japan <- as.numeric(macro$country == "Japan")
macro$japan.year1980 <- macro$year1980*macro$japan

# Matching
match1 <- matchit(japan ~ capmob + gdp + trade, method = "cem", data = macro)
m.data <- match.data(match1)

# Regression
reg2 <- lm(unem ~ japan + year1980 + japan.year1980, data = m.data)
summary(reg2)
