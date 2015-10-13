# Differences-in-differences estimation in R
# By Kevin Goulding
# Code: https://thetarzan.wordpress.com/2011/06/20/differences-in-differences-estimation-in-r-and-stata/

# Load the foreign package
library(haven)

# First download the file eitc.dta from this link:
# https://docs.google.com/open?id=0B0iAUHM7ljQ1cUZvRWxjUmpfVXM
# Then import from your hard drive:
eitc <- read_dta("/home/sussa/Downloads/eitc.dta")

# Create two additional dummy variables to indicate before/after
# and treatment/control groups.

# the EITC went into effect in the year 1994
eitc$post93 <- as.numeric(eitc$year >= 1994)

# The EITC only affects women with at least one child, so the
# treatment group will be all women with children.
eitc$anykids <- as.numeric(eitc$children >= 1)

# Compute the four data points needed in the DID calculation:
a <- sapply(subset(eitc, post93 == 0 & anykids == 0, select = work), mean)
b <- sapply(subset(eitc, post93 == 0 & anykids == 1, select = work), mean)
c <- sapply(subset(eitc, post93 == 1 & anykids == 0, select = work), mean)
d <- sapply(subset(eitc, post93 == 1 & anykids == 1, select = work), mean)

# Compute the effect of the EITC on the employment of women with children:
(d-c)-(b-a)

# Run a simple differences-in-differences regression 
eitc$p93kids.interaction <- eitc$post93*eitc$anykids
reg1 <- lm(work ~ post93 + anykids + p93kids.interaction, data = eitc)
summary(reg1)

# Another example. Here unemployment is the dependent variable and
# we assume that Austria received the treatment in 1980
library(Zelig)
data(macro)
macro <- subset(macro, country == c("Austria", "Belgium"))

# Create a dummy for treatment
macro$year1980 <- as.numeric(macro$year >= 1980)
macro$austria <- as.numeric(macro$country == "Austria")

# Compute the four data points:
a1 <- sapply(subset(macro, year1980 == 0 & austria == 0, select = unem), mean)
b1 <- sapply(subset(macro, year1980 == 0 & austria == 1, select = unem), mean)
c1 <- sapply(subset(macro, year1980 == 1 & austria == 0, select = unem), mean)
d1 <- sapply(subset(macro, year1980 == 1 & austria == 1, select = unem), mean)

# Compute differences-in-differences
(d1-c1)-(b1-a1)        

# Regression
macro$austria.year1980 <- macro$year1980*macro$austria
reg2 <- lm(unem ~ austria + year1980 + austria.year1980, data = macro)
summary(reg2)
