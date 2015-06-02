# Some ways to create variables in R

years <- rep(1945:2014, times = 5) # repeating years
country <- rep(c("BR", "CH", "DE", "UK", "US"), each = 70) # repeating strings
set.seed(222) # setting a reproducible seed
gdp <- round(rnorm(350, 10000, 1000)) # normal distribution (n, mean, sd)
set.seed(666)
polity <- round(rnorm(5, 5, 2))
set.seed(123)
polity2 <- round(rnorm(5, 4, 1))
df <- data.frame(cbind(years, country, gdp, polity, polity2))
df$gdp <- as.numeric(as.character(df$gdp))
df$polity <- as.numeric(as.character(df$polity))
df$polity2 <- as.numeric(as.character(df$polity2))

# Running a simple model with the fake data set
m1 <- lm(polity ~ gdp + polity2, data = df)
summary(m1)
rm(list=ls())
