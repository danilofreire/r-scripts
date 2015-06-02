# Probability of >=2 people having the same birthday (as shown by Gary King in his class)

sims <- 1000
people <- 24
alldays <- seq(1, 365, 1)
sameday <- 0
for (i in 1:sims) {
  room <- sample(alldays, people, replace = TRUE)
  if (length(unique(room)) < people) # same birthday
    sameday <- sameday+1
}
cat("Probability of >=2 people having the same birthday:", sameday/sims, "\n")

sims <- 1000 # set parameters
bernpi <- 0.2
u <- runif(sims) # uniform sims
y <- as.integer(u < bernpi)
summary(y)
