# Using the sample function:
# sample(x, size, replace = FALSE, prob = NULL)
# sample.int(n, size = n, replace = FALSE, prob = NULL)
# sample.int is a bare interface in which both n and size must be supplied as integers.

?sample

outcomes <- c("Danilo", "Mira", "Nobody")
sample100 <- sample(outcomes,            # data set
                    100,                 # sample size
                    replace = TRUE,      # sampling with replacement
                    prob = c(.2,.5,.3))  # probabilities for each observation
table(sample100)

# Using sample and a loop:
outcomes1 <- 0:10         # vector from 0 to 10
sample500 <- rep(NA, 500) # creating an empty vector

for(i in seq(along=sample500)){
        samp <- sample(outcomes1, 200, replace = TRUE) # Samples of size 200
        sample500[i] <- mean(samp)                     # Takes the mean of such samples
}

summary(sample500)
plot(density(sample500))           # density plot of the sample means
quantile(sample500, c(.025, .975)) # 95% interval of the distributions of the means

# Using the caTools package:
require(caTools)
sample = sample.split(data, SplitRatio = .75)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE) 
