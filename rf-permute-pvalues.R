## Confidence interval for Random Forest algorithms

# The package rfPermute calculates p-values for RF.
# Here is one example with a classification problem.

# Load required packages
library(Zelig) # dataset
library(randomForest)
library(rfPermute)

# Load data
data(voteincome)
str(voteincome)

# Remove constant predictor and code dependent variable as factor
df <- voteincome[,-2]
df$vote <- as.factor(df$vote)
str(df)

# Random forest
print(rf <- randomForest(vote ~ ., ntree = 500,
                         importance = TRUE, data = df))

# Confusion matrix
classConfInt(rf)
confusionMatrix(rf, conf.level = 0.95, threshold = 0.8)

# Graph
impHeatmap(rf, xlab = "Transmission", ylab = "Predictor")

# Estimate permutation p-values for Random Forest importance metrics
rf2 <- rfPermute(vote ~ ., data = df, ntree = 500, 
                 na.action = na.omit, nrep = 50, 
                 num.cores = 2)

# Extract a matrix of importance and p-values
rp.importance(rf2, scale = TRUE, decreasing = TRUE)

# Plot unscaled importance distributions
plot(rp.importance(rf2, scale = FALSE))

# Plot scaled measures
plot(rp.importance(rf2, scale = TRUE))
