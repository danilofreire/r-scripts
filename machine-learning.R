####################################
##### Machine Learning Methods #####
####################################


# This script will show how to implement simple machine learning techniques in R.
# The code widely based on Brett Lantz's excellent "Machine Learning with R".
# Machine learning methods are very useful for prediction or classification.
# Here I briefly demonstrate how they work. Quite obviously, I use the "iris" data set.


# Load necessary packages
library(caret) # various convenience functions
library(class) # knn()
library(gmodels) # CrossTable()
library(C50) # decision trees
library(party) # decision trees
library(rpart) # decision trees
library(rpart.plot) # plots for decision trees
library(rattle) # fancyRpartPlot()
library(randomForest) # randomForest()
library(DMwR) # SMOTE for unbalanced data
library(Hmisc) # cut2()
library(ggplot2) # graphs

# Set global seed
set.seed(111)

# Load the data set
data(iris)

##### Classification using k-Nearest Neighbours 

# First, prepare the data set.

# Check the dependent variable
round(prop.table(table(iris$Species)) * 100, digits = 1)

# Create training set with n = 90 and test set with n = 60 (rule of thumb: 60% training, 40% test).
# Since data are ordered we need to use a random sample.
# Remove the dependent variable (5) from the training and the testing set
dim(iris)
str(iris)

# Sampling
sample.numbers <- sample(1:nrow(iris), 90, replace=FALSE)
iris.train <- iris[sample.numbers,]
iris.train1 <- iris.train[-5]
iris.test <- iris[-(sample.numbers),]
iris.test1 <- iris.test[-5]

# Add the dependent variable
iris.train.labels <- iris.train[, 5]
iris.test.labels <- iris.test[, 5]

# Building the classifier. A good rule of thumb is to use the square root of N as
# the number of neighbours. In this case, sqrt(120) = 10.95
pred.knn <- knn(train = iris.train1, test = iris.test1, 
                cl = iris.train.labels, k = 11)

# Evaluating model performance
CrossTable(x = iris.test.labels, y = pred.knn, prop.chisq = FALSE)

# The table shows that we have a great fit. We can test with other values of k.
pred1.knn <- knn(train = iris.train1, test = iris.test1, 
                 cl = iris.train.labels, k = 5)

# Evaluating model performance once more
CrossTable(x = iris.test.labels, y = pred1.knn, prop.chisq = FALSE)

# One can also try min-max normalisations or z-scores to improve the efficiency
# of the model. A simple function:
normalise <- function(x){
        return(x - min(x) / max(x) - min(x))
}

#### Classification using Decision Trees

# Building the prediction tree with the C5.0 algorithm. Again we'll use n = 90
# in the training set. This time, we include all variables in the first step,
# then remove the dependent variable (column 5) in the C5.0() command.
model.dt <- C5.0(iris.train[-5], iris.train$Species, trials = 1)
summary(model.dt)

# Predicted values 
pred.dt <- predict(model.dt, iris.test)

CrossTable(iris.test$Species, pred.dt,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual species', 'predicted species'))

# Now we will grow a tree using the party package. The function is ctree()
# and it works very well. The graphs are also very intuitive.
model.dt2 <- ctree(Species ~ . , data = iris.train)
plot(model.dt2)

# Predicted values
pred.dt2 <- predict(model.dt2, newdata = iris.test)

# Evaluate the model's predictions
CrossTable(iris.test$Species, pred.dt2,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual species', 'predicted species'))

# One can also grow a tree using the rpart package.
model.dt3 <- train(Species ~ . , method = "rpart", data = iris.train)
print(model.dt3$finalModel)
fancyRpartPlot(model.dt3$finalModel)

# Predicted values
pred.dt3 <- predict(model.dt3, newdata = iris.test)

# Evaluating the model
CrossTable(iris.test$Species, pred.dt3,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual species', 'predicted species'))

##### Random Forests

# Growing a forest (many decision trees). For more information on random forests, please refer to:
# http://en.wikipedia.org/wiki/Random_forest and http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm
model.rf <- randomForest(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                         importance = TRUE, data = iris.train)
print(model.rf)
importance(model.rf) 
plot(model.rf)
varImpPlot(model.rf)

# Predicted values
pred.rf <- predict(model.rf, iris.test)

# Checking the predictions
CrossTable(iris.test$Species, pred.rf,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual species', 'predicted species'))

# Partial dependence plots to check the effect of the X variables on Y
par(mfrow=c(2,2))
partialPlot(model.rf, iris.train, Sepal.Length)
partialPlot(model.rf, iris.train, Sepal.Width)
partialPlot(model.rf, iris.train, Petal.Length)
partialPlot(model.rf, iris.train, Petal.Width)

# Cross-validation tests. A simple explanation for the rfcv() function can be found at: 
# http://stats.stackexchange.com/questions/112556/r-random-forest-need-help-understanding-the-rfcv-function
cv <- rfcv(iris.train[-5], iris.train$Species, step = .9)
cv$error.cv 
with(cv, plot(n.var, error.cv, log = "x", type = "o", lwd = 2)) 


# Evaluating the model. Kappa less than .2 indicates poor fit, from .2 to .4 fair,
# .4 to .6 moderate agreement, .6 to .8 good agreement, .8 to 1 very good. 
# (Machine Learning with R, pp. 303)
confusionMatrix(iris.test$Species, pred.rf, 
                positive = c("Setosa", "Versicolor", "Virginica"))

# Random forests using the party package
model.cf <- cforest(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                    data = iris.train)
varimp(model.cf)
varimp(model.cf, conditional = TRUE) # quite slow
print(model.cf)
pred.cf <- predict(model.cf, iris.test, OOB = TRUE) # don't forget to add OOB=T

CrossTable(iris.test$Species, pred.cf,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual species', 'predicted species'))

##### SMOTE algorithm for unbalanced classification problems

# When there is a strong imbalance between groups (e.g., when something occurs, say, in 1% of the cases)
# the estimation problem is overwhelmed by the majority class. SMOTE is an algorithm that balances data 
# via resampling methods (https://www.jair.org/media/953/live-953-2037-jair.pdf).
# There is a SMOTE() function in the DMwR package. An example from their reference manual:

# Data
data(iris)
data <- iris[, c(1, 2, 5)]
data$Species <- factor(ifelse(data$Species == "setosa","rare","common"))
table(data$Species)

## now using SMOTE to create a more "balanced problem"
newData <- SMOTE(Species ~ ., data, perc.over = 600,perc.under=100)
table(newData$Species)

## Checking visually the created data
par(mfrow = c(1, 2))
plot(data[, 1], data[, 2], pch = 19 + as.integer(data[, 3]),
     main = "Original Data")
plot(newData[, 1], newData[, 2], pch = 19 + as.integer(newData[,3]),
     main = "SMOTE'd Data")

## Now an example where we obtain a model with the "balanced" data
classTree <- SMOTE(Species ~ ., iris, perc.over = 600, perc.under = 100,
                   learner = 'rpartXse', se = 0.5)

## check the resulting classification tree
classTree
## The tree with the unbalanced data set would be
rpartXse(Species ~ ., data, se = 0.5)


##### K-Means Cluster

# Separate a variable of interest in k groups. Here, we divide Sepal.Length in 3 categories.
# First, we use the kmeans() function
sl.cluster <- kmeans(iris$Petal.Width, 3)

sl.cluster$size # number of points in each cluster
sl.cluster$centers # matrix of cluster centres

# One can also use the cut2() function from the Hmisc package. 
sl.cluster1 <- cut2(iris$Petal.Width, g = 3)

table(sl.cluster1)
table(sl.cluster1, iris$Species)
round(prop.table(table(sl.cluster1, iris$Species)), digits = 2)

# Compare the clusters with Petal.Length
qplot(sl.cluster1, Petal.Length, data = iris, fill = sl.cluster1,
            geom = c("boxplot", "jitter"))

# For more information on machine and statistical learning, visit:
# http://cran.r-project.org/web/views/MachineLearning.html
# http://statistical-research.com/a-brief-tour-of-the-trees-and-forests/

