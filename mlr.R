#################################################
##### Machine Learning with the mlr package #####
#################################################

### load packages
require(mlr)         # models
require(parallelMap) # parallelisation
require(Zelig)       # data

parallelStartSocket(2) # parallelisation: 2 cores

## first, a binary classification problem

# some data processing before we start
data("voteincome")
voteincome$vote <- as.factor(voteincome$vote)
voteincome$income <- as.numeric(voteincome$income)
voteincome$education <- as.numeric(voteincome$education)
voteincome$age <- as.numeric(voteincome$age)
voteincome$female <- as.numeric(voteincome$female)
voteincome$year <- NULL

# create a classifier
set.seed(0)
task.class <- makeClassifTask(data = voteincome, target = "vote", positive = 1) 
lrnr.class <- makeLearner("classif.randomForest", predict.type = "prob")
rdesc.class <- makeResampleDesc("Holdout", split = 0.75) # 75% train, 25% test data
res.class <- resample(learner = lrnr.class, task = task.class, resampling = rdesc.class)

# get predictions on test set
table(getPredictionResponse(res.class$pred))

# compute accuracy, also see https://mlr-org.github.io/mlr-tutorial/devel/html/performance/index.html
performance(res.class$pred, acc)

# roc curve
roc.class <- generateROCRCurvesData(res.class$pred, meas1 = "tpr", meas2 = "fpr")
plotROCRCurves(roc.class) + ggtitle("ROC Curve")

# plot false negative and positive rates as well as the error rate versus the threshold
d.class <- generateThreshVsPerfData(res.class$pred, measures = list(fpr, fnr, mmce))
plotThreshVsPerf(d.class)

# feature importance
fv.class <- generateFilterValuesData(task.class, method = "rf.importance")
fv.class
plotFilterValues(fv.class) + ggtitle("Variable Importance")

# partial prediction
fit.class <- train(lrnr.class, task.class)
pd.class <- generatePartialPredictionData(fit.class, task.class,
                                          c("income", "age", "education", "female"))
plotPartialPrediction(pd.class) + ggtitle("Partial Probabilities")

# interactions
pd2.class <- generatePartialPredictionData(fit.class, task.class,
                                           c("income", "education"),
                                           interaction = TRUE)
plotPartialPrediction(pd2.class, facet = "education") + ggtitle("Partial Probabilities")

## regression problem
data("swiss")

# create a learner
set.seed(101)
task.reg <- makeRegrTask(data = swiss, target = "Infant.Mortality") 
lrnr.reg <- makeLearner("regr.randomForest")
rdesc.reg <- makeResampleDesc("CV", iters = 5) # cross-validation
res.reg <- resample(learner = lrnr.reg, task = task.reg, resampling = rdesc.reg)

# performance: mean squared error, median squared error and mean absolute error
performance(res.reg$pred, measures = list(mse, medse, mae))

# feature importance
fv.reg <- generateFilterValuesData(task.reg)
fv.reg
plotFilterValues(fv.reg) + ggtitle("Variable Importance")

# partial prediction with confidence interval
fit.reg <- train(lrnr.reg, task.reg)
pd.reg <- generatePartialPredictionData(fit.reg, task.reg,
                                        c("Fertility", "Examination"),
                                        fun = function(x) quantile(x, c(.25, .5, .75)))
plotPartialPrediction(pd.reg) + ggtitle("Partial Probabilities")

# partial prediction for each unit
fit.reg <- train(lrnr.reg, task.reg)
pd.reg <- generatePartialPredictionData(fit.reg, task.reg,
                                        c("Fertility", "Examination"),
                                        individual = TRUE)
plotPartialPrediction(pd.reg) + ggtitle("Partial Probabilities")

# interactions
pd2.reg <- generatePartialPredictionData(fit.reg, task.reg,
                                         c("Fertility", "Examination"),
                                         interaction = TRUE)
plotPartialPrediction(pd2.reg, facet = "Examination") + ggtitle("Partial Probabilities")

# individual derivatives
pd3.reg <- generatePartialPredictionData(fit.reg, task.reg,
                                         "Examination",
                                         individual = TRUE,
                                         derivative = TRUE)
plotPartialPrediction(pd3.reg) + ggtitle("Partial Probabilities")

parallelStop() # stop parallelisation
