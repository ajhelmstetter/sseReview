require(xgboost)

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test


#Each variable is a list containing two things, label and data:
#label is the outcome of our dataset meaning it is the binary classification we will try to predict.
str(train)

#As seen below, the data are stored in a dgCMatrix which is a sparse matrix and label vector is a numeric vector ({0,1}):

##
#Basic training
##

#We are using the train data. As explained above, both data and label are stored in a list.

#In a sparse matrix, cells containing 0 are not stored in memory. Therefore, in a dataset mainly made of 0, memory size is reduced. It is very common to have such a dataset.
#We will train decision tree model using the following parameters:
#objective = "binary:logistic": we will train a binary classification model ;
#max.depth = 2: the trees won’t be deep, because our case is very simple ;
#nthread = 2: the number of cpu threads we are going to use;
#nrounds = 2: there will be two passes on the data, the second one will enhance the model by further reducing the difference between ground truth and prediction.

bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

#Alternatively, you can put your dataset in a dense matrix, i.e. a basic R matrix.

bstDense <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

#xgb.DMatrix
#XGBoost offers a way to group them in a xgb.DMatrix. You can even add other meta data in it. This will be useful for the most advanced features we will discover later.
dtrain <- xgb.DMatrix(data = train$data, label = train$label)
bstDMatrix <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

#Perform the prediction
#The purpose of the model we have built is to classify new data. As explained before, we will use the test dataset for this step.
pred <- predict(bst, test$data)

#How can we use a regression model to perform a binary classification?
#If we think about the meaning of a regression applied to our data, the numbers we get are probabilities that a datum will be classified as 1. Therefore, we will set the rule that if this probability for a specific datum is > 0.5 then the observation is classified as 1 (or 0 otherwise).

prediction <- as.numeric(pred > 0.5)
print(head(prediction))

#Measuring model performance
#To measure the model performance, we will compute a simple metric, the average error.
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))

#Steps explanation:

#as.numeric(pred > 0.5) applies our rule that when the probability (<=> regression <=> prediction) is > 0.5 the observation is classified as 1 and 0 otherwise ;

#probabilityVectorPreviouslyComputed != test$label computes the vector of error between true data and computed probabilities ;

#mean(vectorOfErrors) computes the average error itself.

#The most important thing to remember is that to do a classification, you just do a regression to the label and then apply a threshold.

#Dataset preparation
#For the following advanced features, we need to put data in xgb.DMatrix as explained above.

dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)

#we use watchlist parameter. It is a list of xgb.DMatrix, each of them tagged with a name.
watchlist <- list(train=dtrain, test=dtest)

bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, objective = "binary:logistic")

#Both training and test error related metrics are very similar, and in some way, it makes sense: what we have learned from the training dataset matches the observations from the test dataset.

#If with your own dataset you do not have such results, you should think about how you divided your dataset in training and test.

#eval.metric allows us to monitor two new metrics for each round, logloss and error.
bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")

#Linear boosting
#Until now, all the learnings we have performed were based on boosting trees. XGBoost implements a second algorithm, based on linear boosting. The only difference with the previous command is booster = "gblinear" parameter (and removing eta parameter).
bst <- xgb.train(data=dtrain, booster = "gblinear", nthread = 2, nrounds=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")

#In simple cases, this will happen because there is nothing better than a linear algorithm to catch a linear link. However, decision trees are much better to catch a non linear link between predictors and outcome.

#Save / Load
#Like saving models, xgb.DMatrix object (which groups both dataset and outcome) can also be saved using xgb.DMatrix.save function.
#xgb.DMatrix.save(dtrain, "dtrain.buffer")

# to load it in, simply call xgb.DMatrix
#dtrain2 <- xgb.DMatrix("dtrain.buffer")

#Information extraction
#Information can be extracted from an xgb.DMatrix using getinfo function. Hereafter we will extract label data.

label = getinfo(dtest, "label")
pred <- predict(bst, dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error=", err))

#View feature importance/influence from the learnt model
#Feature importance is similar to R gbm package’s relative influence (rel.inf).

importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#View the trees from a model
#You can dump the tree you learned using xgb.dump into a text file.
xgb.dump(bst, with_stats = TRUE)

#You can plot the trees from your model using ```xgb.plot.tree``
xgb.plot.tree(model = bst)

# save model to binary local file
xgb.save(bst, "xgboost.model")
