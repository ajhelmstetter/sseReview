rm(list=ls())

####
# Binary outcome modelling
####

#FIND AND REPLACE ext_inc for div_inc etc (except first 2 instances)
#COULD INCLUDE ext_inc in sp_inc model, or not - bit circular

library(caret)

#https://amunategui.github.io/binary-outcome-modeling/

df <- read.csv("~/Downloads/sse_review_table - main_table.csv")
df$year<-as.character(df$year)
#It is customary to have the outcome variable (also known as response variable)
#located in the last column of a data set
#df<-df[c('study','model_no','order','trait_type','sse_model','tips','sp_inc','ext_inc',"div_inc")]
#df<-df[c('study','model_no','order','trait_type','sse_model','tips','year','no_markers',"div_inc")]

#we have to fix the factor variables as most models only accept numeric data
#gbm can deal with factor variables as it will dummify them internally, but glmnet won’t.
#dummifying factors breaks all the unique values into separate columns

df$order <- as.factor(df$order)
df$trait_type <- as.factor(df$trait_type)
df$sse_model <- as.factor(df$sse_model)

#reduce to a single binary result per model per study
library(dplyr)

df3 <- df %>%
  group_by(study, model_no) %>%
  slice(which.max(div_inc))

#convert back to df
df<-as.data.frame(df3)

#remove study and model no
#df<-df[c('order','trait_type','sse_model','tips','sp_inc','ext_inc')]
df<-df[c('order','trait_type','sse_model','tips','year','no_markers',"div_inc")]

#reformat data frame with dummy variables
dfDum <- dummyVars("~.",data=df, fullRank=F)
df2 <- as.data.frame(predict(dfDum,df))

#each unique factor is now separated into it’s own column
print(names(df2))

#remove near zero variance predictors
#nearZeroVar() not only removes predictors that have one unique value across samples (zero variance predictors)
#but also removes predictors that have both 1) few unique values relative to the number of samples
#2) large ratio of the frequency of the most common value to the frequency of the second most common value (near-zero variance predictors).
x = nearZeroVar(df2, saveMetrics = TRUE)
str(x, vec.len=2)

#and which ones are the near-zero variance predictors
rm_NZV<-row.names(x[x[,"zeroVar"] + x[,"nzv"] > 0, ])

#remove nearZeroVar columns
df2<-df2[,!grepl(paste(rm_NZV, collapse="|"),colnames(df2))]

#proportion of our outcome variable
prop.table(table(df2$div_inc))

#This is an important step because if the proportion was smaller than 15%
# it would be considered a rare event and would be more challenging to model.

#generalize variables to easily recycle the code for subsequent needs
outcomeName <- 'div_inc'
predictorsNames <- names(df2)[names(df2) != outcomeName]

#It is important to know what type of modeling a particular model supports. This can be done using the caret function getModelInfo:
getModelInfo()$gbm$type

#This tells us that gbm supports both regression and classification.
#As this is a binary classification, we need to force gbm into using the classification mode.
#We do this by changing the outcome variable to a factor
#(we use a copy of the outcome as we’ll need the original one for our next model):

df2$div_inc2 <- ifelse(df2$div_inc==1,'yes','no')
df2$div_inc2 <- as.factor(df2$div_inc2)
outcomeName <- 'div_inc2'

#set seed
set.seed(1234)

#create test and training data
splitIndex <- createDataPartition(df2[,outcomeName], p = .75, list = FALSE, times = 1)
trainDF <- df2[ splitIndex,]
testDF  <- df2[-splitIndex,]

#Caret offers many tuning functions to help you get as much as possible out of your models;
#the trainControl function allows you to control the resampling of your data.
#This will split the training data set internally
#and do it’s own train/test runs to figure out the best settings for your model.

#cross-validate the data 3 times, therefore training it 3 times on different portions
#of the data before settling on the best tuning parameters
#(for gbm it is trees, shrinkage, and interaction depth)

objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

#teach model how to recognise diversification increases
#Because this is a classification model, we’re requesting that our metrics use ROC instead of the default RMSE

objModel <- train(trainDF[,predictorsNames], trainDF[,outcomeName],
                  method='gbm',
                  trControl=objControl,
                  metric = "ROC",
                  preProc = c("center", "scale"))

#which vars were most important?
var_imp<-summary(objModel)
var_imp2<-var_imp$rel.inf
names(var_imp2)<-var_imp$var

par(mar=c(10,5,3,3))
barplot(var_imp2,horiz=F,las=2,
        ylab="relative influence of variable",cex.names = 0.7,
        main="Predicting Binary Outcomes using GBM: div_inc in -SSE models",cex.main=0.9)

#what tuning parameters were most important to the model?
print(objModel)

####
#MODEL EVALUATION
####

#prob gives you the probability on how sure the model is about it’s choice
#can be used to calculate AUC score

#We now call the predict function and pass it our trained model and our testing data. Let’s start by looking at class predictions and using the caret postResample function to get an accuracy score:
predictions <- predict(object=objModel, testDF[,predictorsNames], type='raw')
head(predictions)

#accuracy = how often model is correct?
acc<-print(postResample(pred=predictions, obs=as.factor(testDF[,outcomeName])))
acc

#Now let’s look at probabilities:

#probabilites
library(pROC)
predictions <- predict(object=objModel, testDF[,predictorsNames], type='prob')
head(predictions)

#To get the AUC score, you need to pass the yes column to the roc function
#(each row adds up to 1 but we’re interested in the yes, the survivors):
auc <- roc(ifelse(testDF[,outcomeName]=="yes",1,0), predictions[[2]])
print(auc$auc)

#remember that an AUC ranges between 0.5 and 1, where 0.5 is random and 1 is perfect
legend('topright',legend=c(paste("accuracy =",round(acc[1],4)),
                           paste("AUC =",round(auc$auc,4))))

par(mfrow=c(1,3))

#barplots of no. tips vs div inc
boxplot(log(tips)~div_inc,data=df, xlab="div_inc inferred?", ylab="Log no. tips in tree", outline=FALSE,col=2)
t.test(log(tips)~div_inc,data=df)
wilcox.test(tips~div_inc,data=df, alternative = "two.sided")

#barplots of no. tips vs sp inc
boxplot(log(tips)~sp_inc,data=df, xlab="sp_inc inferred?", ylab="Log no. tips in tree", outline=FALSE,col=3)
t.test(log(tips)~sp_inc,data=df)
wilcox.test(tips~sp_inc,data=df, alternative = "two.sided")

#barplots of no. tips vs ext inc
boxplot(log(tips)~ext_inc,data=df, xlab="ext_inc inferred?", ylab="Log no. tips in tree", outline=FALSE,col=4)
t.test(log(tips)~ext_inc,data=df)
wilcox.test(tips~ext_inc,data=df, alternative = "two.sided")
