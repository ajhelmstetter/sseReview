rm(list=ls())

require(ISLR)

df <- read.csv("~/Downloads/sse_review_table - main_table.csv")

df<-df[,c('study','model_no','order','trait_type','sse_model','tips','year','no_markers','age','age_inferred',"div_inc")]

###
# Data cleaning
###

#log no_markers as long tail
hist(df$no_markers)
df$no_markers<-log(df$no_markers)
hist(df$no_markers)

#log tips as long tail
hist(df$tips)
df$tips<-log(df$tips)
hist(df$tips)

#log age as long tail
hist(df$age)
df$age<-log(df$age)
hist(df$age)

#remove quasse for div_inc
#will need to remove others too
#df<-df[df$sse_model!="QuaSSE",]

#Let's take a look at the density distribution of each variable broken down by Direction value. Like the scatterplot matrix above, the density plot by Direction can help see the separation of Up and Down. It can also help to understand the overlap in Direction values for a variable.
library(caret)
x <- df[,c('tips','year','no_markers','age')]

df$div_inc2 <- ifelse(df$div_inc==1,'yes','no')
y <- as.factor(df$div_inc2)

scales <- list(x=list(relation="free"), y=list(relation="free"))

#skewed dist
featurePlot(x=x, y=y, plot="box",scales=scales,col=c(1,2))
featurePlot(x=x, y=y, plot="density",scales=scales,col=c(1,2))
legend("topright",legend=c("no_change","div_inc"),col=c(1,2))

##reformat data

#factors
df$order <- as.factor(df$order)
df$trait_type <- as.factor(df$trait_type)
df$sse_model <- as.factor(df$sse_model)
df$year<-as.character(df$year)

#reduce to a single binary result per model per study
library(dplyr)

#need to specify dplyr
df3 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#convert back to df
df<-as.data.frame(df3)

#ensure age inferred categorical
df$df$age_inferred <- ifelse(df$age_inferred==1,'yes','no')

#Now you call glm.fit() function. The first ar gument that you pass to this function is an R formula. In this case, the formula indicates that Direction is the response, while the Lag and Volume variables are the predictors. As you saw in the introduction, glm is generally used to fit generalized linear models.

#However, in this case, you need to make it clear that you want to fit a logistic regression model. You resolve this by setting the family argument to binomial. This way, you tell glm() to put fit a logistic regression model instead of one of the many other models that can be fit to the glm.

##missing data causes predict to give reduced number
df[is.na(df)] <- 0

# Logistic Regression
glm.fit <- glm(div_inc ~ order + trait_type + sse_model + tips + year + no_markers + age + age_inferred, data = df, family = binomial)

#ERR: glm.fit: fitted probabilities numerically 0 or 1 occurred
#DUE TO sse_model

#ERR: there is some issue with QuaSSE - something about it only measuring speciation so div results dont make sense

#As you can see, summary() returns the estimate, standard errors, z-score, and p-values on each of the coefficients. It also gives you the null deviance (the deviance just for the mean) and the residual deviance (the deviance for the model with all the predictors).
summary(glm.fit)

#You assign the result of predict() of glm.fit() to glm.probs, with type equals to response. This will make predictions on the training data that you use to fit the model and give me a vector of fitted probabilities.
glm.probs <- predict(glm.fit,type = "response")

#Now I am going to make a prediction of whether the market will be up or down based on the lags and other predictors. In particular, I'll turn the probabilities into classifications by thresholding at 0.5. In order to do so, I use an ifelse() command.
glm.pred <- ifelse(glm.probs > 0.5, "yes", "no")

#glm.pred is a vector of trues and falses. If glm.probs is bigger than 0.5, glm.pred calls "Up"; otherwise, it calls "False".

attach(df)
#Here, you attach the data frame Smarket and make a table of glm.pred, which is the ups and downs from the previous direction. You also take the mean of those

table(glm.pred,div_inc)

#From the table, instances on the diagonals are where you get the correct classification, and off the diagonals are where you make mistake. Looks like you made a lot of mistakes. The mean gives a proportion of 0.52.
mean(glm.pred == div_inc2)

#Model selection
glm.fit2 = step(glm.fit)

summary(glm.fit2)

