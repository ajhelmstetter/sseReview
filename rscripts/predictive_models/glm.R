rm(list = ls())

#library(ISLR)

#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#only keep columns we want in the model
df <-
  df[, c(
    'study',
    'model_no',
    'order',
    'trait_type_1',
    'sse_model',
    'tips',
    'year',
    'no_markers',
    'age',
    'age_inferred',
    'div_inc',
    'no_plastid',
    'no_mito',
    'no_nuclear',
    'perc_sampling',
    'samples_per_state'
  )]

###
# Data transformations
###

# NUMBER OF MARKERS
#log no_markers as long tail
hist(df$no_markers)
df$no_markers <- log(df$no_markers)
hist(df$no_markers)

#NUMBER OF TIPS
#log tips as long tail
hist(df$tips)
df$tips <- log(df$tips)
hist(df$tips)

#AGE OF TREE
#log age as long tail
#still strange
hist(df$age)
df$age <- log(df$age)
hist(df$age)

#GLOBAL SAMPLING FRACTION
hist(df$perc_sampling)

#arcsine
df$perc_sampling<-asin(sqrt(df$perc_sampling))
hist(df$perc_sampling)

#SAMPLES PER STATE
#change to numeric prior to transformation, remove NAs
df$samples_per_state <- as.numeric(df$samples_per_state)

#fix one study (Sabath et al.) with 0
df$samples_per_state[df$samples_per_state==0] <- NA

#log transform
hist(df$samples_per_state)
df$samples_per_state<-log(df$samples_per_state)
hist(df$samples_per_state)

df$samples_per_state[is.na(df$samples_per_state)] <- 0

#remove quasse for div_inc
#may need to remove others too
df<-df[df$sse_model!="QuaSSE",]

####
#reformat data
####

#factors
df$order <- as.factor(df$order)
df$trait_type <- as.factor(df$trait_type)
df$sse_model <- as.factor(df$sse_model)
df$year <- as.character(df$year)

####
# Check correlation of continuous vars
####

df_cor <-
  df[, c(
    'tips',
    'no_markers',
    'age',
    'no_plastid',
    'no_mito',
    'no_nuclear',
    'perc_sampling',
    'samples_per_state'
  )]

cor(df_cor)

###
# Make samples per state into tip bias
###

df$samples_per_state

#make column with combination of study and model
tmp_df <- df %>% tidyr::unite("study_model", 1:2, remove = T)

#reduce dataset to two columns
tmp_df <- tmp_df[, c("study_model", "samples_per_state")]

head(tmp_df)

#get max and min values of samples per state for each model
top_df <-
  tmp_df %>% group_by(study_model) %>% slice_max(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)
bot_df <-
  tmp_df %>% group_by(study_model) %>% slice_min(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)


#reduce to a single binary result per model per study
df3 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#make binary trait factor
df3$div_inc <- as.factor(df3$div_inc)

#make sampling fraction %
df3$perc_sampling <- df3$perc_sampling * 100

#add tip bias column by dividing larger number of tips with state A by smaller number of tips with state B
#multi-state models are therefore largest tip bias possible in the data
df3$tip_bias <- top_df$samples_per_state / bot_df$samples_per_state

#ERR: infinite values
#QUICK FIX: make NaN
df3$tip_bias[is.infinite(df3$tip_bias)]<-NaN

#convert back to df
df <- as.data.frame(df3)

#remove study and model no (first two rows)
df <- df[, -which(colnames(df)%in%c('study',
                                    'model_no',
                                    'samples_per_state'))]

#ensure age inferred categorical
df$age_inferred <- ifelse(df$age_inferred == 1, 'yes', 'no')

##missing data causes predict to give reduced number
df[is.na(df)] <- 0

# Logistic Regression
glm.fit <-
  glm(
    div_inc ~ order + trait_type + sse_model + tips + year + no_markers + age + age_inferred + no_plastid + no_mito + no_nuclear + perc_sampling + tip_bias,
    data = df,
    family = binomial
  )

#ERR: glm.fit: fitted probabilities numerically 0 or 1 occurred
#DUE TO: variables like sse_model
#ERR: there is some issue with QuaSSE - something about it only measuring speciation so div results dont make sense

#Estimate, standard errors, z-score, and p-values on each of the coefficients.
#It also gives you the null deviance (the deviance just for the mean) and the residual deviance (the deviance for the model with all the predictors).
summary(glm.fit)

#You assign the result of predict() of glm.fit() to glm.probs, with type equals to response.
#This will make predictions on the training data that you use to fit the model and give me a vector of fitted probabilities.
glm.probs <- predict(glm.fit, type = "response")

#Turn prediction probabilities into classifications by thresholding at 0.5.
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)

#Make a table of predictions vs results
#Matches = correct prediction
table(glm.pred, df$div_inc)

#Calculate the average chance to get prediction correct
mean(glm.pred == df$div_inc)

#Perform stepwise model selection and repeat
glm.fit2 <- step(glm.fit)
summary(glm.fit2)

glm.probs2 <- predict(glm.fit2, type = "response")
glm.pred2 <- ifelse(glm.probs2 > 0.5, 1, 0)
table(glm.pred2, df$div_inc)
mean(glm.pred2 == df$div_inc)


