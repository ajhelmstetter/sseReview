####
# How do dataset properties compare to recommendations?
####

rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)

#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#check col names
colnames(df)

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'order',
    'sse_model',
    'tips',
    'year',
    'div_inc',
    'perc_sampling',
    'samples_per_state'
  )]

#make sampling fraction percentage
df$perc_sampling<-df$perc_sampling*100

#make sure factors are factors
df$order <- as.factor(df$order)
df$sse_model <- as.factor(df$sse_model)
df$year <- as.factor(df$year)
df$div_inc <- as.factor(df$div_inc)

###
# Make samples per state into tip bias
###

#get max and min values of samples per state for each model
top_df <-
  df %>% group_by(study, model_no) %>% slice_max(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)

head(top_df)

bot_df <-
  df %>% group_by(study, model_no) %>% slice_min(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)

head(bot_df)

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))
head(df2)

#add tip bias column by dividing larger number of tips with state A by smaller number of tips with state B
#multi-state models are therefore largest tip bias possible in the data
df2$tip_bias <- top_df$samples_per_state / bot_df$samples_per_state

#check that orders of df2 and top/bot_df match up
top_df$study==bot_df$study
top_df$model_no==bot_df$model_no

df2$model_no==bot_df$model_no
df2$model_no==top_df$model_no

df2$study==bot_df$study
df2$study==top_df$study

#remove na from tip bias column only
df2<-df2[!is.na(df2$tip_bias),]

#make new col with 2's
df2$davis<-rep(2,length(df2$study))

#split studies by year of publication Davis et al. 2013
#1 = pre DEL, 2 = post DEL
df2$davis[as.numeric(as.character(df2$year))<2014]<-1
df2$davis<-as.factor(df2$davis)

#remove inf cols
df2<-df2[df2$tip_bias<1000000,]
hist(df2$tip_bias)

#number of models with <1:10 tip ratio post-davis
sum(df2[df2$davis==2,]$tip_bias<=10)

#number of models post-davis
length(df2[df2$davis==2,]$tip_bias)

#proportion of models with <1:10 tip ratio post-davis
sum(df2[df2$davis==2,]$tip_bias<=10) / length(df2[df2$davis==2,]$tip_bias)

#number of models with <1:10 tip ratio pre-davis
sum(df2[df2$davis==1,]$tip_bias<=10)

#number of models pre-davis
length(df2[df2$davis==1,]$tip_bias)

#proportion of models with <1:10 tip ratio pre-davis
(length(df2[df2$davis==1,]$tip_bias) - sum(df2[df2$davis==1,]$tip_bias>=10)) / length(df2[df2$davis==1,]$tip_bias)

#number of models where these data were available
w_samp<-unique(df2$study)
w_samp

###
# Studies that meet all three
###

#remove studies with any NA
df2<-na.omit(df2)

#Number of models with all info
unique(df2$study)

#sampling greater than 50
df2<-df2[df2$perc_sampling>=25,]

#more than 300 tips
df2<-df2[df2$tips>=300,]

#less than 10 tip bias
df2<-df2[df2$tip_bias<=10,]

#studies that pass strict filtering
unique(df2$study)
