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

#change to numeric prior to transformation, remove NAs
df$samples_per_state <- as.numeric(df$samples_per_state)

#make sure factors are factors
df$order <- as.factor(df$order)
df$sse_model <- as.factor(df$sse_model)
df$year <- as.factor(df$year)
df$div_inc <- as.factor(df2$div_inc)

#make samples per state into tip bias
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
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))
head(df2)

#add tip bias column by dividing larger number of tips with state A by smaller number of tips with state B
#multi-state models are therefore largest tip bias possible in the data
df2$tip_bias <- top_df$samples_per_state / bot_df$samples_per_state

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

#number of studies with <1:10 tip ratio post-davis
sum(df2[df2$davis==2,]$tip_bias<10)

#number of studies post-davis
length(df2[df2$davis==2,]$tip_bias)

#proportion of studies with <1:10 tip ratio post-davis
sum(df2[df2$davis==2,]$tip_bias<10) / length(df2[df2$davis==2,]$tip_bias)

#number of studies with <1:10 tip ratio pre-davis
sum(df2[df2$davis==1,]$tip_bias<10)

#number of studies pre-davis
length(df2[df2$davis==1,]$tip_bias)

#proportion of studies with <1:10 tip ratio pre-davis
(length(df2[df2$davis==1,]$tip_bias) - sum(df2[df2$davis==1,]$tip_bias>=10)) / length(df2[df2$davis==1,]$tip_bias)

#number of studies where these data were available
w_samp<-unique(df2$study)


###
# Studies that meet all three
###

#remove studies with any NA
df2<-na.omit(df2)

#Number of studies with all info
unique(df2$study)

#sampling greater than 50
df2<-df2[df2$perc_sampling>=25,]

#more than 300 tips
df2<-df2[df2$tips>=300,]

#less than 10 tip bias
df2<-df2[df2$tip_bias<=10,]

#studies that pass
unique(df2$study)
