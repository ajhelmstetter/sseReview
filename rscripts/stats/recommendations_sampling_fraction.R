###
# Look at sampling fraction compared to recommendations
###

rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)

#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'sse_model',
    'year',
    'perc_sampling',
    'div_inc'
  )]

#make sure factors are factors
df$sse_model <- as.factor(df$sse_model)
df$div_inc <- as.factor(df$div_inc)

#histogram of global sampling fraction
hist(df$perc_sampling)

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))
head(df2)

#make sampling fraction %
df2$perc_sampling <- df2$perc_sampling * 100

#add a column pre and post 2009
df2$year_threshold<-rep(2,length(df2$study))
df2$year_threshold[(df2$year)<2010]<-1
df2$year_threshold<-as.factor(df2$year_threshold)

#remove rows with NA
df2<-na.omit(df2)

#number models with sampling fraction > 25% pre-2010
sum(df2$perc_sampling[df2$year_threshold==1]>=25)

#number models pre-2010
length(df2$perc_sampling[df2$year_threshold==1])

#Pre-2010 sampling fraction
sum(df2$perc_sampling[df2$year_threshold==1]>=25) / length(df2$perc_sampling[df2$year_threshold==1])

#number models with sampling fraction >= 25% post 2009
sum(df2$perc_sampling[df2$year_threshold==2]>=25)

#number models post 2009
length(df2$perc_sampling[df2$year_threshold==2])

#Post-2009 sampling fraction
sum(df2$perc_sampling[df2$year_threshold==2]>=25) / length(df2$perc_sampling[df2$year_threshold==2])

#overall
#total models with sampling fraction info
length(df2$perc_sampling)

#sampling fraction >=25 across all studies
sum(df2$perc_sampling>=25) / length(df2$perc_sampling)
