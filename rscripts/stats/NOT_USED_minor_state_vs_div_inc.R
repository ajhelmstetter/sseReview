rm(list = ls())

library(xgboost)
library(tidyr)
library(dplyr)
library(ggplot2)

#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#choose the columns you want
df <-
  df[, c(
    'study',
    'model_no',
    'sse_model',
    'samples_per_state',
    'div_inc'
  )]

###
# Make samples per state into tip bias
###

#remove quasse as only one state
df <- df[df$sse_model != "QuaSSE",]

#make column with combination of study and model
tmp_df <- df %>% tidyr::unite("study_model", 1:2, remove = T)
head(tmp_df)

#get only those models where div_inc was detected
tmp_df <- tmp_df[tmp_df$study_model%in%tmp_df[tmp_df$div_inc==1,]$study_model,]

#get row of state with lowest tip freq
bot_df <- tmp_df %>% group_by(study_model) %>% slice_min(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)

#number of times rare state associated with div_inc
table(bot_df$div_inc==1)

top_df <-
  tmp_df %>% group_by(study_model) %>% slice_max(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)

#number of times rare state associated with div_inc
table(top_df$div_inc==1)
