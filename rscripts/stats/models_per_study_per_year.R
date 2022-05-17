
# Library
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(ggplot2)

#read in full data frame
df <-
  read.csv("~/Dropbox/projects/AJH_DiveRS/sseReview/data/sse_review_table - main_table.csv")

df_tl1 <- unique(df[c("study","sse_model","model_no","trait_level_1")])
head(df_tl1)

#total number of extrinsic traits
table(df_tl1$trait_level_1)[2]

#total number of intrinsic traits
table(df_tl1$trait_level_1)[4]

#Total number of trait_level_1 types analysed
sum(table(df_tl1$trait_level_1))

#proportion extrinsic
table(df_tl1$trait_level_1)[2]/sum(table(df_tl1$trait_level_1))

#proportion extrinsic
table(df_tl1$trait_level_1)[4]/sum(table(df_tl1$trait_level_1))
