###
# Are putative ancestral states linked to trait-based div?
###

#Library
library(tidyverse)
library(ggplot2)
library(treemapify)
library(RColorBrewer)
library(ggrepel)
library(dplyr)

#read in full data frame
df <-
  read.csv("~/Dropbox/projects/AJH_DiveRS/sseReview/data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'sse_model',
    'model_no',
    'trait_level_1',
    'trait_level_2',
    'trait_level_3',
    'trait_level_4',
    'trait_level_5',
    'trait_level_6',
    'div_inc'
  )]

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#make > 1 MuSSE div_inc values 1
df2$div_inc[df2$div_inc>1]<-1

#df with only intrinsic traits
df2_int<-df2[df2$trait_level_1=="Intrinsic",]

#number of times intrinsic is associated with trait dependent diversification
table(df2_int$div_inc)

#proportion of models with intrinsic traits assoc with div_inc
table(df2_int$div_inc)[2] / (table(df2_int$div_inc)[1] + table(df2_int$div_inc)[2])

#df with only extrinsic traits
df2_ext<-df2[df2$trait_level_1=="Extrinsic",]

#number of times extrinsic is associated with trait dependent diversification
table(df2_ext$div_inc)

#proportion of models with extrinsic traits assoc with div_inc
table(df2_ext$div_inc)[2] / (table(df2_ext$div_inc)[1] + table(df2_ext$div_inc)[2])
