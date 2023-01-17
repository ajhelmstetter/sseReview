rm(list = ls())

#####
# Proportion of BiSSE models that are associated with div inc
#####

#Library
library(tidyverse)

#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#subset to HiSSE only
df2<-df2[df2$sse_model=="HiSSE",]

#subset to desired trait only
df2<-df2[df2$trait_level_4=="FlowerMorpho",]

table(df2$div_inc)
table(df2$div_inc)[2] / (table(df2$div_inc)[1] + table(df2$div_inc)[2])

df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

df2<-df2[df2$sse_model=="BiSSE",]
df2<-df2[df2$trait_level_4=="FlowerMorpho",]
table(df2$div_inc)
table(df2$div_inc)[2] / (table(df2$div_inc)[1] + table(df2$div_inc)[2])

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

df2<-df2[df2$sse_model=="HiSSE",]
df2<-df2[df2$trait_level_4=="BreedingSystem",]
table(df2$div_inc)
table(df2$div_inc)[2] / (table(df2$div_inc)[1] + table(df2$div_inc)[2])

df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

df2<-df2[df2$sse_model=="BiSSE",]
df2<-df2[df2$trait_level_4=="BreedingSystem",]
table(df2$div_inc)
table(df2$div_inc)[2] / (table(df2$div_inc)[1] + table(df2$div_inc)[2])

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

df2<-df2[df2$sse_model=="HiSSE",]
df2<-df2[df2$trait_level_4=="LifeForm",]
table(df2$div_inc)
table(df2$div_inc)[2] / (table(df2$div_inc)[1] + table(df2$div_inc)[2])

df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

df2<-df2[df2$sse_model=="BiSSE",]
df2<-df2[df2$trait_level_4=="LifeForm",]
table(df2$div_inc)
table(df2$div_inc)[2] / (table(df2$div_inc)[1] + table(df2$div_inc)[2])


