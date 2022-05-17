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
    'putative_ancestral_state',
    'div_inc'
  )]

#reduce to a single binary result per model per study
df_root_inc <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))
head(df_root_inc)

#make multi-state analyses = 1
df_root$div_inc[df_root$div_inc > 1] <- 1

#only effect
df_root_inc<-df_root_inc[df_root_inc$div_inc==1,]

#frequencies of influential state that are (0) derived or (1) ancestral
table(df_root_inc$putative_ancestral_state)

#proportion of div_inc that were derived
table(df_root_inc$putative_ancestral_state)[1]/sum(table(df_root_inc$putative_ancestral_state))
