###
# Hypothesis vs clade-based analyses
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
    'clade',
    'trait_level_1',
    'trait_level_2',
    'trait_level_4',
    'trait_level_6',
    'character_state'
  )]

#unique combinations of study, model number and clade
#i.e. how many models per study run on each clade
df_uni<- unique(df[c("study", "model_no","clade")])

#get frequency per clade per study
freq_per_clade <-df_uni %>%
  dplyr::count(study, clade, .drop = F)
head(freq_per_clade)

#get number of clades per study
clades_per_study <- freq_per_clade %>%
  dplyr::count(study, .drop = F)
head(clades_per_study)

#names of studies with one clade
studies_one_clade<-clades_per_study[clades_per_study$n==1,]$study

#number of studies on single clades
length(unique(studies_one_clade))

#total number of studies
length(unique(df$study))

#percentage of total studies on one clade
length(unique(studies_one_clade))/length(unique(df$study))

#names of studies with multiple clades
studies_multiple_clades<-clades_per_study[clades_per_study$n>1,]$study

#number of studies on multiple clades
length(studies_multiple_clades)

#####
# studies that look at a single clade
#####

#unique combinations of study, model number and clade
#i.e. how many models per study run on each clade
df_uni<- unique(df[c("study", "clade")])
length(table(df_uni$study)[table(df_uni$study)==1])

#####################################
# Studies that look at a single trait_level_6
#####################################

#unique combinations of study and trait level 1
#i.e. how many studies looked at different traits
df_uni<- unique(df[c("study", "trait_level_6")])

#number of studies that examined differents traits belonging to more than one trait_level_1 category
length(table(df_uni$study)[table(df_uni$study)==1])

#####################################
# Studies that look at different traits
#####################################

#unique combinations of study and trait level 1
#i.e. how many studies looked at different traits
df_uni<- unique(df[c("study", "trait_level_1")])

#number of studies that examined differents traits belonging to more than one trait_level_1 category
length(table(df_uni$study)[table(df_uni$study)>1])

####
# Single clade studies only
####

#reduce original data to only studies on one clade
df2<-df[df$study%in%studies_one_clade,]
length(unique(df2$study))==length(unique(studies_one_clade))

####
# Trait level 6
####

#unique combinations for trait level 6
df_uni6<- unique(df2[c("study","model_no","clade", "trait_level_6")])
head(df_uni6)

#number of times each study looked at trait cat
tmp <-df_uni6 %>%
  dplyr::count(study, trait_level_6,.drop = F)
head(tmp)

#number of traits level 6 tested in each one clade study
traits_per_clade6 <-tmp %>%
  dplyr::count(study,.drop = F)
head(traits_per_clade6)

#number of studies that look at more than one trait (level6) in a single clade
hist(traits_per_clade6$n)
length(traits_per_clade6[traits_per_clade6$n>1,]$study)

####
# Trait level 4
####

#unique combinations for trait level 4
df_uni4<- unique(df2[c("study","model_no","clade", "trait_level_4")])

#number of times each study looked at trait cat
tmp <-df_uni4 %>%
  dplyr::count(study, trait_level_4,.drop = F)

#number of traits level 4 tested in one clade studies
traits_per_clade4 <-tmp %>%
  dplyr::count(study,.drop = F)

#number of studies that look at more than one trait (level2) in a single clade
hist(traits_per_clade4$n)
length(traits_per_clade4[traits_per_clade4$n>1,]$study)

####
# Trait level 2
####

#unique combinations for trait level 2
df_uni2<- unique(df2[c("study","model_no","clade", "trait_level_2")])

#number of times each study looked at trait cat
tmp <-df_uni2 %>%
  dplyr::count(study, trait_level_2,.drop = F)

#number of traits level 2 tested in one clade studies
traits_per_clade2 <-tmp %>%
  dplyr::count(study,.drop = F)

#number of studies that look at more than one trait (level2) in a single clade
hist(traits_per_clade2$n)
length(traits_per_clade2[traits_per_clade2$n>1,]$study)

#percentage of total studies look at more than one trait_level_2 in a clade
length(traits_per_clade2[traits_per_clade2$n>1,]$study)/length(unique(df$study))
traits_per_clade2[traits_per_clade2$n>1,]$study

#how many of these included biogeography
table(tmp[tmp$study%in%traits_per_clade2[traits_per_clade2$n>1,]$study,]$trait_level_2)
