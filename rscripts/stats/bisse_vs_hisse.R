rm(list = ls())
library(tidyr)
library(dplyr)

#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#####
# Proportion of BiSSE models that are associated with div inc
#####

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#df with only intrinsic traits
df2_int<-df2[df2$sse_model=="BiSSE",]

#number of times intrinsic is associated with trait dependent diversification
table(df2_int$div_inc)

#proportion of models with intrinsic traits assoc with div_inc
table(df2_int$div_inc)[2] / (table(df2_int$div_inc)[1] + table(df2_int$div_inc)[2])


#####
# Proportion of HiSSE models that are associated with div inc
#####

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#df with only intrinsic traits
df2_int<-df2[df2$sse_model=="HiSSE",]

#number of times intrinsic is associated with trait dependent diversification
table(df2_int$div_inc)

#proportion of models with intrinsic traits assoc with div_inc
table(df2_int$div_inc)[2] / (table(df2_int$div_inc)[1] + table(df2_int$div_inc)[2])


#####
# Proportion of BiSSE models with > 100 tips that are associated with div inc
#####

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#only models with > 100 tips
df2 <- df2[df2$tips>100,]

#df with only intrinsic traits
df2_int<-df2[df2$sse_model=="BiSSE",]

#number of times intrinsic is associated with trait dependent diversification
table(df2_int$div_inc)

#proportion of models with intrinsic traits assoc with div_inc
table(df2_int$div_inc)[2] / (table(df2_int$div_inc)[1] + table(df2_int$div_inc)[2])

#####
# Proportion of HiSSE models with > 100 tips that are associated with div inc
#####

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#only models with > 100 tips
df2 <- df2[df2$tips>100,]

#df with only intrinsic traits
df2_int<-df2[df2$sse_model=="HiSSE",]

#number of times intrinsic is associated with trait dependent diversification
table(df2_int$div_inc)

#proportion of models with intrinsic traits assoc with div_inc
table(df2_int$div_inc)[2] / (table(df2_int$div_inc)[1] + table(df2_int$div_inc)[2])


#####
# Proportion of BiSSE models with > 300 tips that are associated with div inc
#####

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#only models with > 300 tips
df2 <- df2[df2$tips>300,]

#df with only intrinsic traits
df2_int<-df2[df2$sse_model=="BiSSE",]

#number of times intrinsic is associated with trait dependent diversification
table(df2_int$div_inc)

#proportion of models with intrinsic traits assoc with div_inc
table(df2_int$div_inc)[2] / (table(df2_int$div_inc)[1] + table(df2_int$div_inc)[2])

#####
# Proportion of HiSSE models with > 300 tips that are associated with div inc
#####

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#only models with > 300 tips
df2 <- df2[df2$tips>300,]

#df with only intrinsic traits
df2_int<-df2[df2$sse_model=="HiSSE",]

#number of times intrinsic is associated with trait dependent diversification
table(df2_int$div_inc)

#proportion of models with intrinsic traits assoc with div_inc
table(df2_int$div_inc)[2] / (table(df2_int$div_inc)[1] + table(df2_int$div_inc)[2])

###
# BiSSE only trait level 4
###

#Library
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(patchwork)
library(grid)

#read in full data frame
df <-
  read.csv("data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'sse_model',
    'trait_level_4',
    'div_inc'
  )]

# Subset to only BiSSE
df<-df[df$sse_model == 'BiSSE', ]

#make sure columns are the right type
df$trait_level_4 <- as.factor(df$trait_level_4)

#reduce to a single binary result per model per study
df <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#count numbers of 0 and 1 per trait
df2 <- df %>% group_by(trait_level_4, div_inc, .drop=FALSE) %>% summarize(count=n()) %>% ungroup() %>% complete(trait_level_4, div_inc, fill = list(count = 0))

#make binary classification character
df2$div_inc<-as.character(df2$div_inc)

#colour
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[4]))

##
l4<-ggplot(df2, aes(fill=div_inc, y=count, x=reorder(trait_level_4, -count))) +
  geom_bar(position="stack", stat="identity",alpha=0.85) +
  scale_x_discrete(name ="Trait category") +
  scale_y_continuous(expand = c(0, 0),
                     #limits=c(0,25),
                     name ="Number of times tested (BiSSE only)") +
  scale_fill_discrete(labels = c("No effect", "Effect")) +
  theme(axis.text.x = element_text(angle = 90,vjust=-0.05,hjust = 1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_line(colour="grey"),
        panel.grid.major.y = element_line(colour="grey"),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.89,0.91),
        legend.background = element_blank(),
        plot.margin = unit(c(1, 1, 1, 2),"lines")) +
  coord_cartesian(clip = "off")

l4

###
# HiSSE only trait level 4
###

#read in full data frame
df <-
  read.csv("data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'sse_model',
    'trait_level_4',
    'div_inc'
  )]

# Subset to only BiSSE
df<-df[df$sse_model == 'HiSSE', ]

#make sure columns are the right type
df$trait_level_4 <- as.factor(df$trait_level_4)

#reduce to a single binary result per model per study
df <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#count numbers of 0 and 1 per trait
df2 <- df %>% group_by(trait_level_4, div_inc, .drop=FALSE) %>% summarize(count=n()) %>% ungroup() %>% complete(trait_level_4, div_inc, fill = list(count = 0))

#make binary classification character
df2$div_inc<-as.character(df2$div_inc)

#colour
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[4]))

##
l4<-ggplot(df2, aes(fill=div_inc, y=count, x=reorder(trait_level_4, -count))) +
  geom_bar(position="stack", stat="identity",alpha=0.85) +
  scale_x_discrete(name ="Trait category") +
  scale_y_continuous(expand = c(0, 0),
                     #limits=c(0,25),
                     name ="Number of times tested (HiSSE only)") +
  scale_fill_discrete(labels = c("No effect", "Effect")) +
  theme(axis.text.x = element_text(angle = 90,vjust=-0.05,hjust = 1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_line(colour="grey"),
        panel.grid.major.y = element_line(colour="grey"),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.89,0.91),
        legend.background = element_blank(),
        plot.margin = unit(c(1, 1, 1, 2),"lines")) +
  coord_cartesian(clip = "off")

l4

#####
# Proportion of BiSSE models that are associated with div inc
#####
#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

df2<-df2[df2$sse_model=="HiSSE",]
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


## Results per model per trait type to check consistency across models

#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#reduce to a single binary result per model per study
df <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#make MuSSE results = 1
df$div_inc[df$div_inc>1]<-1

#count numbers of 0 and 1 per trait
df2 <- df %>% group_by(trait_level_2, sse_model, div_inc, .drop=FALSE) %>% summarize(count=n()) %>% ungroup() %>% complete(trait_level_2, sse_model, div_inc, fill = list(count = 0))

view(na.omit(df2))

###
# Loop through different sse models trait_level_2 ----
###

par(mfrow=c(3,3))

#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#SecSSE and BiSSEness
ssem<-c("BiSSE","HiSSE","GeoSSE","MuSSE","ClaSSE","MuHiSSE","QuaSSE","GeoHiSSE","FiSSE","MiSSE")

pl<-list()

for(i in 1:length(ssem)){

###
#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#make sure columns are the right type
df$trait_level_4 <- as.factor(df$trait_level_4)

#reduce to a single binary result per model per study
df <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

df<-df[df$sse_model==ssem[i],]

#make MuSSE results = 1
df$div_inc[df$div_inc>1]<-1

#only high freq traits
hf<-c("GeographicRange","BreedingSystem","FlowerMorpho","Pollination","Biome","Soil","LifeForm","FruitMorpho")
df<-df[df$trait_level_4 %in% hf, ]

df<-droplevels(df)

#count numbers of 0 and 1 per trait
df2 <- df %>% group_by(trait_level_4, div_inc, .drop=FALSE) %>% summarize(count=n()) %>% ungroup() %>% complete(trait_level_4, div_inc, fill = list(count = 0))

#make binary classification character
df2$div_inc<-as.character(df2$div_inc)

#colour
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[4]))

##
pl[[i]]<-ggplot(df2, aes(fill=div_inc, y=count, x=reorder(trait_level_4, -count))) +
  geom_bar(position="stack", stat="identity",alpha=0.85) +
  scale_x_discrete(name ="Trait category") +
  scale_y_continuous(expand = c(0, 0),
                     #limits=c(0,25),
                     name = paste("Number of times tested",ssem[i])) +
  scale_fill_discrete(labels = c("No effect", "Effect")) +
  theme(axis.text.x = element_text(angle = 90,vjust=-0.05,hjust = 1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_line(colour="grey"),
        panel.grid.major.y = element_line(colour="grey"),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.89,0.91),
        legend.background = element_blank(),
        plot.margin = unit(c(1, 1, 1, 2),"lines")) +
  coord_cartesian(clip = "off")

}

library(patchwork)
(pl[[1]] | pl[[2]] ) /
  (pl[[3]] | pl[[4]] | pl[[5]] | pl[[6]]) /
    (pl[[7]] | pl[[8]] | pl[[9]] | pl[[10]])
ggsave("figures/review/results_per_sse_model_level4.png",height=15,width=20)

# Loop through different sse models trait_level_2 ----

pl<-list()

for(i in 1:length(ssem)){

  ###
  #read in data
  df <- read.csv("data/sse_review_table - main_table.csv")

  #make sure columns are the right type
  df$trait_level_2 <- as.factor(df$trait_level_2)

  #reduce to a single binary result per model per study
  df <- df %>%
    group_by(study, model_no) %>%
    dplyr::slice(which.max(div_inc))

  df<-df[df$sse_model==ssem[i],]

  #make MuSSE results = 1
  df$div_inc[df$div_inc>1]<-1

  #only high freq traits
  hf<-c("Reproduction","Biogeography","Vegetative","Habitat","Pollination")
  df<-df[df$trait_level_2 %in% hf, ]

  df<-droplevels(df)

  #count numbers of 0 and 1 per trait
  df2 <- df %>% group_by(trait_level_2, div_inc, .drop=FALSE) %>% summarize(count=n()) %>% ungroup() %>% complete(trait_level_2, div_inc, fill = list(count = 0))

  #make binary classification character
  df2$div_inc<-as.character(df2$div_inc)

  #colour
  options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[4]))

  ##
  pl[[i]]<-ggplot(df2, aes(fill=div_inc, y=count, x=reorder(trait_level_2, -count))) +
    geom_bar(position="stack", stat="identity",alpha=0.85) +
    scale_x_discrete(name ="Trait category") +
    scale_y_continuous(expand = c(0, 0),
                       #limits=c(0,25),
                       name = paste("Number of times tested",ssem[i])) +
    scale_fill_discrete(labels = c("No effect", "Effect")) +
    theme(axis.text.x = element_text(angle = 90,vjust=-0.05,hjust = 1),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_line(colour="grey"),
          panel.grid.major.y = element_line(colour="grey"),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          legend.position = c(0.89,0.91),
          legend.background = element_blank(),
          plot.margin = unit(c(1, 1, 1, 2),"lines")) +
    coord_cartesian(clip = "off")

}

(pl[[1]] | pl[[2]] ) /
  (pl[[3]] | pl[[4]] | pl[[5]] | pl[[6]]) /
  (pl[[7]] | pl[[8]] | pl[[9]] | pl[[10]])
ggsave("figures/review/results_per_sse_model_level2.png",height=15,width=20)
