###
# Number of studies
###
rm(list = ls())

library(tidyr)
library(dplyr)

#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#unique study names
unique(df$study)

#number of studies
length(unique(df$study))

###
# Number of models
###

#make df of study and model number in study
stud_mod<-data.frame(df$study,df$model_no)

#unique combinations of model number and study
uni_stud_mod<-unique(stud_mod)
head(uni_stud_mod)

#number of models
length(uni_stud_mod$df.model_no)

#####
# studies that look at a single clade
#####

#read in full data frame
df <-
  read.csv("data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'sse_model',
    'model_no',
    'order',
    'family',
    'clade',
    'trait_level_1',
    'trait_level_2',
    'trait_level_4',
    'trait_level_6',
    'character_state',
    'div_inc'
  )]

#unique combinations of study, model number and clade
#i.e. how many models per study run on each clade
df_uni<- unique(df[c("study", "clade")])

#number of studies focusing on one clade only
no_single<-length(table(df_uni$study)[table(df_uni$study)==1])

#BUT some clades were nested:
## Alcantara et al.
## De Vos et al.
## Ebersbach et al.
## Forest et al.
## Roalson & Roberts
## Spriggs et al.
no_single <- no_single + 6

#BUT some clades were analysed separately in a single model?
## Golberg et al. 2011
## Anacker et al.
no_single <- no_single - 2

#BUT: some studies looked at multiple clades but could not be recorded this way (lack of info)
## Mayrose et al.
no_single <- no_single - 1

#number of studies on a single clade
no_single

#on multiple clades
152-no_single

#####
# Studies that look at a single trait_level_6
#####

#unique combinations of study and trait level 6
#i.e. each different trait examined in each study
df_uni<- unique(df[c("study", "trait_level_6")])

#number of studies that examined traits belonging to a single trait_level_6 category
length(table(df_uni$study)[table(df_uni$study)==1])

#####
# Studies that look at different traits
#####

#unique combinations of study and trait level 1
#i.e. how many studies looked at different traits
df_uni<- unique(df[c("study", "trait_level_1")])

#number of studies that examined differents traits belonging to more than one trait_level_1 category
tl1<-length(table(df_uni$study)[table(df_uni$study)>1])

#+1 for onstein and linder who looked at trait_level_1 'combination' only
tl1 + 1

####
# Number of orders
####

#remove anything that isn't order (e.g. 'multiple')
orders<-unique(df$order)[grep("ales",unique(df$order))]

#list of orders studied
sort(orders)

#number orders studied
length(orders)

####
# Number of families
####

#remain anything that isn't a family (e.g. 'multiple')
families<-unique(df$family)[grep("ceae",unique(df$family))]

#list of families studied
sort(families)

#number families  studied
length(families)


#####
# Number intrinsic
#####

#intrinsic rows only
df_int<-df[df$trait_level_1=="Intrinsic",]

#unique combinations of study and model number
df_int<-df_int[,c(1,3)]
df_int_uni<-unique(df_int)

#number of models
length(df_int_uni$study)

#perecentage
length(df_int_uni$study) / 629

#####
# Number extrinsic
#####

#extrinsic rows only
df_ext<-df[df$trait_level_1=="Extrinsic",]

#unique combinations of study and model number
df_ext<-df_ext[,c(1,3)]

#unique combinations of study and model number
df_ext_uni<-unique(df_ext)

#number of models
length(df_ext_uni$study)

#perecentage
length(df_ext_uni$study) / 629

#####
# Proportion intrinsic that are associated with div inc
#####

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

#proportion of total models run on intrinsic traits
sum(table(df2_int$div_inc)) / length(df2$study)

#####
# Proportion extrinsic that are associated with div inc
#####

#df with only extrinsic traits
df2_ext<-df2[df2$trait_level_1=="Extrinsic",]

#number of times extrinsic is associated with trait dependent diversification
table(df2_ext$div_inc)

#proportion of models with extrinsic traits assoc with div_inc
table(df2_ext$div_inc)[2] / (table(df2_ext$div_inc)[1] + table(df2_ext$div_inc)[2])

#proportion of total models run on extrinsic traits
sum(table(df2_ext$div_inc)) / length(df2$study)

#####
# Number of studies with hidden state models
#####

# Subset to only hidden state models
hidd<-c("HiSSE","MuHiSSE","GeoHiSSE","SecSSE","MiSSE")
df2<-df[df$sse_model %in% hidd, ]

#look at unique studies
sort(unique(df2$study))

#number of unique studies
length(unique(df2$study))
