###
# Number of studies
###
rm(list = ls())

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
length(table(df_uni$study)[table(df_uni$study)==1])

#####
# Studies that look at a single trait_level_6
#####

#unique combinations of study and trait level 6
#i.e. how many studies looked at different traits
df_uni<- unique(df[c("study", "trait_level_6")])

#number of studies that examined differents traits belonging to more than one trait_level_1 category
length(table(df_uni$study)[table(df_uni$study)==1])


#####
# Studies that look at different traits
#####

#unique combinations of study and trait level 1
#i.e. how many studies looked at different traits
df_uni<- unique(df[c("study", "trait_level_1")])

#number of studies that examined differents traits belonging to more than one trait_level_1 category
length(table(df_uni$study)[table(df_uni$study)>1])


####
# Number of orders
####

#remain anything that isn't order (e.g. 'multiple')
orders<-unique(df$order)[grep("ales",unique(df$order))]

#list of orders studied
sort(orders)

#number orders studied
length(orders)

####
# Number of families
####

#remain anything that isn't order (e.g. 'multiple')
families<-unique(df$family)[grep("ceae",unique(df$family))]

#list of orders studied
sort(families)

#number orders studied
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

#make > 1 MuSSE div_inc values 1
df$div_inc[df$div_inc>1]<-1

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))


#df with only intrinsic traits
df2_int<-df2[df2$trait_level_1=="Intrinsic",]

#number of times intrinsic is associated with trait dependent diversification
table(df2_int$div_inc)

#proportion of models with intrinsic traits assoc with div_inc
table(df2_int$div_inc)[2] / (table(df2_int$div_inc)[1] + table(df2_int$div_inc)[2])

#####
# Proportion extrinsic that are associated with div inc
#####

#df with only extrinsic traits
df2_ext<-df2[df2$trait_level_1=="Extrinsic",]

#number of times extrinsic is associated with trait dependent diversification
table(df2_ext$div_inc)

#proportion of models with extrinsic traits assoc with div_inc
table(df2_ext$div_inc)[2] / (table(df2_ext$div_inc)[1] + table(df2_ext$div_inc)[2])

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
