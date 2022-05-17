####
# Predicting SSE model results with GAM
####

rm(list = ls())

library(xgboost)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Matrix)
library(caret)

#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#check col names
colnames(df)

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'year',
    'order',
    'family',
    'level',
    'clade',
    'trait_level_1',
    'trait_level_2',
    'trait_level_3',
    'trait_level_4',
    'trait_level_5',
    'trait_level_6',
    'sse_model',
    'tips',
    'age',
    #'age_inferred',
    'no_markers',
    #'no_plastid',
    #'no_mito',
    #'no_nuclear',
    'perc_sampling',
    'samples_per_state',
    'putative_ancestral_state',
    'div_inc'
  )]


###
# Data transformations
###

###
# Make samples per state into tip bias
###

#make column with combination of study and model (make sure they are rows 1 and 2)
tmp_df <- df %>% tidyr::unite("study_model", 1:2, remove = T)

#reduce dataset to two columns
tmp_df <- tmp_df[, c("study_model", "samples_per_state")]

head(tmp_df)

#get max and min values of samples per state for each model
top_df <-
  tmp_df %>% group_by(study_model) %>% slice_max(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)
bot_df <-
  tmp_df %>% group_by(study_model) %>% slice_min(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)


#reduce to a single binary result per model per study
df3 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#set Multi-State results to 1
df3$div_inc[df3$div_inc > 1] <- 1

#make binary trait factor
df3$div_inc <- as.factor(df3$div_inc)

#add tip bias column by dividing larger number of tips with state A by smaller number of tips with state B
#multi-state models are therefore largest tip bias possible in the data
df3$tip_bias <- top_df$samples_per_state / bot_df$samples_per_state

#get rid of inf tip bias value
df3<-df3[!is.infinite(df3$tip_bias),]

#convert back to df
df <- as.data.frame(df3)

# NUMBER OF MARKERS
#log no_markers as long tail
hist(df$no_markers)
df$no_markers <- log(df$no_markers)
hist(df$no_markers)

#NUMBER OF TIPS
#log tips as long tail
hist(df$tips)
df$tips <- log(df$tips)
hist(df$tips)

#AGE OF TREE
#log age as long tail
#still strange
hist(df$age)
df$age <- log(df$age)
hist(df$age)

#GLOBAL SAMPLING FRACTION
hist(df$perc_sampling)
#arcsine
df$perc_sampling <- asin(sqrt(df$perc_sampling))
hist(df$perc_sampling)

#TIP BIAS log
hist(df$tip_bias)
df$tip_bias<- log(df$tip_bias)
hist(df$tip_bias)

#SAMPLES PER STATE
#change to numeric prior to transformation, remove NAs
df$samples_per_state <- as.numeric(df$samples_per_state)

#make sure factors are factors
df$order <- as.factor(df$order)
df$sse_model <- as.factor(df$sse_model)
df$year <- as.factor(df$year)
df$putative_ancestral_state <- as.factor(df$putative_ancestral_state)

#remove study and model no (first two rows)
#sampling per state various among rows in models, should remove as well.
df <- df[, -which(colnames(df) %in% c('study',
                                      'model_no',
                                      'samples_per_state'))]

#remove quasse for div_inc
#may need to remove others too
#df <- df[df$sse_model != "QuaSSE", ]

####
# Check correlation of continuous vars
####

df_cor <-
  df[, c(
    'tips',
    'no_markers',
    'age',
    'perc_sampling',
    'tip_bias'
  )]

cor(df_cor, use = "complete.obs")

#remove no_markers outlier
df<-df[df$no_markers<10,]

#scale the data
#for (i in 1:length(df[1, ])) {
#  if (is.numeric(df[, i])) {
#    df[, i] <- scale(df[, i])
#  }
#}

#can include ordinal vars
#ordinal variable : can take a limited number of values (like factor) ;
#these values are ordered (unlike factor). Here these ordered values are: Marked > Some > None
#GOOD FOR MUSSE?

#One-hot encoding
#Next step, we will transform the categorical data to dummy variables. This is the one-hot encoding step.
#The purpose is to transform each value of each categorical feature in a binary feature {0, 1}.
#Response is excluded because it will be our label column, the one we want to predict.

#####
# Replacing NAs
#####

#select only numeric columns
df_nums <- df %>% dplyr::select(where(is.numeric))
df_nums <- as.data.frame(cbind(df_nums,df$div_inc))
colnames(df_nums)[length(colnames(df_nums))]<-"div_inc"

#percentage NA
mean(is.na(df_nums))

###
# Two strategies to deal with NAs
###

#replace NAs with values
library(gam)
df_nums <- na.gam.replace(df_nums)

#remove missing data
#df_nums<-na.omit(df_nums)

#mgcv package
#detach other gam functions
detach("package:gam", unload=TRUE)
library(mgcv)

#make GAM
mongam <- gam(data = df_nums, div_inc ~
                #sse_model +
                #putative_ancestral_state +
                #trait_level_1 +
                #s(tips, k = 4, bs = "cr", by = sse_model ) + ## ca c'est si tu veux faire une interaction
                #s(perc_sampling, k = 3, bs = "cr", by = tip_bias) +
                #ti(perc_sampling, tip_bias, k = 5, bs = "cr") + ##interaction between continuous variables
                s(tips, k = 5, bs = "cr")
              + s(age, k = 5, bs = "cr")
              + s(no_markers, k = 5, bs = "cr")
              + s(perc_sampling, k = 5, bs = "cr")
              + s(tip_bias, k = 5, bs = "cr")
                ,
                family = binomial())

summary(mongam)

png("figures/gam_allvars_mean.png",width = 1000,height=500)
par(mfrow=c(2,3))
par(mar=c(5,5,3,1))
plot.gam(mongam,ylim=c(-2,2),cex.lab=1.75,cex.axis=1.75)

mtext("(a)",side=3,line=28,adj = -1.45)
mtext("(b)",side=3,line=28,adj = -0.15)
mtext("(c)",side=3,line=28,adj = 1.15)
mtext("(d)",side=3,line=1,adj = -1.45)
mtext("(e)",side=3,line=1,adj = -0.15)

dev.off()


####
# GAM each var separately
####

png("figures/gam_indvars_mean.png",width = 1000,height=500)

par(mfrow=c(2,3))
par(mar=c(5,5,3,1))

#tips
mongam <- gam(data = df,
              div_inc ~
              s(tips, k = 5, bs = "cr"),
              family = binomial()
              )
summary(mongam)
plot.gam(mongam,ylim=c(-2,2),cex.lab=1.75,cex.axis=1.75)
mtext("(a)",side=3,line=1,adj = -0.15)

#age
mongam <- gam(data = df,
              div_inc ~
              s(age, k = 5, bs = "cr"),
              family = binomial()
)
summary(mongam)
plot.gam(mongam,ylim=c(-2,2),cex.lab=1.75,cex.axis=1.75)
mtext("(b)",side=3,line=1,adj = -0.15)

#number of markers
mongam <- gam(data = df,
              div_inc ~
              s(no_markers, k = 5, bs = "cr"),
              family = binomial()
)
summary(mongam)
plot.gam(mongam,ylim=c(-2,2),cex.lab=1.75,cex.axis=1.75)
mtext("(c)",side=3,line=1,adj = -0.15)

#sampling fraction
mongam <- gam(data = df,
              div_inc ~
                s(perc_sampling, k = 5, bs = "cr"),
              family = binomial()
)
summary(mongam)
plot.gam(mongam,ylim=c(-2,2),cex.lab=1.75,cex.axis=1.75)
mtext("(d)",side=3,line=1,adj = -0.15)


#tip bias
mongam <- gam(data = df,
              div_inc ~
              s(tip_bias, k = 5, bs = "cr"),
              family = binomial()
)
summary(mongam)
plot.gam(mongam,ylim=c(-2,2),cex.lab=1.75,cex.axis=1.75)
mtext("(e)",side=3,line=1,adj = -0.15)


dev.off()
