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

#get max and min values of samples per state for each model
top_df <-
  df %>% group_by(study, model_no) %>% slice_max(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)

head(top_df)

bot_df <-
  df %>% group_by(study, model_no) %>% slice_min(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)

head(bot_df)

#reduce to a single binary result per model per study
df3 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#set Multi-State results to 1
df3$div_inc[df3$div_inc > 1] <- 1

#make binary trait factor
#df3$div_inc <- as.factor(df3$div_inc)

#check that orders of df2 and top/bot_df match up
top_df$study==bot_df$study
top_df$model_no==bot_df$model_no

df3$model_no==bot_df$model_no
df3$model_no==top_df$model_no

df3$study==bot_df$study
df3$study==top_df$study

#add tip bias column by dividing larger number of tips with state A by smaller number of tips with state B
#multi-state models are therefore largest tip bias possible in the data
df3$tip_bias <- top_df$samples_per_state / bot_df$samples_per_state

#get rid of inf tip bias value
df3$tip_bias[is.infinite(df3$tip_bias)] <- NA

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

#make sure factors are factors
df$order <- as.factor(df$order)
df$sse_model <- as.factor(df$sse_model)
df$year <- as.factor(df$year)
df$putative_ancestral_state <- as.factor(df$putative_ancestral_state)
df$div_inc <- as.factor(df$div_inc)

#remove study and model no (first two rows)
#sampling per state various among rows in models, should remove as well.
df <- df[, -which(colnames(df) %in% c('study',
                                      'model_no',
                                      'samples_per_state'))]

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

#NOT NEEDED:
#remove no_markers outliers
#df$no_markers[df$no_markers>10] <- NA

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
                s(tips, k = 5, bs = "cr")
              + s(age, k = 5, bs = "cr")
              + s(no_markers, k = 5, bs = "cr")
              + s(tip_bias, k = 5, bs = "cr")
              + s(perc_sampling, k = 5, bs = "cr")
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

png("figures/gam_indvars.png",width = 1000,height=500)

par(mfrow=c(2,3))
par(mar=c(5,5,3,1))

#df3 is

#tips
mongam <- mgcv::gam(data = df,
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
#one less than density because removed outlier
mongam <- gam(data = df,
              div_inc ~
              s(no_markers, k = 5, bs = "cr"),
              family = binomial()
)
summary(mongam)
plot.gam(mongam,ylim=c(-2,2),cex.lab=1.75,cex.axis=1.75)
mtext("(c)",side=3,line=1,adj = -0.15)

#tip bias
#two infinite that are removed
mongam <- gam(data = df,
              div_inc ~
              s(tip_bias, k = 5, bs = "cr"),
              family = binomial()
)
summary(mongam)
plot.gam(mongam,ylim=c(-2,2),cex.lab=1.75,cex.axis=1.75)
mtext("(d)",side=3,line=1,adj = -0.15)

#sampling fraction
mongam <- gam(data = df,
              div_inc ~
                s(perc_sampling, k = 5, bs = "cr"),
              family = binomial()
)
summary(mongam)
plot.gam(mongam,ylim=c(-2,2),cex.lab=1.75,cex.axis=1.75)
mtext("(e)",side=3,line=1,adj = -0.15)

dev.off()
