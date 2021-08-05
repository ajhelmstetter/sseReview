rm(list = ls())

library(xgboost)
library(tidyr)
library(dplyr)
library(ggplot2)

#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#check col names
colnames(df)

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'order',
    'trait_level_1',
    'trait_level_2',
    'trait_level_3',
    'trait_level_4',
    'trait_level_5',
    'sse_model',
    'tips',
    'year',
    'no_markers',
    'age',
    'age_inferred',
    'div_inc',
    'no_plastid',
    'no_mito',
    'no_nuclear',
    'perc_sampling',
    'samples_per_state',
    'putative_ancestral_state'
  )]

###
# Data transformations
###

# NUMBER OF MARKERS
#log no_markers as long tail
hist(df$no_markers)
df$no_markers <- log(df$no_markers)
hist(df$no_markers)

#NUMBER OF TIPS
#log tips as long tail
#hist(df$tips)
#df$tips <- log(df$tips)
#hist(df$tips)

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

#SAMPLES PER STATE
#change to numeric prior to transformation, remove NAs
df$samples_per_state <- as.numeric(df$samples_per_state)

#fix one study (Sabath et al.) with 0
df$samples_per_state[df$samples_per_state == 0] <- NA

#log transform
#hist(df$samples_per_state)
#df$samples_per_state <- log(df$samples_per_state)
#hist(df$samples_per_state)

df$samples_per_state[is.na(df$samples_per_state)] <- 0

###
# Make samples per state into tip bias
###

#remove quasse for div_inc
#may need to remove others too
df <- df[df$sse_model != "QuaSSE",]

#make sure factors are factors
df$order <- as.factor(df$order)
#df$trait_type <- as.factor(df$trait_level_3)
df$sse_model <- as.factor(df$sse_model)
df$year <- as.factor(df$year)

#make samples per state into tip bias
df$samples_per_state

#make column with combination of study and model
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

#make binary trait factor
df3$div_inc <- as.factor(df3$div_inc)

#make sampling fraction %
df3$perc_sampling <- df3$perc_sampling * 100

#add tip bias column by dividing larger number of tips with state A by smaller number of tips with state B
#multi-state models are therefore largest tip bias possible in the data
df3$tip_bias <- top_df$samples_per_state / bot_df$samples_per_state

#convert back to df
df <- as.data.frame(df3)

#remove study and model no (first two rows)
#df <- df[, -which(colnames(df) %in% c('study',
#                                      'model_no',
#                                      'samples_per_state'))]

library(data.table)
df <- data.table(df, keep.rownames = FALSE)
df[is.na(df)] <- 0
head(df)
str(df)

####
# DAVIS ET AL 2013
####

#make new col
df$davis<-rep(2,length(df$study))
#split studies by year of publication Davis et al. 2013
#1 = pre DEL, 2 = post DEL
df$davis[as.numeric(as.character(df$year))<2014]<-1
df$davis<-as.factor(df$davis)
#overlain histograms
ggplot(df, aes(x=tips, color=davis,fill=davis)) +
  geom_histogram(alpha=0.5, position="identity",bins=5)

#make frequency table according to Davis et al. thresholds
freq<-vector()
freq[1]<-sum(df[df$davis==2,]$tips>500)
freq[2]<-sum(df[df$davis==2,]$tips<500 & df[df$davis==2,]$tips>=300)
freq[3]<-sum(df[df$davis==2,]$tips<300 & df[df$davis==2,]$tips>=100)
freq[4]<-sum(df[df$davis==2,]$tips<100 & df[df$davis==2,]$tips>=50)
freq[5]<-sum(df[df$davis==2,]$tips<50)

freq[6]<-sum(df[df$davis==1,]$tips>500)
freq[7]<-sum(df[df$davis==1,]$tips<500 & df[df$davis==1,]$tips>=300)
freq[8]<-sum(df[df$davis==1,]$tips<300 & df[df$davis==1,]$tips>=100)
freq[9]<-sum(df[df$davis==1,]$tips<100 & df[df$davis==1,]$tips>=50)
freq[10]<-sum(df[df$davis==1,]$tips<50)

df2<-data.frame(freq,rep(c("500=<",
                      "300-499",
                      "100-299",
                      "50-99",
                      "<50"),2),
                c(rep("post-davis",5),rep("pre-davis",5)))
colnames(df2)<-c("freq","no_tips","time")
str(df2)

df2$no_tips<-as.factor(df2$no_tips)
df2$no_tips<-factor(df2$no_tips, c("<50", "50-99", "100-299", "300-499","500=<"))


ggplot(df2, aes(fill=time, y=freq, x=no_tips))+
  geom_bar(position="dodge", stat="identity")+
  scale_x_discrete(name ="No. of tips") +
  scale_y_continuous(expand = c(0, 0),
                     limits=c(0,120),
                     name ="Count") +
  scale_fill_discrete(labels = c("Post Davis et al.", "Pre Davis et al.")) +
  theme(axis.text.x = element_text(angle = 90,vjust=-0.05,hjust = 1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_line(colour="grey"),
        panel.grid.major.y = element_line(colour="grey"),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.89,0.93),
        legend.background = element_blank())

####
# Proportions for DEL tree size
####

df2

#studies < 100 tips pre-davis/ total studies pre-davis
(sum(df2$freq[8:10]))  / sum(df2[df2$time=="pre-davis",]$freq)

#studies < 100 tips pre-davis/ total studies pre-davis
(sum(df2$freq[3:5]))  / sum(df2[df2$time=="post-davis",]$freq)

####
# Sampling fraction
###

df3<-df

#Sampling fraction pre/post 2009
df3_2009<-df3[as.numeric(as.character(df3$year))<2010,]
table(df3_2009$perc_sampling>=50)
table(df3_2009$perc_sampling<50)

df3_2010<-df3[as.numeric(as.character(df3$year))>2009,]
table(df3_2010$perc_sampling>=50)
table(df3_2010$perc_sampling<50)

#Sampling fraction pre/post 2015
df3_2015<-df3[as.numeric(as.character(df3$year))<2016,]
table(df3_2015$perc_sampling>=50)
table(df3_2015$perc_sampling<50)

df3_2016<-df3[as.numeric(as.character(df3$year))>2015,]
table(df3_2016$perc_sampling>=50)
table(df3_2016$perc_sampling<50)


####
# Tip ratio bias
####
####
# CHECK MATHS
####

#remove inf cols and columns with no data
df3<-df[df$tip_bias>0,]
df3<-df3[df3$tip_bias<1000000,]
df3$tip_bias

#proportion of studies with 10% tip bias post-davis
sum(df3[df3$davis==2,]$tip_bias>10) / length(df3[df3$davis==2,]$tip_bias)

#proportion of studies with 10% tip bias pre-davis
sum(df3[df3$davis==1,]$tip_bias>10) / length(df3[df3$davis==1,]$tip_bias)

#number of studies where these data were available
unique(df3$study)
