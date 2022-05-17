####
# How do dataset properties compare to recommendations?
####

rm(list = ls())

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
    'tips',
    'year',
    'div_inc'
  )]

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))
head(df2)

#remove na
df2<-na.omit(df2)

####
# DAVIS ET AL 2013
####

#make new col with 2's
df2$davis<-rep(2,length(df2$study))

#split studies by year of publication Davis et al. 2013
#1 = pre DEL, 2 = post DEL
df2$davis[as.numeric(as.character(df2$year))<2014]<-1
df2$davis<-as.factor(df2$davis)

#make frequency table according to Davis et al. thresholds
freq<-vector()
freq[1]<-sum(df2[df2$davis==2,]$tips>=500)
freq[2]<-sum(df2[df2$davis==2,]$tips<500 & df2[df2$davis==2,]$tips>=300)
freq[3]<-sum(df2[df2$davis==2,]$tips<300 & df2[df2$davis==2,]$tips>=100)
freq[4]<-sum(df2[df2$davis==2,]$tips<100 & df2[df2$davis==2,]$tips>=50)
freq[5]<-sum(df2[df2$davis==2,]$tips<50)

freq[6]<-sum(df2[df2$davis==1,]$tips>=500)
freq[7]<-sum(df2[df2$davis==1,]$tips<500 & df2[df2$davis==1,]$tips>=300)
freq[8]<-sum(df2[df2$davis==1,]$tips<300 & df2[df2$davis==1,]$tips>=100)
freq[9]<-sum(df2[df2$davis==1,]$tips<100 & df2[df2$davis==1,]$tips>=50)
freq[10]<-sum(df2[df2$davis==1,]$tips<50)

freq

#make table with frequency, threshold, and  time (pre/post DEL)
df3<-data.frame(freq,rep(c("500=<",
                           "300-499",
                           "100-299",
                           "50-99",
                           "<50"),2),
                c(rep("post-davis",5),rep("pre-davis",5)))
colnames(df3)<-c("freq","no_tips","time")

#change to factor for plotting
df3$no_tips<-factor(df3$no_tips, c("<50", "50-99", "100-299", "300-499","500=<"))

df3

#grouped barplot
ggplot(df3, aes(fill=time, y=freq, x=no_tips))+
  geom_bar(position="dodge", stat="identity")+
  scale_x_discrete(name ="No. of tips") +
  scale_y_continuous(expand = c(0, 0),
                     limits=c(0,175),
                     name ="Number of models") +
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

#number of models < 300 tips pre DEL
sum(df3$freq[8:10])

#total number of models pre DEL
sum(df3[df3$time=="pre-davis",]$freq)

#proportion models < 300 tips pre-davis/ total models pre-davis
(sum(df3$freq[8:10]))  / sum(df3[df3$time=="pre-davis",]$freq)

#number of models < 300 tips post DEL
sum(df3$freq[3:5])

#total number of models post DEL
sum(df3[df3$time=="post-davis",]$freq)

#proportion models < 300 tips post-davis/ total models post-davis
(sum(df3$freq[3:5]))  / sum(df3[df3$time=="post-davis",]$freq)

