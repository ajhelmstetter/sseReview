rm(list=ls())
setwd("~/Dropbox/projects/AJH_DiveRS/sse_review/plots/")
library(RColorBrewer)
par(mar=c(3,3,3,3))
par(mfrow=c(1,1))

#read in model results
df<-read.csv("/home/andrew/Dropbox/projects/AJH_DiveRS/sse_review/plant_mods.csv")

#get list of -SSE models
all_kw <-
  read.table("~/Dropbox/projects/AJH_DiveRS/sse_review/keywords.txt")
all_kw$V1[84:96]
mods <- as.character(all_kw$V1[84:96])

#make columns for each model
for(i in 1:length(mods)){
  df[grep(mods[i], df$x, value = F), mods[i]] <- 1
}

#NA to 0
df[is.na(df)] <- 0

#remove 1st col
bin_df<-df[,3:length(colnames(df))]

#set rownames
rownames(bin_df)<-df$X

#extract years
as.character(df$X)
matches <- regmatches(df$X, gregexpr("_[[:digit:]]+", df$X))
tmp<-unlist(matches)
matches <- regmatches(tmp, gregexpr("[[:digit:]]+", tmp))

#check matches
cbind(df$X,as.numeric(unlist(matches)))

#add new col
bin_df$year<-as.numeric(unlist(matches))

bin_df

#reformat df
for(i in 1:(length(colnames(bin_df))-1)){
  if(i == 1){
    years<-bin_df$year[grep("1",bin_df[,i])]
    mods<-rep(colnames(bin_df)[i],length(bin_df$year[grep("1",bin_df[,i])]))
  } else {
    years<-c(years,bin_df$year[grep("1",bin_df[,i])])
    mods<-c(mods,rep(colnames(bin_df)[i],length(bin_df$year[grep("1",bin_df[,i])])))
  }
}

ym<-data.frame(years,mods)
colnames(ym)<-c("year","model")

#get counts per year
library(plyr)
ym<-count(ym, c('year', 'model'))

# library
library(ggplot2)

# Stacked barplot
ggplot(ym, aes(fill=model, y=freq, x=year)) +
  geom_bar(position="stack", stat="identity") +
  scale_y_continuous(breaks = seq(0, 70, by = 5), expand = expansion(mult = c(0, .1))) +
  scale_x_continuous(breaks = seq(2007, 2020, by = 1)) +
  xlab("Publication year") +
  ylab("Frequency of model use") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.2, color="black"),
    panel.grid.minor.y = element_line( size=.1, color="grey"),
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 1, linetype = "solid"))

#to add:

#check number of papers published graph, put on graph width?
#check overlap between hisse/bisse
