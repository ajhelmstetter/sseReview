library(ape)

#read in tree files from MuSSE runs
trees<-list.files("~/Downloads/6-MUSSE/4_run_musse/input/trees/",full.names = T)

#empty vector
crowns<-vector()

#get root ages for all trees
for(i in 1:length(trees)){
  phy<-read.nexus(trees[i])
  crowns[i]<-max(branching.times(phy))
}

#data frame of crown ages
data.frame(list.files("~/Downloads/6-MUSSE/4_run_musse/input/trees/"),crowns)

#list of files with rates
files<-list.files("~/Downloads/6-MUSSE/5_prepare_graphs/input/",full.names = T)

#empty vector
rates<-vector()

#loop reading tables and extracting rates
j<-1
for(i in 1:length(files)){
  df<-read.table(files[i])
  rates[j]<-df[2,1]-df[4,1]
  j<-j+1
  rates[j]<-df[1,1]-df[3,1]
  j<-j+1
}

head(rates)

#make df with order, rates and state
rates_df<-data.frame(sort(rep(list.files("~/Downloads/6-MUSSE/5_prepare_graphs/input/"),2)),rates,rep(c("temp","trop"),length(rates)/2))

rates_df


##
# 3 states
##

#list of files with rates
files<-list.files("~/Downloads/6-MUSSE/4_run_musse/output/three_state",full.names = T)
files

#empty vector
rates<-vector()

#loop reading tables and extracting rates
j<-1
for(i in 1:length(files)){
  df<-read.table(files[i])
  rates[j]<-df[1,1]-df[4,1]
  j<-j+1
  rates[j]<-df[2,1]-df[5,1]
  j<-j+1
  rates[j]<-df[3,1]-df[6,1]
  j<-j+1
}

head(rates)

#make df with order, rates and state
rates_df<-data.frame(sort(rep(list.files("~/Downloads/6-MUSSE/4_run_musse/output/three_state"),3)),rates,rep(c("afro","asian","neo"),length(rates)/3))

colnames(rates_df)<-c("file","rates","state")
rates_df

## samples per state
library(geiger)
setwd("~/Downloads/6-MUSSE/4_run_musse/input/")

#load global data
dat <- read.table("input_musse_2areas_wsp.txt", header = T, sep = "\t", row.names = 1)
samp.all <- read.table("sampling_estimate_out_2areas.txt", header = T, sep = "\t")

liste <- list.files(path = "trees",pattern = ".nex")

i<-1

#1 = tropical
#2 = non-tropical

for(i in 1:length(liste)){
  tax <- liste[i] #example tree
  print(liste[i])

  #load tree
  tre <-  read.nexus(paste("trees/", tax,sep = ""))

  #prepare input for make.musse function
  tre.dat <- treedata(tre,dat)

  states.vector <- tre.dat$data[,1]
  names(states.vector) <- row.names(tre.dat$data)
  print(table(tre.dat$data))

}

#three state


#load global data
dat <- read.table("input_musse_all_tropical_areas_wsp.txt", header = T, sep = "\t", row.names = 1)

liste <- list.files(path = "trees",pattern = ".nex")

i<-1

#1 = afrotropics
#2 = asian tropics
#3 = neotropics

for(i in 1:length(liste)){
  tax <- liste[i] #example tree
  print(liste[i])

  #load tree
  tre <-  read.nexus(paste("trees/", tax,sep = ""))

  #prepare input for make.musse function
  tre.dat <- treedata(tre,dat)

  states.vector <- tre.dat$data[,1]
  names(states.vector) <- row.names(tre.dat$data)
  print(table(tre.dat$data))

}

