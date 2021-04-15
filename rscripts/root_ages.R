library(ape)

#single tree
phy<-read.nexus("~/Downloads/doi_10.5061_dryad.tb7055f__v1/soldata/basicdata/fullmatchtree.nex")
phy
max(branching.times(phy))

plot(ladderize(phy),cex=0.5)
nodelabels(round(branching.times(phy),3),cex=0.3)

#trees in folder
phyl<-list.files("~/Downloads/doi_10.5061_dryad.8m7t2__v1/WilliamsEtAl.AJB2014.Scripts/WilliamsEtAl.AJB2014.Scripts/tree.",full.names = T,pattern="*.nex")

phy<-read.nexus(phyl[6])
plot(phy,cex=0.5)
nodelabels(round(branching.times(phy),3))


age<-vector()

for(i in 1:length(phyl)){
  phy<-read.nexus(phyl[i])
  age[i]<-max(branching.times(phy[[1]]))
}

age
