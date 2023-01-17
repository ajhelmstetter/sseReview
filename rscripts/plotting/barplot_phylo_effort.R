###
# Number of studies per order on tree
###

#Library
library(tidyverse)
library(ggplot2)
library(treemapify)
library(RColorBrewer)
library(ggrepel)

#read in full data frame
df <-
  read.csv("~/Dropbox/projects/AJH_DiveRS/sseReview/data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'order'
  )]

#make sure columns are the right type
df$order <- as.factor(df$order)

#remove na
#df<-drop_na(df)

#unique combinations of study/order
df_uni<- unique(df[c("study", "order")])

#rearrange df to give value of 1 per order and study combination
library(dplyr)
so <-df_uni %>%
  dplyr::count(order, study, .drop = F)
head(so)

#get frequency table
ord_freq<-data.frame(table(so$order))
colnames(ord_freq)<-c("order","freq")
head(ord_freq)

# Number of species per order (I extracted a few years ago from the APG website @ mobot):
# https://www.mobot.org/mobot/research/apweb/orders/boraginalesweb.htm

#read in data
ord_no<-read.csv("data/species_per_order.csv")

#get overlapping data in each dataset
ord_no<-ord_no[ord_no$order%in%ord_freq$order,]

#sort alphabetically
ord_no<-ord_no[order(ord_no$order),]
head(ord_no)

#get frequencies for orders that are tested
ord_freq<-ord_freq[ord_freq$order%in%ord_no$order,]

#check dfs are in same order
ord_no$order==ord_freq$order

#add frequencies to order species numbers table
ord_no$freq <- ord_freq$freq

#read in order tree
#Li, Hong-Tao et al. (2019), Data from: Origin of angiosperms and the puzzle of the Jurassic gap, Dryad, Dataset, https://doi.org/10.5061/dryad.bq091cg

library(ape)
phy<-read.nexus("data/2881_dating_Order.tre")

#drop non-angio tips
phy<-drop.tip(phy,c("Cycadales",
                    "Ginkgoales",
                    "Pinales",
                    "Ephedrales",
                    "Welwitschiales",
                    "Gnetales",
                    "Araucariales",
                    "Cupressales"))


#ladderize
phy<-ladderize(phy)

#drop "Asparagales1"
phy<-drop.tip(phy,"'Asparagales1'")

#keep only order and freq from studied, reorder
stud<-ord_no[,c(1,3,2)]
head(stud)

#add 0's to unstudied orders
unstud<-data.frame(setdiff(phy$tip.label,ord_no$order),rep(0,length(setdiff(phy$tip.label,ord_no$order))))

#col names
colnames(unstud)<-c("order","freq")

#sort alphabetically
unstud<-unstud[order(unstud$order),]

#reread no species dataset
ord_no<-read.csv("data/species_per_order.csv")

#get only unstudied orders
ord_no<-ord_no[ord_no$order%in%unstud$order,]

#order alphabetically
ord_no<-ord_no[order(ord_no$order),]

#check matches
ord_no$order==unstud$order

#combine order freq and species numbers
unstud<-cbind(ord_no,unstud)

#reorder columns
unstud<-unstud[,c(1,4,2)]
head(unstud)

#combine studied and unstudied dfs
df<-rbind(stud,unstud)

#problem with installation of ggtreeExtra
#requires:
#install.packages("https://cran.r-project.org/src/contrib/Archive/rvcheck/rvcheck_0.1.8.tar.gz", repos = NULL)
library(ggtreeExtra)
library(ggtree)
library(dplyr)

#plot tree
p <- ggtree(phy, layout="fan", open.angle=10) + geom_tiplab(size=2,hjust=-0.05)
p


#add barplot
p2 <- p +
  geom_fruit(
    data=df,
    geom=geom_bar,
    mapping = aes(
      y=order,
      x=freq
    ),
    fill = brewer.pal(5,"Set2")[3],
    width=0.6,
    alpha=0.8,
    pwidth=0.5,
    orientation="y",
    stat="identity",
    offset = 0.5,
    axis.params=list(
      axis       = "x",
      text.size  = 1.8,
      hjust      = 1,
      vjust      = 0.5,
      nbreak     = 3
    ),
    grid.params=list()
  )

p2

ggsave("figures/barplot_phylo_effort.png",
       width = 20,
       height = 20,
       units = 'cm')

#not run
#add species number layer
#p3 <- p2 + geom_fruit(
#    data=df,
#    geom=geom_bar,
#    mapping = aes(
#      y=order,
#      x=log(no_species)
#    ),
#    pwidth=0.4,
#    orientation="y",
#    stat="identity",
#    offset = 0.38,
#    axis.params=list(
#      axis       = "x",
#      text.size  = 1.8,
#      hjust      = 1,
#      vjust      = 0.5,
#      nbreak     = 3,
#    ),
#    grid.params=list()
#  )
#
#p3
