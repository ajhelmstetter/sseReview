###
# Number of orders / families per study
###

#Library
library(tidyverse)
library(ggplot2)
library(treemapify)
library(RColorBrewer)

#read in full data frame
df <-
  read.csv("data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'family',
    'order' )]

###
# Order level
###

#make sure columns are the right type
df$order <- as.factor(df$order)

#unique combinations of study/order
df_uni<- unique(df[c("study", "order")])

#get frequency table
ord_freq<-data.frame(table(df_uni$order))
colnames(ord_freq)<-c("order","freq")
head(ord_freq)

#filter out multiple
ord_freq<-ord_freq[grep("ales",ord_freq$order),]

#colour palette
nb.cols <- length(levels(ord_freq$order))
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)

#plot treemap
ggplot(ord_freq, aes(area = freq, fill = order, label=order)) +
  geom_treemap(alpha=0.7)  +
  geom_treemap_text(colour = "black", place = "centre") +
  theme(legend.position = "none") +
  scale_fill_manual(values = mycolors)

ggsave("figures/treemap_orders.png",
       width = 20,
       height = 12,
       units = 'cm'
)

###
# Family level
###

#make sure columns are the right type
df$family <- as.factor(df$family)

#unique combinations of study/family
df_uni<- unique(df[c("study", "family")])

#get frequency table
fam_freq<-data.frame(table(df_uni$family))
colnames(fam_freq)<-c("family","freq")

#filter out non-family and multiple
fam_freq<-fam_freq[grep("ceae",fam_freq$family),]

#colour palette
nb.cols <- length(levels(fam_freq$family))
mycolors <- colorRampPalette(brewer.pal(12, "RdYlBu"))(nb.cols)

#plot treemap
ggplot(fam_freq, aes(area = freq, fill = family, label=family)) +
  geom_treemap(alpha=0.7)  +
  geom_treemap_text(colour = "black", place = "centre") +
  theme(legend.position = "none") +
  scale_fill_manual(values = mycolors)

ggsave("treemap_families.png",
       width = 20,
       height = 12,
       units = 'cm'
)
