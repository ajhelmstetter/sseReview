###
# Taxonomic diversity in SSE models
###

setwd("~/Dropbox/projects/AJH_DiveRS/sse_review/plots/")

#Library
library(tidyverse)
library(ggplot2)
library(treemapify)
library(RColorBrewer)

#read in full data frame
df <-
  read.csv("~/Dropbox/projects/AJH_DiveRS/sse_review/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'order',
    'trait_type_1',
    'trait_type_2'
  )]

#make sure columns are the right type
df$order <- as.factor(df$order)

#remove na
#df<-drop_na(df)

#unique combinations of study/order
df_uni<- unique(df[c("study", "order")])

#reduce to a single result per order per study
library(dplyr)
so <-df_uni %>%
  dplyr::count(order, study, .drop = F)
so

#get frequency table
ord_freq<-data.frame(table(so$order))
colnames(ord_freq)<-c("order","freq")

#colour palette
nb.cols <- length(levels(ord_freq$order))
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)

#plot treemap
ggplot(ord_freq, aes(area = freq, fill = order, label=order)) +
  geom_treemap(alpha=0.7)  +
  geom_treemap_text(colour = "black", place = "centre") +
  theme(legend.position = "none") +
  scale_fill_manual(values = mycolors)

ggsave("treemap_orders.pdf")

###
# Trait type diversity in SSE models
###


#unique combinations of study/trait type
df_uni<- unique(df[c("study", "trait_type_1")])

#reduce to a single result per trait_type_1 per study
library(dplyr)
so <-df_uni %>%
  dplyr::count(trait_type_1, study, .drop = F)
#so

#get frequency table
ord_freq<-data.frame(table(so$trait_type_1))
colnames(ord_freq)<-c("trait_type_1","freq")

#colour palette
nb.cols <- length(levels(ord_freq$trait_type_1))
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

#plot treemap
ggplot(ord_freq, aes(area = freq, fill = trait_type_1, label=trait_type_1)) +
  geom_treemap(alpha=0.7)  +
  geom_treemap_text(colour = "black", place = "centre") +
  theme(legend.position = "none") +
  scale_fill_manual(values = mycolors)

ggsave("treemap_traits.pdf")
