###
# Frequency studied vs species in order
###

#Library
library(tidyverse)
library(ggplot2)
library(treemapify)
library(RColorBrewer)
library(ggrepel)
library(dplyr)

# theme
my_theme = theme(
  text = element_text(size = 10),
  axis.text = element_text(size = 10),
  axis.title.x = element_text(size = 11, margin = margin(
    t = 5,
    r = 0,
    b = 0,
    l = 0
  )),
  #axis.text.x=element_blank(),
  #axis.ticks.x=element_blank(),
  axis.line.x = element_line(),
  #axis.title.y = element_blank(),
  #axis.text.y = element_blank(),
  #axis.ticks.y = element_blank(),
  axis.line.y = element_line(),
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size = 10),
  legend.position = c(0.8, 0.9),
  panel.border = element_blank(),
  panel.grid.minor = element_line(colour = "grey", size = 0.15),
  panel.grid.major = element_line(colour = "grey", size = 0.3 ),
  panel.background = element_blank()
)

#read in full data frame
df <-
  read.csv("~/Dropbox/projects/AJH_DiveRS/sseReview/data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'order',
    'trait_level_1',
    'trait_level_2'
  )]

#make sure columns are the right type
df$order <- as.factor(df$order)

#unique combinations of study/order
so <- unique(df[c("study", "order")])

#redundant
#reduce to a single result per order per study
#
#so <-df_uni %>%
#  dplyr::count(order, study, .drop = T)
#so

#get frequency table
ord_freq<-data.frame(table(so$order))
colnames(ord_freq)<-c("order","freq")

# Number of species per order (JK extracted a few years ago from the APG website @ mobot):
# https://www.mobot.org/mobot/research/apweb/orders/boraginalesweb.htm

#read in data
ord_no<-read.csv("data/species_per_order.csv")

#get overlapping data in each dataset
ord_no<-ord_no[ord_no$order%in%ord_freq$order,]
ord_no<-ord_no[order(ord_no$order),]

#drop 'Multiple'
ord_no <- ord_no %>% filter(!grepl("Multiple", order))
ord_freq <- ord_freq %>% filter(!grepl("Multiple", order))

#check match
ord_freq$order==ord_no$order
setdiff(ord_freq$order,ord_no$order)

#add freq column to species numbers
ord_no$freq <- ord_freq$freq

#set theme
theme_set(my_theme)

#scatterplot
ggplot(ord_no, aes(x = no_species, y = freq)) +
  geom_point(alpha = 0.5, size = 2.5) +
  scale_x_continuous(name = "Number of species in order",labels = scales::comma) +
  scale_y_continuous(name = "Number of studies on clades in order")  +
  geom_text_repel(aes(label=ifelse(freq>5,as.character(order),'')))

ggsave("figures/scatterplot_orders_species_papers.png",
       width = 20,
       height = 12,
       units = 'cm'
)

