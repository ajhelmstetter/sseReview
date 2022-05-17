
#Library
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(patchwork)
library(grid)

#read in full data frame
df <-
  read.csv("data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'order',
    'clade',
    'level',
    'div_inc'
  )]

#make sure columns are the right type
df$order <- as.factor(df$order)
df$level <- as.factor(df$level)

#reduce to a single binary result per model per study
df <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#set Multi-State results to 1
df$div_inc[df$div_inc > 1] <- 1

#count numbers of 0 and 1 per trait
df2 <- df %>% group_by(level, div_inc) %>% summarize(count=n())

#make binary classification character
df2$div_inc<-as.character(df2$div_inc)

#remove 'flora' as not a useful taxonomic level
df2<-df2[!(df2$level=="Flora"),]

#color
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[1]))

#reorder by taxonomic level
df2 <-df2 %>% mutate(level = fct_relevel(level,
                                                 "Genus",
                                                 "Tribe",
                                                 "Subfamily",
                                                 "Family",
                                                 "Order",
                                                 "CAO" ))

#plot
l2<-ggplot(df2, aes(fill=div_inc, y=count, x=level)) +
  geom_bar(position="stack", stat="identity",alpha=0.85) +
  scale_x_discrete(name ="Taxonomic level of clade") +
  scale_y_continuous(expand = c(0, 0),
                     limits=c(0,300),
                     name ="Number of times tested") +
  scale_fill_discrete(labels = c("No effect", "Effect")) +
  theme(axis.text.x = element_text(angle = 90,vjust=-0.05,hjust = 1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_line(colour="grey"),
        panel.grid.major.y = element_line(colour="grey"),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = c(0.89,0.91),
        legend.background = element_blank())

l2

ggsave(
  "figures/stacked_barplot_taxonomy.png",
  width = 15,
  height = 15,
  units = 'cm'
)
