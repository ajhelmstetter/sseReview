
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
    'sse_model',
    'trait_level_1',
    'trait_level_2',
    'trait_level_3',
    'trait_level_4',
    'trait_level_5',
    'trait_level_6',
    'div_inc'
  )]

# Subset to only hidden state models
hidd<-c("HiSSE","MuHiSSE","GeoHiSSE","SecSSE")
df<-df[df$sse_model %in% hidd, ]

#make sure columns are the right type
df$trait_level_1 <- as.factor(df$trait_level_1)
df$trait_level_2 <- as.factor(df$trait_level_2)
df$trait_level_3 <- as.factor(df$trait_level_3)
df$trait_level_4 <- as.factor(df$trait_level_4)
df$trait_level_5 <- as.factor(df$trait_level_5)
df$trait_level_6 <- as.factor(df$trait_level_6)

#reduce to a single binary result per model per study
df <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

####
# TRAIT LEVEL 2
####

#set Multi-State results to 1
df$div_inc[df$div_inc > 1] <- 1

#count numbers of 0 and 1 per trait
df2 <- df %>% group_by(trait_level_2, div_inc, .drop=FALSE) %>% summarize(count=n()) %>% ungroup() %>% complete(trait_level_2, div_inc, fill = list(count = 0))

#make binary classification character
df2$div_inc<-as.character(df2$div_inc)

#color
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[3]))

#plot
l2<-ggplot(df2, aes(fill=div_inc, y=count, x=reorder(trait_level_2, -count))) +
  geom_bar(position="stack", stat="identity",alpha=0.85) +
  scale_x_discrete(name ="Trait category") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,30),
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
        legend.background = element_blank(),
        plot.margin = unit(c(1, 1, 1, 2),"lines")) +
  coord_cartesian(clip = "off") +
  annotation_custom(
    grob = textGrob(label = "(a)", hjust = 0, gp = gpar(cex = 1)),
    ymin = 30,      # Vertical position of the textGrob
    ymax = 30,
    xmin = -0.6,         # Note: The grobs are positioned outside the plot area
    xmax = -0.6)

l2

####
# TRAIT LEVEL 4
####

#count numbers of 0 and 1 per trait
df2 <- df %>% group_by(trait_level_4, div_inc, .drop=FALSE) %>% summarize(count=n()) %>% ungroup() %>% complete(trait_level_4, div_inc, fill = list(count = 0))

#make binary classification character
df2$div_inc<-as.character(df2$div_inc)

#colour
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[4]))

##
l4<-ggplot(df2, aes(fill=div_inc, y=count, x=reorder(trait_level_4, -count))) +
  geom_bar(position="stack", stat="identity",alpha=0.85) +
  scale_x_discrete(name ="Trait category") +
  scale_y_continuous(expand = c(0, 0),
                     limits=c(0,25),
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
        legend.background = element_blank(),
        plot.margin = unit(c(1, 1, 1, 2),"lines")) +
  coord_cartesian(clip = "off") +
  annotation_custom(
    grob = textGrob(label = "(b)", hjust = 0, gp = gpar(cex = 1)),
    ymin = 25,      # Vertical position of the textGrob
    ymax = 25,
    xmin = -1.75,         # Note: The grobs are positioned outside the plot area
    xmax = -1.75)


l4

###
# Combined
###

l2 / l4

ggsave("figures/stacked_barplot_traits_hidden_2and4.png",
       width = 20,
       height = 25,
       units = 'cm')

