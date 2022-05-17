###
# Effect of data characteristics on inference of div rate change
###

# Library
library(viridis)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(patchwork)
library(dplyr)
library(RColorBrewer)

# theme
my_theme = theme(
  text = element_text(size = 12),
  axis.text = element_text(size = 12),
  axis.title.x = element_text(size = 18, margin = margin(
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
  #axis.line.y = element_blank(),
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size = 12),
  legend.position = c(0.8, 0.9),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank()
)


#read in full data frame
df <-
  read.csv("data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'sse_model',
    'div_inc'
  )]

#reduce to a single binary result per model per study
df <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#set Multi-State results to 1
df$div_inc[df$div_inc > 1] <- 1

#count numbers of 0 and 1 per trait
df2 <- df %>% group_by(sse_model, div_inc) %>% summarize(count=n())

#make binary classification character
df2$div_inc<-as.character(df2$div_inc)

#remove 'flora' as not a useful taxonomic level
#df2<-df2[!(df2$level=="Flora"),]

#color
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[1]))
theme_set(my_theme)

#plot
l2<-ggplot(df2, aes(fill=div_inc, y=count, x=reorder(sse_model, -count))) +
  geom_bar(position="stack", stat="identity",alpha=0.85) +
  scale_x_discrete(name ="SSE model") +
  scale_y_continuous(name ="Number of times tested") +
  scale_fill_discrete(labels = c("No effect", "Effect")) +
  theme(axis.text.x = element_text(angle = 90,vjust=-0.05,hjust = 1),
        axis.ticks.x = element_blank(),
        #axis.ticks.y = element_blank(),
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
  "figures/stacked_barplot_ssemodel.png",
  width = 15,
  height = 15,
  units = 'cm'
)

