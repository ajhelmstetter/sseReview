
#Library
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

#theme
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[1]))

#read in full data frame
df <-
  read.csv("data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'order',
    'trait_level_1',
    'trait_level_2',
    'trait_level_3',
    'trait_level_4',
    'trait_level_5',
    'sse_model',
    'tips',
    'year',
    'no_markers',
    'age',
    'age_inferred',
    'perc_sampling',
    'samples_per_state',
    'div_inc',
    'div_eq',
    'div_dec'
  )]

#make sure columns are the right type
df$order <- as.factor(df$order)
df$trait_level_1 <- as.factor(df$trait_level_1)
df$trait_level_2 <- as.factor(df$trait_level_2)
df$trait_level_3 <- as.factor(df$trait_level_3)
df$trait_level_4 <- as.factor(df$trait_level_4)
df$trait_level_5 <- as.factor(df$trait_level_5)
df$sse_model <- as.factor(df$sse_model)
df$year <- as.factor(df$year)
df$perc_sampling <- as.numeric(df$perc_sampling)
df$samples_per_state <- as.numeric(df$samples_per_state)


#get counts of model use per trait category
agg_st_mod<-aggregate(df$div_inc,by=list(df$study,df$model_no,df$trait_level_1),FUN=sum)
#view(agg_st_mod)

#WHY NA?
#remove NA
agg_st_mod<-na.omit(agg_st_mod)

#make multistate models with more than one positive effect 1
agg_st_mod[agg_st_mod$x>1,]$x<-1

#count numbers of 0 and 1 per trait
df2 <-agg_st_mod %>% group_by(Group.3, x) %>% summarize(count=n())

#make binary classification character
df2$x<-as.character(df2$x)

#UNCOMMENT TO :
#generate missing combinations and add

#mc<-data.frame(c("Fire","Soil","Symbiosis"),
#      c("1","1","0"),
#      c(0,0,0))
#colnames(mc)<-colnames(df2)
#df2<-rbind(df2,mc)


ggplot(df2, aes(fill=x, y=count, x=reorder(Group.3, -count))) +
  geom_bar(position="stack", stat="identity") +
  scale_x_discrete(name ="Trait category") +
  scale_y_continuous(expand = c(0, 0),
                     #limits=c(0,80),
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

ggsave(
  "figures/stacked_barplot_traits_level2.png",
  width = 20,
  height = 12,
  units = 'cm'
)
