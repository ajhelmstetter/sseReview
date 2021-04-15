###
# Taxonomic diversity in SSE models
###

setwd("~/Dropbox/projects/AJH_DiveRS/sseReview/figures/")

#Library
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

#theme


#read in full data frame
df <-
  read.csv("~/Dropbox/projects/AJH_DiveRS/sseReview/data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'order',
    'trait_type_1',
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
df$trait_type_1 <- as.factor(df$trait_type_1)
df$sse_model <- as.factor(df$sse_model)
df$year <- as.factor(df$year)
df$perc_sampling <- as.numeric(df$perc_sampling)
df$samples_per_state <- as.numeric(df$samples_per_state)

#traits with any div change
df$div_chg<-df$div_inc+df$div_dec

#get counts of model use per year
ym<-df %>%
  dplyr::count(trait_type_1,div_chg, .drop = F)
ym

agg_st_mod<-aggregate(df$div_chg,by=list(df$study,df$model_no,df$trait_type_1),FUN=sum)
#view(agg_st_mod)

#make multistate models with more than one positive effect 1
agg_st_mod[agg_st_mod$x>1,]$x<-1

#count numbers of 0 and 1 per trait
df2 <-agg_st_mod %>% group_by(Group.3, x) %>% summarize(count=n())

#missing combinations
mc<-data.frame(c("Fire","Soil","Symbiosis"),
      c("1","1","0"),
      c(0,0,0))

colnames(mc)<-colnames(df2)

df2$x<-as.character(df2$x)

df2<-rbind(df2,mc)


ggplot(df2, aes(fill=x, y=count, x=reorder(Group.3, -count))) +
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete(name ="Trait category") +
  scale_y_continuous(expand = c(0, 0),
                     limits=c(0,50),
                     name ="Count") +
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
  "~/Dropbox/projects/AJH_DiveRS/sseReview/figures/grouped_barplot_traits.pdf",
  width = 20,
  height = 12,
  units = 'cm'
)
