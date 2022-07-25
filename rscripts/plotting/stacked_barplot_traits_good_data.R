
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
    'trait_level_2',
    'trait_level_4',
    'tips',
    'perc_sampling',
    'samples_per_state',
    'div_inc'
  )]

# Subset to only thos models with good data
#df<-df[df$sse_model %in% hidd, ]

#make sure columns are the right type
df$trait_level_2 <- as.factor(df$trait_level_2)
df$trait_level_4 <- as.factor(df$trait_level_4)

####
#make samples per state into tip bias
####

#make column with combination of study and model for easy categorization
tmp_df <- df %>% tidyr::unite("study_model", 1:2, remove = T)

#reduce dataset to two columns
tmp_df <- tmp_df[, c("study_model", "samples_per_state")]
head(tmp_df)

tmp_df %>% group_by(study_model)

#get max and min values of samples per state for each model
top_df <-
  tmp_df %>% group_by(study_model) %>% slice_max(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)

head(top_df)

bot_df <-
  tmp_df %>% group_by(study_model) %>% slice_min(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)

head(bot_df)

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#make div_inc values > 1 (multistate models) = 1
df2$div_inc[df2$div_inc>1]<-1

#make binary trait factor
df2$div_inc <- as.factor(df2$div_inc)

#make sampling fraction %
df2$perc_sampling <- df2$perc_sampling * 100

#check that orders of df2 and top/bot_df match up
setdiff(top_df$study_model,bot_df$study_model)
setdiff(bot_df$study_model,top_df$study_model)
table(df2$study==gsub('(.*)_\\w+', '\\1',top_df$study_model))

#add tip bias column by dividing larger number of tips with state A by smaller number of tips with state B
#multi-state models are therefore largest tip bias possible in the data
df2$tip_bias <- top_df$samples_per_state / bot_df$samples_per_state

#apply recommended filtering
df2<-df2[df2$tips>300,]
df2<-df2[df2$perc_sampling>0.25,]
df2<-df2[df2$tip_bias>10,]

df2<-na.omit(df2)
df2<-droplevels(df2)

####
# TRAIT LEVEL 2
####

#count numbers of 0 and 1 per trait
df3 <- df2 %>% group_by(trait_level_2, div_inc, .drop=FALSE) %>% summarize(count=n()) %>% ungroup() %>% complete(trait_level_2, div_inc, fill = list(count = 0))

#make binary classification character
df3$div_inc<-as.character(df3$div_inc)

#color
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[3]))

#plot
l2<-ggplot(df3, aes(fill=div_inc, y=count, x=reorder(trait_level_2, -count))) +
  geom_bar(position="stack", stat="identity",alpha=0.85) +
  scale_x_discrete(name ="Trait category") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,10),
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
df3 <- df2 %>% group_by(trait_level_4, div_inc, .drop=FALSE) %>% summarize(count=n()) %>% ungroup() %>% complete(trait_level_4, div_inc, fill = list(count = 0))

#make binary classification character
df3$div_inc<-as.character(df3$div_inc)

#colour
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[4]))

##
l4<-ggplot(df3, aes(fill=div_inc, y=count, x=reorder(trait_level_4, -count))) +
  geom_bar(position="stack", stat="identity",alpha=0.85) +
  scale_x_discrete(name ="Trait category") +
  scale_y_continuous(expand = c(0, 0),
                     limits=c(0,10),
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

ggsave("figures/review/stacked_barplot_traits_good_data_2and4.png",
       width = 20,
       height = 25,
       units = 'cm')

