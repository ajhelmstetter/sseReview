rm(list=ls())
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
    'tips',
    'perc_sampling',
    'samples_per_state',
    'div_inc'
  )]

#make sure columns are the right type
df$trait_level_2 <- as.factor(df$trait_level_2)

###
# Make samples per state into tip bias
###

#get max and min values of samples per state for each model
top_df <-
  df %>% group_by(study, model_no) %>% slice_max(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)

head(top_df)

bot_df <-
  df %>% group_by(study, model_no) %>% slice_min(n = 1,
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
top_df$study==bot_df$study
top_df$model_no==bot_df$model_no

df2$model_no==bot_df$model_no
df2$model_no==top_df$model_no

df2$study==bot_df$study
df2$study==top_df$study

#add tip bias column by dividing larger number of tips with state A by smaller number of tips with state B
#multi-state models are therefore largest tip bias possible in the data
df2$tip_bias <- top_df$samples_per_state / bot_df$samples_per_state

# df2 = all data
df2<-na.omit(df2)
#df2<-droplevels(df2)

# df3 = relaxed filtering
df3<-df2[df2$tips>=100,]
df3<-df3[df3$perc_sampling>=10,]
df3<-df3[df3$tip_bias<=20,]
#df3<-na.omit(df3)
#df3<-droplevels(df3)

# df4 = strict filtering
df4<-df2[df2$tips>=300,]
df4<-df4[df4$perc_sampling>=25,]
df4<-df4[df4$tip_bias<=10,]
#df4<-na.omit(df4)
#df4<-droplevels(df4)

#count and percentages of 0 and 1 per trait
df2 <- df2 %>% group_by(sse_model, div_inc, .drop=FALSE) %>% summarize(count=n()) %>%   mutate(perc = count/sum(count)) %>% ungroup()  %>% complete(sse_model, div_inc, fill = list(count = 0))

df3 <- df3 %>% group_by(sse_model, div_inc, .drop=FALSE) %>%
  summarize(count=n()) %>%
  mutate(perc = count/sum(count)) %>%
  ungroup()  %>%
  complete(sse_model, div_inc, fill = list(count = 0))

#fix NA issue
df3$perc[is.na(df3$perc)]<-0

df4 <- df4 %>% group_by(sse_model, div_inc, .drop=FALSE) %>% summarize(count=n()) %>%   mutate(perc = count/sum(count)) %>% ungroup()  %>% complete(sse_model, div_inc, fill = list(count = 0))

#make binary classification character
df2$div_inc<-as.character(df2$div_inc)
df3$div_inc<-as.character(df3$div_inc)
df4$div_inc<-as.character(df4$div_inc)

#combine dfs
df_all<-rbind(df2,df3,df4)
df_all$filter<-c(rep("none",length(rownames(df2))),
                 rep("relaxed",length(rownames(df3))),
                 rep("strict",length(rownames(df4))))

df_all_2<-df_all %>%
  group_by(sse_model, filter) %>%
  arrange(filter, desc(div_inc)) %>%
  mutate(lab_ypos = cumsum(perc) - 0.5 * perc)

df_all_2$lab_ypos[df_all_2$count==0] <- NA

#not sure why its not working if results are only no effect (MiSSE/SecSSE), but here is a bad fix
#df4$lab_ypos[16]<-0.5 #MiSSE
#df4$lab_ypos[24]<-0.5 #SecSSE

#color
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[3]))

ggplot(df_all_2, aes(fill=div_inc, y=perc, x=filter)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(y = lab_ypos, label = count, group = div_inc), color = "white") +
  facet_grid(~ sse_model) +
  scale_y_continuous(expand = c(0.01, 0),
                     name ="Percentage") +
  scale_x_discrete(name ="Filtering severity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("figures/review/stacked_grouped_barplot_tdd_sse_model.png",
       width = 25,
       height = 10,
       units = 'cm')
