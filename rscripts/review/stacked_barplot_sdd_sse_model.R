rm(list = ls())
library(tidyr)
library(dplyr)
library(ggplot2)

#read in data
df <- read.csv("data/sse_review_table - main_table.csv")

#####
# Proportion of BiSSE models that are associated with div inc
#####

#reduce to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

dfx <- df[df$sse_model=="BiSSE",] %>%
  group_by(study, model_no) %>%
  summarize(count=n())

dfb<-df[df$sse_model=="BiSSE",]
write.csv(dfb$study[dfb$div_inc==1],"outputs/dfb.csv")

df2b<-df2[df2$sse_model=="BiSSE",]
write.csv(df2b$study[df2b$div_inc==1],"outputs/df2b.csv")

#set Multi-State results to 1
df2$div_inc[df2$div_inc > 1] <- 1

#div_inc results per SSE model
df3<-df2 %>% group_by(sse_model, div_inc, .drop=FALSE) %>%
  summarize(count=n()) %>%
  mutate(perc = count/sum(count)) %>%
  ungroup()  %>%
  complete(sse_model, div_inc, fill = list(count = 0))

#add column for locations of numbers in figure
df4<-df3 %>%
  group_by(sse_model) %>%
  arrange(sse_model, desc(div_inc)) %>%
  mutate(lab_ypos = cumsum(perc) - 0.5 * perc)

#fix NA and scale issue
df4$perc[is.na(df4$perc)]<-0
df4$div_inc<-as.character(df4$div_inc)

#not sure why its not working if results are only no effect (MiSSE/SecSSE), but here is a bad fix
df4$lab_ypos[16]<-0.5 #MiSSE
df4$lab_ypos[24]<-0.5 #SecSSE

#color
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[3]))

ggplot(df4, aes(fill=div_inc, y=perc, x=sse_model)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(y = lab_ypos, label = count, group = div_inc), color = "white") +
  #facet_grid(~ trait_level_2) +
  scale_y_continuous(expand = c(0.01, 0),
                     name ="Proportion of models detecting TDD") +
  scale_x_discrete(name ="SSE model") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("figures/review/stacked_barplot_tdd_sse_model.png",
       width = 20,
       height = 15,
       units = 'cm')
