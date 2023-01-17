###
# Effect of data characteristics on inference of div rate change
###
rm(list=ls())

# Library
#library(viridis)
#library(hrbrthemes)
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
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y = element_blank(),
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
    'order',
    'trait_level_1',
    'sse_model',
    'tips',
    'year',
    'no_markers',
    'age',
    'age_inferred',
    'perc_sampling',
    'samples_per_state',
    'div_inc'
  )]

#make sure columns are the right type
df$order <- as.factor(df$order)
df$trait_level_1 <- as.factor(df$trait_level_1)
df$sse_model <- as.factor(df$sse_model)
df$year <- as.factor(df$year)
df$perc_sampling <- as.numeric(df$perc_sampling)
df$samples_per_state <- as.numeric(df$samples_per_state)

#sort by study name
df<-df[order(df$study),]

####
#make samples per state into tip bias
####

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

#reduce entire dataset to a single binary result per model per study
df2 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc)) #if 1 is present in comparison there are rate differences, if absent there aren't

#not run
#if multistate models are removed, number of div_inc = 1 should be the same
#df3 <- df[df$sse_model != "MuSSE",]
#df3 <- df3[df3$sse_model != "SecSSE",]
#df3 <- df3[df3$sse_model != "MuHiSSE",]
#df4 <- df2[df2$sse_model != "MuSSE",]
#df4 <- df4[df4$sse_model != "SecSSE",]
#df4 <- df4[df4$sse_model != "MuHiSSE",]
#table(df3$div_inc)
#table(df4$div_inc)

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

###
# plot densities
###

theme_set(my_theme)

options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[1]))

#tips
p1 <-
  ggplot(df2, aes(tips, fill = div_inc, colour = div_inc)) +
  geom_density(alpha = 0.5, color = NA) +
  scale_x_continuous(name = "Number of tips",  trans = "log10") +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.5)) +
  scale_fill_discrete(labels = c("No effect", "Effect")) +
  annotate(geom="text", x=10, y=1.475, label="(a)",size=6)
p1

#number of measurements
length(na.omit(df2$tips))

options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[2]))

#age
p2 <- ggplot(df2, aes(age, fill = div_inc, colour = div_inc)) +
  geom_density(alpha = 0.5, color = NA) +
  scale_x_continuous(name = "Age of tree (Ma)",  trans = "log10") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_discrete(labels = c("No effect", "Effect")) +
  theme(legend.position = "none") +
  annotate(geom="text", x=1, y=1.48, label="(b)",size=6)
p2

#number of measurements
length(na.omit(df2$age))

#perc_sampling
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[3]))

p3 <-
  ggplot(df2, aes(perc_sampling, fill = div_inc, colour = div_inc)) +
  geom_density(alpha = 0.5, color = NA) +
  scale_x_continuous(name = "Sampling fraction (%)") +
  scale_y_continuous(expand = c(0, 0),limits=c(0,0.02)) +
  scale_fill_discrete(labels = c("No effect", "Effect")) +
  theme(legend.position = "none") +
  annotate(geom="text", x=0, y=0.0195, label="(e)",size=6)
p3

#number of measurements
length(na.omit(df2$perc_sampling))

#tip bias
#there are some extreme tip values that are fall outside the scale
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[4]))

#get rid of studies where there are inf values for tip bias
df3<-as.data.frame(df2)
df3<-df3[df3$tip_bias<100000,]

p4 <- ggplot(df3, aes(tip_bias, fill = div_inc, colour = div_inc)) +
  geom_density(alpha = 0.5, color = NA) +
  scale_x_continuous(name = "Tip bias",  trans = "log10") +
  scale_y_continuous(expand = c(0, 0),limits = c(0,1.5)) +
  scale_fill_discrete(labels = c("No effect", "Effect")) +
  theme(legend.position = "none") +
  annotate(geom="text", x=1, y=1.45, label="(d)",size=6)

p4

#number of measurements
length(na.omit(df3$tip_bias))

#number of markers
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[5]))

p5 <-
  ggplot(df2, aes(no_markers, fill = div_inc, colour = div_inc)) +
  geom_density(alpha = 0.5, color = NA) +
  scale_x_continuous(name = "Total number of markers",  trans = "log10") +
  scale_y_continuous(expand = c(0, 0),limits = c(0,2)) +
  scale_fill_discrete(labels = c("No effect", "Effect")) +
  theme(legend.position = "none") +
  annotate(geom="text", x=1, y=1.9, label="(c)",size=6)

p5

#number of measurements
length(na.omit(df2$no_markers))

###
#arrange plots together
###

p1 | p2 / p4 | p5 / p3

ggsave("figures/densities_all.png",
       width = 30,
       height = 20,
       units = 'cm')

#pdf for publication
ggsave("figures/densities_all.pdf",
       width = 30,
       height = 20,
       units = 'cm')

#undo theme
theme_set(theme_bw())
