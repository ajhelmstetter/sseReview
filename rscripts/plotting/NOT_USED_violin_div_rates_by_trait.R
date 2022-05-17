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
  axis.title.x = element_text(size = 14, margin = margin(
    t = 5,
    r = 0,
    b = 0,
    l = 0
  )),
  #axis.text.x=element_blank(),
  #axis.ticks.x=element_blank(),
  axis.line.x = element_line(),
  axis.title.y = element_text(angle=90, size = 14, margin = margin(
    t = 0,
    r = 20,
    b = 0,
    l = 0
  )),
  axis.text.y = element_text(),
  axis.ticks.y = element_line(),
  axis.line.y = element_line(),
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size = 12),
  legend.position = c(0.8, 0.9),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank()
)

theme_set(my_theme)

#read in full data frame
df <-
  read.csv("data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'trait_level_1',
    'trait_level_2',
#    'trait_level_3',
#    'trait_level_4',
    'div_inc',
    'div_rate',
    'age_inferred'
  )]

#only those where age was inferred
df<-df[df$age_inferred==1,]

#only those rows with div rate values (not NA)
df<-df[!is.na(df$div_rate),]

#log transform div rate
#df$div_rate<-log(df$div_rate)

#####
# Boxplots trait level 1
#####

ggplot(df, aes(
  x = factor(trait_level_1),
  y = div_rate,
  fill = trait_level_1
)) + geom_boxplot(outlier.shape = NA, width = 0.2) +
  geom_violin(width=0.75) +
  geom_jitter(
    aes(color = trait_level_1),
    size = 1,
    alpha = 0.5,
    width = 0.05,
    height = 0
  ) +
  theme(legend.position = "none",
        axis.title.x = element_text(vjust = 0.3)) +
  scale_y_continuous(name = "Net diversification rate (species / Ma)", trans="log10") +
  scale_x_discrete(name = "Trait level 1") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.3) +
  scale_color_viridis(discrete = TRUE, alpha = 0.7)

ggsave("figures/violinplot_divrates_trait_level_1.png",
       width = 20,
       height = 15,
       units = 'cm')


#####
# Boxplots trait level 2
#####

ggplot(df, aes(
  x = factor(trait_level_2),
  y = div_rate,
  fill = trait_level_2
)) + geom_boxplot(outlier.shape = NA, width = 0.2) +
  geom_violin(width=0.75) +
  geom_jitter(
    aes(color = trait_level_2),
    size = 1,
    alpha = 0.5,
    width = 0.05,
    height = 0
  ) +
  theme(legend.position = "none",
        axis.title.x = element_text(vjust = 0.3),
        axis.text = element_text(angle=90)) +
  scale_y_continuous(name = "Net diversification rate (species / Ma)", trans="log10") +
  scale_x_discrete(name = "Trait level 2") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.3) +
  scale_color_viridis(discrete = TRUE, alpha = 0.7)

ggsave("figures/violinplot_divrates_trait_level_2.png",
       width = 25,
       height = 15,
       units = 'cm')
