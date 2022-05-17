###
# scatterplot of sampling fraction vs total number of species in clade
###

#Library
library(tidyverse)
library(ggplot2)
library(treemapify)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)

# theme
my_theme = theme(
  text = element_text(size = 10),
  axis.text = element_text(size = 10),
  axis.title.x = element_text(size = 11, margin = margin(
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
  axis.line.y = element_line(),
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size = 10),
  legend.position = c(0.1, 0.95),
  panel.border = element_blank(),
  panel.grid.minor = element_line(colour = "grey", size = 0.15),
  panel.grid.major = element_line(colour = "grey", size = 0.3 ),
  panel.background = element_blank()
)

#read in full data frame
df <-
  read.csv("~/Dropbox/projects/AJH_DiveRS/sseReview/data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'div_inc',
    'perc_sampling',
    'tips'
  )]

#make div_inc values > 1 (multistate models) = 1
df$div_inc[df$div_inc>1]<-1
table(df$div_inc)

#reduce to a single binary result per model per study
df <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#lm of trait-dependent results
summary(lm(log(tips) ~ asin(sqrt(perc_sampling)), data = df[df$div_inc == 1, ]))

#lm of non-trait-dependent results
summary(lm(log(tips) ~ asin(sqrt(perc_sampling)), data = df[df$div_inc == 0, ]))

#theme
theme_set(my_theme)

#color
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5,"Set2")[4]))
options(ggplot2.discrete.colour = c("#999999", brewer.pal(5,"Set2")[4]))

#sampling fraction vs tip number
ggplot(df, aes(x = tips, y = asin(sqrt(perc_sampling)), color = as.factor(div_inc), shape = as.factor(div_inc))) +
  geom_point(alpha = 0.8, size = 2.5) +
  geom_smooth(method = lm, linetype = "dashed", aes(fill = as.factor(div_inc))) +
  scale_x_continuous(name = "Number of tips", trans='log10') +
  scale_y_continuous(name = "Sampling fraction (arcsine)",limits=c(0,1.75)) +
  stat_regline_equation(label.y = 1.75,
                        aes(label = ..rr.label..[1]),
                        colour = "#999999") +
  stat_regline_equation(label.y = 1.7, aes(label = ..rr.label..[2])) +
  theme(
    legend.text = element_text(size = 9),
    legend.position = c(.875, .95),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.background = element_blank(),
    panel.grid.minor = element_line(),
    panel.grid.major = element_line()
  ) + guides(color = guide_legend(override.aes = list(
    fill = NA, linetype = c(0, 0), label = ""
  )))

ggsave(
  "figures/scatterplot_sampling_tips.png",
  width = 20,
  height = 20,
  units = 'cm'
)
