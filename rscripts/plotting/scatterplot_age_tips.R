###
# scatterplot of root age vs size of tree
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
  panel.grid.major = element_line(colour = "grey", size = 0.3),
  panel.background = element_blank()
)


#read in full data frame
df <-
  read.csv("~/Dropbox/projects/AJH_DiveRS/sseReview/data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c('study',
         'model_no',
         'div_inc',
         'age',
         'tips')]

#reduce to a single binary result per model per study
df <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#set MuSSE results to 1
df$div_inc[df$div_inc > 1] <- 1

#set div_inc to factor for plotting
df$div_inc <- as.factor(df$div_inc)

#remove NA (age not inferred or no tip information)
df<-na.omit(df)

#lm of trait-dependent results
summary(lm(log(tips) ~ age, data = df[df$div_inc == 1,]))

#lm of non-trait-dependent results
summary(lm(log(tips) ~ age, data = df[df$div_inc == 0,]))

#check plot
plot(age ~ log(tips), data = df[df$div_inc == 0,])

#theme
theme_set(my_theme)

#sit fill and color
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5, "Set2")[2]))
options(ggplot2.discrete.colour = c("#999999", brewer.pal(5, "Set2")[2]))

#root age vs tip number
ggplot(df, aes(
  x = tips,
  y = age,
  color = as.factor(div_inc),
  shape = as.factor(div_inc)
)) +
  geom_point(alpha = 0.8, size = 2.5) +
  geom_smooth(method = lm, linetype = "dashed", aes(fill = as.factor(div_inc))) +
  scale_x_continuous(name = "Number of tips", trans = "log10") +
  scale_y_continuous(name = "Root age (Ma)") +
  stat_regline_equation(label.y = 318,
                        aes(label = ..rr.label..[1]),
                        colour = "#999999") +
  stat_regline_equation(label.y = 310, aes(label = ..rr.label..[2])) +
  theme(
    legend.text = element_text(size = 9),
    legend.position = c(.875, .95),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.background = element_blank(),
    panel.grid.minor = element_line(),
    panel.grid.major = element_line()
  ) + guides(color = guide_legend(override.aes = list(
    fill = NA,
    linetype = c(0, 0),
    label = ""
  )))

ggsave(
  "figures/scatterplot_age_tips.png",
  width = 20,
  height = 20,
  units = 'cm'
)
