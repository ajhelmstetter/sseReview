###
# Model use
###

# Library
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(ggplot2)

#read in full data frame
df <-
  read.csv("~/Dropbox/projects/AJH_DiveRS/sse_review/sse_review_table - main_table.csv")

source(
  "https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R"
)

raincloud_theme = theme(
  text = element_text(size = 10),
  axis.text = element_text(size = 10),
  axis.title.x = element_text(size = 11, margin = margin(
    t = 20,
    r = 0,
    b = 0,
    l = 0
  )),
  #axis.text.x=element_blank(),
  #axis.ticks.x=element_blank(),
  #axis.line.x=element_blank(),
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size = 10),
  legend.position = c(0.85, 0.95),
  plot.title = element_text(
    lineheight = .8,
    face = "bold",
    size = 16
  ),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_blank()
)

colnames(df)

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
    "div_inc"
  )]

#make sure factors are factors
df$order <- as.factor(df$order)
df$trait_type_1 <- as.factor(df$trait_type_1)
df$sse_model <- as.factor(df$sse_model)
df$year <- as.factor(df$year)

#reduce to a single binary result per model per study
library(dplyr)
df3 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#make binary trait factor
df3$div_inc <- as.factor(df3$div_inc)

#functions to calculate boxplots
lb <- function(x)
  mean(x) - sd(x)
ub <- function(x)
  mean(x) + sd(x)

#summarise dataset
library(plyr)
sumld <-
  ddply(
    df3,
    ~ div_inc,
    summarise,
    mean = mean(tips),
    median = median(tips),
    lower = lb(tips),
    upper = ub(tips)
  )

head(sumld)

#raincloud plot
g <-
  ggplot(data = df3, aes(y = tips, x = div_inc, fill = div_inc)) +
  geom_flat_violin(position = position_nudge(x = .15, y = 0), alpha = .7) +
  geom_point(
    aes(y = tips, color = div_inc),
    position = position_jitter(width = .1),
    size = .5,
    alpha = 0.8
  ) +
  geom_boxplot(
    width = .1,
    guides = FALSE,
    outlier.shape = NA,
    alpha = 0.5
  ) +
  scale_y_continuous(trans = 'log10') +
  expand_limits(x = 3) +
  guides(fill = FALSE) +
  guides(color = guide_legend(override.aes = list(size=8))) +
  scale_color_brewer(palette = "Set1",
                     labels =
                       c("No effect inferred","Trait effect inferred")) +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  raincloud_theme + labs(y = "Number of tips in tree")

#plot
g
