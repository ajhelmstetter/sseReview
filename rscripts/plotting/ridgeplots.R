###
# ridgeplots
###

# Library
library(tidyverse)
library(ggplot2)
library(ggridges)
library(dplyr)
library(RColorBrewer)

#read in full data frame
df <-
  read.csv("data/sse_review_table - main_table.csv")

colnames(df)

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c('study',
         'model_no',
         'order',
         'sse_model',
         'tips',
         'perc_sampling',
         'year',
         'div_inc')]

#make sure columns are the right type
df$sse_model <- as.factor(df$sse_model)
df$year <- as.factor(df$year)

#reduce to a single binary result per model per study
df3 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#remove year where density cant be calculated
df4 <- df3 %>% filter(!grepl("2010", year))

#####
# Tips vs publication year
#####

theme_set(theme_minimal())

options(ggplot2.discrete.fill = brewer.pal(11, "RdYlBu"))

ggplot(df4, aes(
  x = tips,
  y = year,
  group = year,
  fill = year,
)) +
  geom_density_ridges(rel_min_height = 0.0001,
                      alpha = 0.9,
                      color = NA) +
  scale_x_continuous(name = "Number of tips",
                     trans = 'log10',
                     labels = scales::comma) +
  scale_y_discrete(name = "Publication year") +
  theme(
    legend.position = "none",
    axis.title.x = element_text(margin = margin(
      t = 10,
      r = 0,
      b = 0,
      l = 0
    )),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    ))
  )

ggsave(
  "figures/ridgeplot_tips_year.png",
  width = 20,
  height = 12,
  units = 'cm'
)

#####
# Sampling fraction vs publication year
#####

options(ggplot2.discrete.fill = brewer.pal(11, "PiYG"))

ggplot(df4, aes(
  x = perc_sampling,
  y = year,
  group = year,
  fill = year
)) +
  geom_density_ridges(rel_min_height = 0.001,
                      alpha = 0.9,
                      color = NA) +
  scale_x_continuous(name = "Sampling Fraction", limits = c(0, 1)) + # Cut off the trailing tails. Specify `rel_min_height`: a percent cutoff
  scale_y_discrete(name = "Publication year") +
  theme(legend.position = "none")

ggsave(
  "figures/ridgeplot_samplingfraction_year.png",
  width = 20,
  height = 12,
  units = 'cm'
)
