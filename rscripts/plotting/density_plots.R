###
# Effect of data characteristics on inference of div rate change
###

setwd("~/Dropbox/projects/AJH_DiveRS/sseReview/rscripts/")

# Library
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(patchwork)
library(dplyr)
library(RColorBrewer)

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
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y = element_blank(),
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size = 10),
  legend.position = c(0.8, 0.9),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank()
)

#palette
options(ggplot2.discrete.fill = c("#999999", "#E69F00"))

#read in full data frame
df <-
  read.csv("~/Dropbox/projects/AJH_DiveRS/sseReview/data/sse_review_table - main_table.csv")

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
    'perc_sampling',
    'samples_per_state',
    'div_inc'
  )]

#make sure columns are the right type
df$order <- as.factor(df$order)
df$trait_type_1 <- as.factor(df$trait_type_1)
df$sse_model <- as.factor(df$sse_model)
df$year <- as.factor(df$year)
df$perc_sampling <- as.numeric(df$perc_sampling)
df$samples_per_state <- as.numeric(df$samples_per_state)

#remove na
#df<-drop_na(df)

#make samples per state into tip bias
df$samples_per_state

#make column with combination of study and model
tmp_df <- df %>% tidyr::unite("study_model", 1:2, remove = T)

#reduce dataset to two columns
tmp_df <- tmp_df[, c("study_model", "samples_per_state")]

head(tmp_df)

#get max and min values of samples per state for each model
top_df <-
  tmp_df %>% group_by(study_model) %>% slice_max(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)
bot_df <-
  tmp_df %>% group_by(study_model) %>% slice_min(n = 1,
                                                 order_by = samples_per_state,
                                                 with_ties = F)

#reduce to a single binary result per model per study
df3 <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#make binary trait factor
df3$div_inc <- as.factor(df3$div_inc)

#make sampling fraction %
df3$perc_sampling <- df3$perc_sampling * 100

#add tip bias column by dividing larger number of tips with state A by smaller number of tips with state B
#multi-state models are therefore largest tip bias possible in the data
df3$tip_bias <- top_df$samples_per_state / bot_df$samples_per_state

###
# plot densities
###

theme_set(my_theme)

#tips
p1 <-
  ggplot(df3, aes(log(tips), fill = div_inc, colour = div_inc)) +
  geom_density(alpha = 0.5, color = NA) +
  scale_x_continuous(name = "Number of tips (log)") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_discrete(labels = c("No effect", "Effect"))

#age
p2 <- ggplot(df3, aes(log(age), fill = div_inc, colour = div_inc)) +
  geom_density(alpha = 0.5, color = NA) + theme(legend.position = "none") +
  scale_x_continuous(name = "Age of tree (log)") +
  scale_y_continuous(expand = c(0, 0))

#perc_sampling
p3 <-
  ggplot(df3, aes(perc_sampling, fill = div_inc, colour = div_inc)) +
  geom_density(alpha = 0.5, color = NA) + theme(legend.position = "none") +
  scale_x_continuous(name = "Sampling fraction (%)") +
  scale_y_continuous(expand = c(0, 0))

#tip bias
p4 <- ggplot(df3, aes(tip_bias, fill = div_inc, colour = div_inc)) +
  geom_density(alpha = 0.5, color = NA) + theme(legend.position = "none") +
  scale_x_continuous(name = "Tip bias", limits = c(0, 30)) +
  scale_y_continuous(expand = c(0, 0))

#number of markers
p5 <-
  ggplot(df3, aes(log(no_markers), fill = div_inc, colour = div_inc)) +
  geom_density(alpha = 0.5, color = NA) + theme(legend.position = "none") +
  scale_x_continuous(name = "Number of markers (log)") +
  scale_y_continuous(expand = c(0, 0))

#arrange plots
p1 | p2 / p3 | p4 / p5

ggsave("densities.pdf",
       width = 20,
       height = 12,
       units = 'cm')

###
# ridgeplots
###

theme_set(theme_minimal())

#remove models where density cant be calculated
remove.list <-
  paste(c("SecSSE", "MuHiSSE", "MiSSE", "BiSSEness"), collapse = '|')
df4 <- df3 %>% filter(!grepl(remove.list, sse_model))

#tips vs type of SSE model
# Cut off the trailing tails.
# Specify `rel_min_height`: a percent cutoff
options(ggplot2.discrete.fill = brewer.pal(9, "PiYG"))

ggplot(df4, aes(
  x = log(tips),
  y = sse_model,
  group = sse_model,
  fill = sse_model
)) +
  geom_density_ridges(rel_min_height = 0.0001,
                      alpha = 0.9,
                      color = NA) +
  scale_x_continuous(name = "Number of tips (log)") +
  theme(legend.position = "none", axis.title.y = element_blank())

ggsave(
  "ridgeplot_tips_model.pdf",
  width = 20,
  height = 12,
  units = 'cm'
)

#remove years where density cant be calculated
df5 <- df3 %>% filter(!grepl("2010", year))

options(ggplot2.discrete.fill = brewer.pal(11, "RdYlBu"))
#tips vs publication year
ggplot(df5, aes(
  x = log(tips),
  y = year,
  group = year,
  fill = year
)) +
  geom_density_ridges(rel_min_height = 0.0001,
                      alpha = 0.9,
                      color = NA) +
  scale_x_continuous(name = "Number of tips (log)") +
  scale_y_discrete(name = "Publication year") +
  theme(legend.position = "none")

ggsave(
  "ridgeplot_tips_year.pdf",
  width = 20,
  height = 12,
  units = 'cm'
)

###
# scatterplots
###

theme_set(theme_classic())
options(ggplot2.discrete.fill = c("#999999", "#E69F00"))

#sampling fraction vs tip number
ggplot(df3, aes(x = perc_sampling, y = log(tips), color = div_inc)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_smooth(method = lm, linetype = "dashed", aes(fill = div_inc)) +
  scale_x_continuous(name = "Sampling fraction (%)") +
  scale_y_continuous(name = "Number of tips (log)") +
  theme(
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.8, 0.9),
    panel.grid.minor = element_line(),
    panel.grid.major = element_line()
  )

ggsave(
  "scatterplot_sampling_tips.pdf",
  width = 20,
  height = 12,
  units = 'cm'
)
