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
  #legend.key = element_blank(),
  #legend.title = element_blank(),
  #legend.text = element_text(size = 10),
  #legend.position = c(0.1, 0.95),
  panel.border = element_blank(),
  panel.grid.minor = element_line(colour = "grey", size = 0.15),
  panel.grid.major = element_line(colour = "grey", size = 0.3),
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
    'sse_model',
    'perc_sampling',
    'samples_per_state',
    'div_inc'
  )]

#make sure columns are the right type
df$order <- as.factor(df$order)
df$sse_model <- as.factor(df$sse_model)
df$year <- as.factor(df$year)
df$perc_sampling <- as.numeric(df$perc_sampling)
df$samples_per_state <- as.numeric(df$samples_per_state)

#make div_inc values > 1 (multistate models) = 1
df$div_inc[df$div_inc>1]<-1
table(df$div_inc)

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

#add tip bias column by dividing larger number of tips with state A by smaller number of tips with state B
#multi-state models are therefore largest tip bias possible in the data
df3$tip_bias <- top_df$samples_per_state / bot_df$samples_per_state

#drop NA
df3<-na.omit(df3)

#drop inf
df3 <- df3[!is.infinite(df3$tip_bias),]

#lm of trait-dependent results
summary(lm(log(tip_bias) ~ asin(sqrt(perc_sampling)), data = df3[df3$div_inc ==
                                                                                1, ]))

#lm of non-trait-dependent results
summary(lm(log(tip_bias) ~ asin(sqrt(perc_sampling)), data = df3[df3$div_inc ==
                                                                                0, ]))
#theme
theme_set(my_theme)

#color
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5, "Set2")[3]))
options(ggplot2.discrete.colour = c("#999999", brewer.pal(5, "Set2")[3]))

#tip number / sampling fraction = total number of taxa in clade of interest
#sampling fraction vs tip number
ggplot(df3, aes(
  x = tip_bias,
  y = asin(sqrt(perc_sampling)),
  color = as.factor(div_inc),
  shape = as.factor(div_inc)
)) +
  geom_point(alpha = 0.8, size = 2.5) +
  geom_smooth(method = lm, linetype = "dashed", aes(fill = as.factor(div_inc))) +
  scale_x_continuous(name = "Tip bias", trans =
                       'log10') +
  scale_y_continuous(name = "Sampling fraction (arcsine)") +
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
    panel.grid.major = element_line(),
  ) + guides(color = guide_legend(override.aes = list(
    fill = NA, linetype = c(0, 0), label = ""
  )))

ggsave(
  "figures/scatterplot_sampling_bias.png",
  width = 20,
  height = 20,
  units = 'cm'
)
