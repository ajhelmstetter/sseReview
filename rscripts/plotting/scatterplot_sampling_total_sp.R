
###
# scatterplot of sampling fraction vs total number of species in clade
###


#Library
library(tidyverse)
library(ggplot2)
library(treemapify)
library(RColorBrewer)
library(ggrepel)

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
    'div_inc',
    'perc_sampling',
    'tips'
  )]

#set div_inc to factor for plotting
df$div_inc<-as.factor(df$div_inc)

#remove rows with NA
df<-na.omit(df)

#tip number / sampling fraction = total number of taxa in clade of interest
#sampling fraction vs tip number
ggplot(df, aes(x = perc_sampling, y = tips/(perc_sampling/100), color = div_inc)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_smooth(method = lm, linetype = "dashed", aes(fill = div_inc)) +
  scale_x_continuous(name = "Sampling fraction (%)") +
  scale_y_continuous(name = "Total number of species in study group", trans='log10') +
  theme(
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.minor = element_line(),
    panel.grid.major = element_line()
  )

ggsave(
  "figures/scatterplot_sampling_total_species.png",
  width = 20,
  height = 12,
  units = 'cm'
)

### 3d plot
library(plotly)

par(mar=c(5,5,5,5))
fig<-plot_ly(x=log(df$perc_sampling), y=log(df$tips), z=log(df$tips)/log((df$perc_sampling/100)), type="scatter3d", mode="markers", color=df$div_inc)
fig <- fig %>% layout(
  scene = list(
    xaxis = list(title = "sampling %"),
    yaxis = list(title = "No. tips"),
    zaxis = list(title = "Total species")
  ))

fig
