###
# Model use
###

# Library
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(ggplot2)

options(ggplot2.discrete.fill = brewer.pal(11, "RdYlBu"))

#read in full data frame
df <-
  read.csv("~/Dropbox/projects/AJH_DiveRS/sseReview/data/sse_review_table - main_table.csv")

#unique combinations of study/order
df_mod <- unique(df[c("study", "sse_model", "year")])

#get counts of model use per year
ym <- df_mod %>%
  dplyr::count(year, sse_model, .drop = F)
ym

#fill in non-existing combinations with 0
years <- unique(ym$year)
sse_models <- unique(ym$sse_model)
combinations <- expand.grid(year = years, sse_model = sse_models)
ym2 <-
  full_join(ym, combinations, by = c("year" = "year", "sse_model" = "sse_model")) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  arrange(year, sse_model)

head(ym2)

###
# stacked barplot
###

#colors
library(RColorBrewer)
colourCount <- length(unique(ym2$sse_model))
getPalette <- colorRampPalette(brewer.pal(12, "Paired"))
pal <- getPalette(colourCount)

#count by studies per year
ys <- df_mod %>%
  dplyr::count(study, year, .drop = F)
head(ys)

#make a df of the number of studies per year
df_ys <- data.frame(table(ys$year))
head(df_ys)

#format year
df_ys$Var1 <- as.numeric(as.character(df_ys$Var1))
str(df_ys)

#remove line from 2021
df_ys<-df_ys[c(1:(length(df_ys$Freq)-1)),]

ggplot(ym, aes(y = n, x = year)) +
  geom_bar(aes(fill = sse_model), position = "stack", stat = "identity",alpha=0.9) +
  scale_y_continuous(breaks = seq(0, 30, by = 5), expand = expansion(mult = c(0, .1))) +
  scale_x_continuous(breaks = seq(2009, 2021, by = 1)) +
  xlab("Publication year") +
  ylab("No. of studies using model") +
  scale_fill_manual(values = pal) +
  theme(
    # remove the vertical grid lines
    text = element_text(size = 11),
    legend.title = element_blank(),
    legend.position="right",
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line(size = .2, color = "black"),
    panel.grid.minor.y = element_line(size = .1, color = "grey"),
    panel.background = element_rect(
      fill = "white",
      colour = "black",
      size = 1,
      linetype = "solid"
    )
  ) +
  geom_line(
    data = df_ys,
    aes(x = Var1, y = Freq, group = 1),
    size = 1,
    color = "black",
    alpha = 0.75
  ) +
  geom_point(
    data = df_ys,
    aes(x = Var1, y = Freq, group = 1),
    size = 2,
    color = "black",
    alpha = 0.75
  )

# variable widths not possible in ggplot2 without geom_rect() craziness
ggsave(
  "figures/stacked_barplot_models.png",
  width = 24,
  height = 16,
  units = 'cm'
)

