rm(list = ls())

library(xgboost)
library(tidyr)
library(dplyr)
library(ggplot2)

load("outputs/xgboost.Rdata")

#combine importance tables
for (i in 1:length(imps)) {
  if (i == 1) {
    imps_df <- cbind(imps[[i]])
  } else {
    imps_df <- rbind(imps_df, imps[[i]])
  }
}

#aggregate based on feature
imps_agg <-
  imps_df %>% group_by(Feature) %>% summarise_each(funs(mean, sd))

#look at aggregated data
head(imps_agg)

#check calc is good
mean(imps_df$Gain[imps_df$Feature=='age'])
sd(imps_df$Gain[imps_df$Feature=='age'])

#order imps_agg by gain_mean
imps_agg <- imps_agg[order(imps_agg$Gain_mean, decreasing = T),]

#get colours based on variable type
imp_cols <- rep(1, length(imps_agg$Feature))
imp_cols[grepl("level", imps_agg$Feature)] <- "Taxonomy"
imp_cols[grepl("order", imps_agg$Feature)] <- "Taxonomy"
imp_cols[grepl("trait_", imps_agg$Feature)] <- "Trait"
imp_cols[grepl("sse_", imps_agg$Feature)] <- "SSE model"
imp_cols[grepl("year", imps_agg$Feature)] <- "Year"

imp_cols[grepl("1", imp_cols)] <- "Input data"

#limits
lim <- c(0, max(imps_agg$Gain_mean) + (max(imps_agg$Gain_mean) / 5))

#theme
theme_set(theme_bw())
library(feathers)
options(ggplot2.discrete.colour = get_pal(names(feathers_palettes)[4])[c(1:3, 5, 6)])

#TOP 20
#lollipop plot
p <-
  ggplot(imps_agg[1:20, ], aes(x = reorder(Feature, Gain_mean), y = Gain_mean)) +
  geom_segment(
    aes(
      x = reorder(Feature, Gain_mean),
      xend = reorder(Feature, Gain_mean),
      y = 0,
      yend = Gain_mean,
      color = as.factor(imp_cols[1:20])
    ),
    size = 0.6,
    alpha = 0.5
  ) +
  geom_point(aes(color = as.factor(imp_cols[1:20])),
             shape = 16,
             size = 1.5,
             alpha = 1) +
  geom_errorbar(
    aes(
      ymin = Gain_mean - Gain_sd,
      ymax = Gain_mean + Gain_sd,
      color = as.factor(imp_cols[1:20])
    ),
    alpha = 1,
    width = 0.4,
    size = 0.8
  ) +
#  scale_shape_manual(values = c(15, 16, 17, 18, 19)) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), limits = lim) +
  theme(
    axis.text.x = element_text(angle = 0),
    axis.text.y = element_text(size=12),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(vjust = 0.2,size=14),
    axis.title.y = element_text(size=14),
    legend.title = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor.x = element_line(colour = "grey"),
    panel.grid.major.x = element_line(colour = "grey"),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.position = c(0.85, 0.1),
    legend.background = element_rect(fill = "white", colour = "grey")
  ) +
  xlab("Variable") +
  ylab("Gain mean")

p

#save
ggsave(
  "figures/lollipop_xgboost.png",
  width = 25,
  height = 15,
  units = 'cm'
)
