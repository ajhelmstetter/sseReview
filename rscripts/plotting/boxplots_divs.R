###
# boxplots of diversification differences
###

#Library
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(patchwork)
library(grid)
library(viridis)

#read in full data frame
df <-
  read.csv("data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'order',
    'trait_level_6',
    'character_state',
    'age_inferred',
    'sse_model',
    'div_rate'
  )]

#make sure columns are the right type
df$order <- as.factor(df$order)
df$trait_level_6 <- as.factor(df$trait_level_6)
df$character_state <- as.factor(df$character_state)
df$sse_model <- as.factor(df$sse_model)

#handpick some comparisons
hand_states <- c(
  "Annual",
  "Perennial",
#  "Dioecy",
#  "Non-dioecy",
  "C3",
  "C4",
  "CAM",
  "Epiphyte",
  "Non-epiphyte",
  "Terrestrial",
  "Tropical",
  "Temperate",
  "Non-tropical",
  "Woody",
  "Herbaceous",
  "Self-compatibility",
  "Self-incompatibility",
  "Diploid",
  "Polyploid"
)

#drop those without age inferred
df<-df[df$age_inferred==1,]

#subset datset to only high-frequency states
hf_df <- df[df$character_state %in% hand_states,]

#reset levels
hf_df$character_state <- droplevels(hf_df$character_state)

#make new df
rats<-hf_df[,c(1,4,5,8)]

#clean and examine rates df
colnames(rats) <-
  c("study", "trait_level_6", "character_state", "rate")
head(rats)

#change temperate to non-tropical
rats$character_state[rats$character_state == "Temperate"] <- "Non-tropical"

#change terrestrial to non-epiphyte
rats$character_state[rats$character_state == "Terrestrial"] <-"Non-epiphyte"

#modify one trait level for grouping
rats$trait_level_6[rats$trait_level_6 == "GeographicRange"] <- "Biome"

#convert trait_level_6 to character
rats$trait_level_6 <- as.character(rats$trait_level_6)

#add epiphytism as category
rats$trait_level_6[rats$character_state == "Epiphyte"] <- "Epiphytism"
rats$trait_level_6[rats$character_state == "Non-epiphyte"] <- "Epiphytism"

#add woodiness as category to separate from epiphytism
rats$trait_level_6[rats$character_state == "Woody"] <- "Woodiness"
rats$trait_level_6[rats$character_state == "Herbaceous"] <- "Woodiness"

#convert trait_level_6 back to factor
rats$trait_level_6 <- as.factor(rats$trait_level_6)

#make rate diffs numeric
rats$rate <- as.numeric(rats$rate)

#boxplots
#"free_x" will make y axis the same
ggplot(rats, aes(
  x = factor(character_state),
  y = rate,
  fill = character_state
)) + geom_boxplot(outlier.shape = NA, width = 0.4) + facet_wrap( ~ trait_level_6, scales = "free") +
  geom_jitter(
    aes(color = character_state),
    size = 1,
    alpha = 0.5,
    width = 0.05,
    height = 0
  ) +
  theme(legend.position = "none",
        axis.title.x = element_text(vjust = 0.3)) +
  scale_y_continuous(name = "Net diversification rate") +
  scale_x_discrete(name = "Character state") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.3) +
  scale_color_viridis(discrete = TRUE, alpha = 0.7)

ggsave(
  "figures/boxplots_divs.png",
  width = 20,
  height = 25,
  units = 'cm'
)
