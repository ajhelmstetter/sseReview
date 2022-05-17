rm(list=ls())

#Library
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(patchwork)
library(grid)

#read in full data frame
df <-
  read.csv("data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'clade',
    'character_state',
    'trait_level_6',
    'div_inc'
  )]

#make sure columns are the right type
df$character_state <- as.factor(df$character_state)

#examine how many times states were used
sort(table(df$character_state)[table(df$character_state) > 3])

#get states that are tested 4 or more times
hf_states <-
  names(table(df$character_state)[table(df$character_state) > 3])
head(hf_states)

#handpick some comparisons
hand_states <- c(
  "Annual",
  "Perennial",
  "Dioecy",
  "Non-dioecy",
  "C3",
  "C4",
  "CAM",
  "Epiphyte",
  "Hemiepiphytic",
  "Non-epiphyte",
  "Large tree", #to pair with hemiepiphytic
  "Terrestrial",
  "Tropical",
  "Non-tropical",
  "Temperate",
  "Woody",
  "Herbaceous",
  "Self-compatibility",
  "Self-incompatibility",
  "Diploid",
  "Polyploid"
)

#subset dataset to only selected states
hf_df <- df[df$character_state %in% hand_states, ]

#make character state character so can be manipulated
hf_df$character_state <- as.character(hf_df$character_state)

#split lifeform into epiphytism and woodiness
hf_df$trait_level_6[grepl("Herbaceous",hf_df$character_state)]<-"Woodiness"
hf_df$trait_level_6[grepl("Woody",hf_df$character_state)]<-"Woodiness"

#replace terrestrial/large tree with non-epiphyte
hf_df$character_state[grep("Hemiepiphytic",hf_df$character_state)]<-"Epiphyte"

#replace terrestrial/large tree with non-epiphyte
hf_df$character_state[grep("Terrestrial",hf_df$character_state)]<-"Non-epiphyte"
hf_df$character_state[grep("Large tree",hf_df$character_state)]<-"Non-epiphyte"

#geographic range to biome for grouping
hf_df$trait_level_6[grepl("Non-tropical",hf_df$character_state)]<-"Biome"

#replace temperate with Non-tropical
hf_df$character_state[grep("Non-tropical",hf_df$character_state)]<-"Temperate"

#reduce to a single binary result per model per study
hf_df<-hf_df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

hf_df$character_state[grepl(0,hf_df$div_inc)]<-".No effect"

#unique combinations of clade and trait
table(unique(data.frame(hf_df$clade,hf_df$trait_level_6))$hf_df.trait_level_6)

#unique combinations of study and trait
table(unique(data.frame(hf_df$study,hf_df$trait_level_6))$hf_df.trait_level_6)

#number of models per trait
table(hf_df$trait_level_6)


#not run
#remove NA
#agg_st_mod <- na.omit(agg_st_mod)

#not run
#make div_inc values > 1 (multistate models) = 1
#hf_df$div_inc[hf_df$div_inc>1]<-1

#count numbers of 0 and 1 per state
df2 <- hf_df %>% group_by(character_state, trait_level_6) %>% summarize(count = n())
head(df2)
#view(df2)

df2<-data.frame(df2)

#add missing combinations
df2<-rbind(df2,c("Non-epiphyte","LifeForm",0))
df2<-rbind(df2,c("Self-compatibility","MatingSystem",0))
df2<-rbind(df2,c(".No effect","MatingSystem",0))
df2<-rbind(df2,c("C3","Photosynthesis",0))

#make binary classification character
df2$count <- as.numeric(df2$count)

theme_update(
  axis.text.x = element_text(
    angle = 90,
    vjust = -0.05,
    hjust = 1
  ),
  axis.ticks.x = element_blank(),
  axis.title.x = element_text(vjust = 0.2),
  axis.ticks.y = element_blank(),
  legend.title = element_blank(),
  panel.border = element_blank(),
  panel.grid.minor = element_line(colour = "grey"),
  panel.grid.major.y = element_line(colour = "grey"),
  panel.grid.major.x = element_blank(),
  panel.background = element_rect(fill = "white"),
  legend.position = c(0.85, 0.10),
  legend.background = element_blank()
)

#color
options(ggplot2.discrete.fill = c("#999999", brewer.pal(5, "Set2")[1]))

#faceted stacked barplot
ggplot(df2, aes(
  x = factor(character_state),
  y = count
)) +
  geom_bar(stat = "identity", width = 0.75) + facet_wrap( ~ trait_level_6, scales =
                                                            "free_x") +
  scale_x_discrete(name = "Trait category") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 50),
                     name = "Number of times tested") +
  scale_fill_discrete(labels = c("No effect", "Effect"))

ggsave(
  "figures/stacked_barplots_states.png",
  width = 20,
  height = 25,
  units = 'cm'
)
