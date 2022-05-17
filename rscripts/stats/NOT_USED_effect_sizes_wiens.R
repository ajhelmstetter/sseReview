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
  "Dioecy",
  "Non-dioecy",
  "C3",
  "C4",
  "CAM",
  "Epiphyte",
  "Non-epiphyte",
  "Terrestrial",
  "Tropics",
  "Temperate",
  "Non-tropical",
  "Woody",
  "Herbaceous",
  "Self-compatibility",
  "Self-incompatibility",
  "Diploid",
  "Polyploid"
)


#subset datset to only high-frequency states
hf_df <- df[df$character_state %in% hand_states,]

#drop NAs

hf_df <- na.omit(hf_df)

#reset levels
hf_df$character_state <- droplevels(hf_df$character_state)

#save CSV
write.csv(hf_df, "outputs/effect_sizes.csv")

#empty matrix to store data
rats <- matrix(nrow = 0, ncol = 4)

#loop through studies
for (i in 1:length(unique(hf_df$study))) {
  #make subset dataframe for study
  temp_df <- hf_df[hf_df$study == unique(hf_df$study)[i], ]

  #loop through models in study
  for (j in 1:length(unique(temp_df$model_no))) {
    #make subset dataframe for model
    temp_mod_df <-
      temp_df[temp_df$model_no == unique(temp_df$model_no)[j], ]

    #IF BOTH +VE
    if (temp_mod_df$div_rate[which.max(temp_mod_df$div_rate)] &&
        temp_mod_df$div_rate[which.min(temp_mod_df$div_rate)] > 0) {
      cat(temp_mod_df$study[1], "is both +VE")
      cat("")
      #maximum div rate
      rat_val <-
        temp_mod_df$div_rate[which.max(temp_mod_df$div_rate)] /

        #minimum div rate
        temp_mod_df$div_rate[which.min(temp_mod_df$div_rate)]

      #make data frame
      stu_sta_rat <-
        data.frame(
          temp_mod_df$study[which.max(temp_mod_df$div_rate)],
          temp_mod_df$trait_level_6[which.max(temp_mod_df$div_rate)],
          as.character(temp_mod_df$character_state[which.max(temp_mod_df$div_rate)]),
          as.numeric(log2(rat_val))
        )

    } else {
      #IF ONE -VE
      #abs(x-y)/max(x,y)

      cat(temp_mod_df$study[1], "is at least one -VE")
      cat("")

      #abs(x-y)
      rat_val <-
        abs(temp_mod_df$div_rate[which.max(temp_mod_df$div_rate)] - temp_mod_df$div_rate[which.min(temp_mod_df$div_rate)]) /

        #max(abs(x),abs(y))
        max(abs(temp_mod_df$div_rate[which.max(temp_mod_df$div_rate)]),
            abs(temp_mod_df$div_rate[which.min(temp_mod_df$div_rate)]))

      #make data frame
      stu_sta_rat <-
        data.frame(
          temp_mod_df$study[which.max(temp_mod_df$div_rate)],
          temp_mod_df$trait_level_6[which.max(temp_mod_df$div_rate)],
          as.character(temp_mod_df$character_state[which.max(temp_mod_df$div_rate)]),
          as.numeric(log2(rat_val))
        )

    }

    #bind data frame to template
    rats <- rbind(rats, stu_sta_rat)

  }

}

colnames(rats) <-
  c("study", "trait_level_6", "character_state", "effect_size")


#change temperate to non-tropical
rats$character_state[rats$character_state == "Temperate"] <-
  "Non-tropical"


#change terrestrial to non-epiphyte
rats$character_state[rats$character_state == "Terrestrial"] <-
  "Non-epiphyte"

#modify one trait level for grouping
rats$trait_level_6[rats$trait_level_6 == "GeographicRange"] <-
  "Biome"

#convert trait_level_6 to character
rats$trait_level_6 <- as.character(rats$trait_level_6)

#add woodiness as category to separate from epiphytism
rats$trait_level_6[rats$character_state == "Woody"] <- "Woodiness"
rats$trait_level_6[rats$character_state == "Herbaceous"] <-
  "Woodiness"

#add epiphytism as category
#add woodiness as category to separate from epiphytism
rats$trait_level_6[rats$character_state == "Epiphyte"] <-
  "Epiphytism"
rats$trait_level_6[rats$character_state == "Non-epiphyte"] <-
  "Epiphytism"

#convert trait_level_6 back to factor
rats$trait_level_6 <- as.factor(rats$trait_level_6)

#remove infinite values
#rats <- rats[rats$effect_size < 10,]

#add missing combinations
rats <- rbind(rats, c("",
                      "MatingSystem",
                      "Self-compatibility",
                      NA))

rats <- rbind(rats, c("",
                      "Epiphytism",
                      "Non-epiphyte",
                      NA))

#make effect sizes numeric
rats$effect_size <- as.numeric(rats$effect_size)

view(rats)

ggplot(rats, aes(
  x = factor(character_state),
  y = effect_size,
  fill = character_state
)) + geom_boxplot(outlier.shape = NA, width = 0.4) + facet_wrap( ~ trait_level_6, scales = "free_x") +
  geom_jitter(
    aes(color = character_state),
    size = 1,
    alpha = 0.5,
    width = 0.05,
    height = 0
  ) +
  theme(legend.position = "none",
        axis.title.x = element_text(vjust = 0.3)) +
  scale_y_continuous(limits = c(-1, 6),
                     name = "Effect size") +
  scale_x_discrete(name = "Character state") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.3) +
  scale_color_viridis(discrete = TRUE, alpha = 0.7)

ggsave(
  "figures/boxplots_effect_sizes.png",
  width = 20,
  height = 25,
  units = 'cm'
)
