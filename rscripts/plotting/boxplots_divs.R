###
# boxplots of diversification differences
###

rm(list=ls())

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
#  "Dioecy", #not time calibrated
#  "Non-dioecy",
  "C3",
  "C4",
  "CAM",
  "CCM",
  "Non-CCM",
  "C4_CAM",
  "Epiphyte",
  "Hemiepiphytic",
  "Non-epiphyte",
  "Large tree", #to pair with hemiepiphytic in one case
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

#make character state character so can be manipulated
hf_df$character_state <- as.character(hf_df$character_state)
hf_df$trait_level_6 <- as.character(hf_df$trait_level_6)

#make new df
rats<-hf_df[,c(1,2,4,5,8)]

#clean and examine rates df
colnames(rats) <-
  c("study", "model_no","trait_level_6", "character_state", "rate")
head(rats)

#convert trait_level_6 to character
rats$trait_level_6 <- as.character(rats$trait_level_6)

#split lifeform into epiphytism and woodiness
rats$trait_level_6[grep("Herbaceous",rats$character_state)]<-"Woodiness"
rats$trait_level_6[grep("Woody",rats$character_state)]<-"Woodiness"

#change Hemiepiphytic to Epiphyte
rats$character_state[rats$character_state == "Hemiepiphytic"] <-"Epiphyte"

#replace terrestrial/large tree with non-epiphyte
rats$character_state[grep("Terrestrial",rats$character_state)]<-"Non-epiphyte"
rats$character_state[grep("Large tree",rats$character_state)]<-"Non-epiphyte"

#add epiphytism as category
rats$trait_level_6[rats$character_state == "Epiphyte"] <- "Epiphytism"
rats$trait_level_6[rats$character_state == "Non-epiphyte"] <- "Epiphytism"

#change temperate to non-tropical
rats$character_state[rats$character_state == "Temperate"] <- "Non-tropical"

#group C4/CAM photosynthesis
rats$character_state[grep("Non-CCM",rats$character_state)]<-"C3"
rats$character_state[grep("CCM",rats$character_state)]<-"C4_CAM"
rats$character_state[grep("C4",rats$character_state)]<-"C4_CAM"
rats$character_state[grep("CAM",rats$character_state)]<-"C4_CAM"

#modify trait level for grouping
rats$trait_level_6[rats$trait_level_6 == "MatingSystem"] <- "Self-compatibility"

#remove unwanted MuSSE
rats<-rats[rats$trait_level_6 != "Combination",]

#remove geographic range from Sun et al. paper
rats<-rats[rats$trait_level_6 != "GeographicRange",]

#convert trait_level_6 back to factor
rats$trait_level_6 <- as.factor(rats$trait_level_6)

#make rates numeric
rats$rate <- as.numeric(rats$rate)

###
# Calculate significance with paired Wilcoxon
###

#sort by model number for pairwise comparisons
rats<-rats[order(rats$study),]

#Biome p=0.7314
wilcox.test(rate ~ character_state, data = rats[rats$trait_level_6=="Biome",], paired = TRUE)

#should be same
one_trait<-rats[rats$trait_level_6=="Biome",]
wilcox.test(one_trait[one_trait$character_state=="Tropical",]$rate,
            one_trait[one_trait$character_state=="Non-tropical",]$rate,
            paired = T
)

#Epiphytism p=0.0625
#remove rogue MuSSE state
er<-rats[rats$trait_level_6=="Epiphytism",]
#er<-er[!grepl("Bruun",er$study),]
wilcox.test(rate ~ character_state, data = er, paired = TRUE)

#should be same
one_trait<-rats[rats$trait_level_6=="Epiphytism",]
wilcox.test(one_trait[one_trait$character_state=="Epiphyte",]$rate,
            one_trait[one_trait$character_state=="Non-epiphyte",]$rate,
            paired = T
)

#LifeSpan p=0.4017
wilcox.test(rate ~ character_state, data = rats[rats$trait_level_6=="LifeSpan",], paired = TRUE)

one_trait<-rats[rats$trait_level_6=="LifeSpan",]
wilcox.test(one_trait[one_trait$character_state=="Annual",]$rate,
            one_trait[one_trait$character_state=="Perennial",]$rate,
            paired = T
)

#Self-compatability p=0.125m
wilcox.test(rate ~ character_state, data = rats[rats$trait_level_6=="Self-compatibility",], paired = TRUE)

one_trait<-rats[rats$trait_level_6=="Self-compatibility",]
wilcox.test(one_trait[one_trait$character_state=="Self-compatibility",]$rate,
            one_trait[one_trait$character_state=="Self-incompatibility",]$rate,
            paired = T
)

#Photosynthesis p=0.01415
wilcox.test(rate ~ character_state, data = rats[rats$trait_level_6=="Photosynthesis",], paired = TRUE)

one_trait<-rats[rats$trait_level_6=="Photosynthesis",]
wilcox.test(one_trait[one_trait$character_state=="C3",]$rate,
            one_trait[one_trait$character_state=="C4_CAM",]$rate,
            paired = T
)


#Ploidy p=1
#Extra polyploid from MuSSE analysis so did not use paired
wilcox.test(rate ~ character_state, data = rats[rats$trait_level_6=="Ploidy",], paired=T)

one_trait<-rats[rats$trait_level_6=="Ploidy",]
wilcox.test(one_trait[one_trait$character_state=="Diploid",]$rate,
            one_trait[one_trait$character_state=="Polyploid",]$rate,
            paired = T
)


#Woodiness p=0.5
wilcox.test(rate ~ character_state, data = rats[rats$trait_level_6=="Woodiness",], paired = TRUE)

one_trait<-rats[rats$trait_level_6=="Woodiness",]
wilcox.test(one_trait[one_trait$character_state=="Herbaceous",]$rate,
            one_trait[one_trait$character_state=="Woody",]$rate,
            paired = T
)

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
