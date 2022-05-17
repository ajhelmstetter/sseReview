###
# Frequency studied vs species in family
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
  legend.position = c(0.8, 0.9),
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
    'family',
    'trait_level_1',
    'trait_level_2'
  )]

#make sure columns are the right type
df$family <- as.factor(df$family)

#unique combinations of study/family
df_uni<- unique(df[c("study", "family")])

#drop 'Multiple'
df_uni <- df_uni %>% filter(!grepl("Multiple", family))

#reduce to a single result per family per study
library(dplyr)
so <-df_uni %>%
  dplyr::count(family, study, .drop = T)
so$family

#get frequency table
fam_freq<-data.frame(table(so$family))
colnames(fam_freq)<-c("family","freq")
fam_freq

#drop 'Multiple'
fam_freq <- fam_freq %>% filter(!grepl("Multiple", family))

#read in data from Cristenhuiz paper
fam_no<-read.csv("data/species_per_family.csv")

#not studied
not_studied<-fam_no[fam_no$family%in%setdiff(fam_no$family,fam_freq$family),]
head(not_studied[order(not_studied$no_species,decreasing = T),])

#get species numbers of studied families
fam_no<-fam_no[fam_no$family%in%fam_freq$family,]
fam_no<-fam_no[order(fam_no$family),]
head(fam_no)

#check match
fam_freq$family==fam_no$family
setdiff(fam_freq$family,fam_no$family)

#add column with frequency to species numbers
fam_no$freq <- fam_freq$freq

theme_set(my_theme)

#scatterplot
ggplot(fam_no, aes(x = no_species, y = freq)) +
  geom_point(alpha = 0.5, size = 2.5) +
  scale_x_continuous(name = "Number of species in family", labels = scales::comma) +
  scale_y_continuous(name = "Number of papers on family") +
  geom_text_repel(aes(label=ifelse(freq>1,as.character(family),'')))

ggsave("figures/scatterplot_families_species_papers.png",
       width = 20,
       height = 12,
       units = 'cm'
)

