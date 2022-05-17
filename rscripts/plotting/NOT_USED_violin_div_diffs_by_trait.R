###
# Violin plot of div rates differences for trait categories
###

# Library
library(viridis)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(patchwork)
library(dplyr)
library(RColorBrewer)

# theme
my_theme = theme(
  text = element_text(size = 12),
  axis.text = element_text(size = 12),
  axis.title.x = element_blank(),
  axis.text.x=element_text(angle=90),
  #axis.ticks.x=element_blank(),
  axis.line.x = element_line(),
  axis.title.y = element_text(angle=90, size = 14, hjust = -5 , margin = margin(
    t = 0,
    r = 10,
    b = 0,
    l = 20
  )),
  axis.text.y = element_text(),
  axis.ticks.y = element_line(),
  axis.line.y = element_line(),
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(size = 12),
  legend.position = c(0.8, 0.9),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank()
)

theme_set(my_theme)

#read in full data frame
df <-
  read.csv("data/sse_review_table - main_table.csv")

#choose the columns you want in model (plus study/model_no for formatting)
df <-
  df[, c(
    'study',
    'model_no',
    'sse_model',
    'trait_level_1',
    'trait_level_2',
#    'trait_level_3',
#    'trait_level_4',
    'div_inc',
    'div_rate',
    'age_inferred'
  )]

#only those where age was inferred
df<-df[df$age_inferred==1,]

#only those rows with div rate values (not NA)
df<-df[!is.na(df$div_rate),]

#make new df with MuSSE only (MuHiSSE only has two states with div rates)
df_muss<-df[df$sse_model=="MuSSE",]

#get max div rate in MuSSE model
max_muss<-df_muss %>% group_by(study,model_no) %>% slice_max(n = 1,
                                                 order_by = div_rate,
                                                 with_ties = F)

#get min div rate in MuSSE model
min_muss<-df_muss %>% group_by(study,model_no) %>% slice_min(n = 1,
                                                             order_by = div_rate,
                                                             with_ties = F)

#combine min and max rates so only extremes per MuSSE model
df_muss_minmax<-rbind(data.frame(max_muss),data.frame(min_muss))

#remove MuSSE studies from df
df<-df[df$sse_model!="MuSSE",]

#add reduced MuSSE rows with min and max values only
df<-rbind(df,df_muss_minmax)

#calculate div rate differences
df2 <- df %>%
  group_by(study, model_no )%>%
  mutate(
    div_diff = div_rate - lag(div_rate)
  )

#absolute difference
df2$div_diff<-abs(df2$div_diff)

#remove rows with NA introduced by mutate
df2<-na.omit(df2)

#change to factor for sorting
df2$trait_level_1<-as.factor(df2$trait_level_1)
df2$trait_level_2<-as.factor(df2$trait_level_2)

#####
# Stats trait level 1
#####

# Compute the analysis of variance
res.aov <- aov(div_diff ~ trait_level_1, data = df2)

# Summary of the analysis
summary(res.aov)

# Homogeneity of variances
plot(res.aov, 1)

# Normality
plot(res.aov, 2)

# Extract the residuals
aov_residuals <- residuals(object = res.aov)

# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

#
kruskal.test(div_diff ~ trait_level_1, data = df2)

#pairwise non-parametric
pairwise.wilcox.test(df2$div_diff, df2$trait_level_1,
                     p.adjust.method = "BH")

#####
# Boxplots trait level 1
#####

#order factors for plot
library(forcats)
df3 <-df2 %>% mutate(trait_level_1 = fct_relevel(trait_level_1,
                                                 "Extrinsic",
                                                 "Intrinsic",
                                                 "Interactions"))

p1 <- ggplot(df3, aes(
  x = trait_level_1,
  y = div_diff,
  fill = trait_level_1
)) + geom_boxplot(outlier.shape = NA, width = 0.2) +
  geom_violin(width=0.75) +
  geom_jitter(
    aes(color = trait_level_1),
    size = 1,
    alpha = 0.5,
    width = 0.05,
    height = 0
  ) +
  theme(legend.position = "none",
        axis.title.x = element_text(vjust = 0.3)) +
  scale_y_continuous(name="",trans="log10") +
  scale_x_discrete(name = "Trait level 1") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.3) +
  scale_color_viridis(discrete = TRUE, alpha = 0.8)  +
  annotate(geom="text", x=0.06, y=9, label="(a)",size=5) +
  coord_cartesian(ylim = c(0.0002, 5),
                  xlim = c(1, 3),
                  clip = 'off')

#####
# Stats trait level 2
#####

# Compute the analysis of variance
res.aov <- aov(div_diff ~ trait_level_2, data = df2)

# Summary of the analysis
summary(res.aov)

# Homogeneity of variances
plot(res.aov, 1)

# Normality
plot(res.aov, 2)

# Extract the residuals
aov_residuals <- residuals(object = res.aov)

# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

#
kruskal.test(div_diff ~ trait_level_2, data = df2)

#pairwise non-parametric
pairwise.wilcox.test(df2$div_diff, df2$trait_level_2,
                     p.adjust.method = "BH")

#####
# Boxplots trait level 2
#####

df4 <-df2 %>% mutate(trait_level_2 = fct_relevel(trait_level_2,
                                        "Dispersal",
                                        "Biogeography",
                                        "Symbiosis",
                                        "Habitat",
                                        "Vegetative",
                                        "Reproduction",
                                        "Combination",
                                        "Genome",
                                        "Pollination"))

p2 <- ggplot(df4, aes(
  x = factor(trait_level_2),
  y = div_diff,
  fill = trait_level_2
)) + geom_boxplot(outlier.shape = NA, width = 0.2) +
  geom_violin(width=1.4) +
  geom_jitter(
    aes(color = trait_level_2),
    size = 1,
    alpha = 0.5,
    width = 0.05,
    height = 0
  ) +
  theme(legend.position = "none",
        axis.title.x = element_text(vjust = 0.3),
        axis.text.x = element_text(angle=90)) +
        scale_y_continuous(name = "Difference in net diversification rate (species / Ma)", trans="log10") +
  scale_x_discrete(name = "Trait level 2") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.3) +
  scale_color_viridis(discrete = TRUE, alpha = 0.8) +
  annotate(geom="text", x=-0.6, y=9, label="(b)",size=5) +
  coord_cartesian(ylim = c(0.0002, 5),
                  xlim = c(1, 9),
                  clip = 'off')

###
#arrange plots together
###

p1 / p2

ggsave("figures/violins_all.png",
       width = 30,
       height = 30,
       units = 'cm')
