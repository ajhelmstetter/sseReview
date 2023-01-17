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
  axis.title.y = element_text(angle=90, size = 14, hjust = -1.5 , margin = margin(
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
#dont need for ratio
#df<-df[df$age_inferred==1,]

#only those rows with div rate values (not NA)
df<-df[!is.na(df$div_rate),]

#remove combination from df
df<-df[df$trait_level_2!="Combination",]

#make new df with MuSSE and MuHiSSE only
df_muss<-df[df$sse_model=="MuSSE" | df$sse_model=="MuHiSSE",]
df_muss

#get max div rate in multistate models
max_muss<-df_muss %>% group_by(study,model_no) %>% slice_max(n = 1,
                                                 order_by = div_rate,
                                                 with_ties = F)

#get min div rate in multistate models
min_muss<-df_muss %>% group_by(study,model_no) %>% slice_min(n = 1,
                                                             order_by = div_rate,
                                                             with_ties = F)

#combine min and max rates so only extremes per multistate model
df_muss_minmax<-rbind(data.frame(max_muss),data.frame(min_muss))

#remove MuSSE and MuHiSSE studies from df
df<-df[df$sse_model!="MuSSE",]
df<-df[df$sse_model!="MuHiSSE",]

#add reduced MuSSE rows with min and max values only
df<-rbind(df,df_muss_minmax)

#empty matrix to store data
rats <- matrix(nrow = 0, ncol = 4)

#loop through studies
for (i in 1:length(unique(df$study))) {
  #make subset dataframe for study
  temp_df <- df[df$study == unique(df$study)[i],]

  #loop through models in study
  for (j in 1:length(unique(temp_df$model_no))) {
    #make subset dataframe for model
    temp_mod_df <-
      temp_df[temp_df$model_no == unique(temp_df$model_no)[j],]

    #IF BOTH +VE
    if (temp_mod_df$div_rate[which.max(temp_mod_df$div_rate)] > 0 &&
        temp_mod_df$div_rate[which.min(temp_mod_df$div_rate)] > 0) {
      cat(temp_mod_df$study[1], "is both +VE")
      writeLines("")

      #abs(x-y)
      rat_val <-
        (temp_mod_df$div_rate[which.max(temp_mod_df$div_rate)] - temp_mod_df$div_rate[which.min(temp_mod_df$div_rate)]) /

        #max(abs(x),abs(y))
        temp_mod_df$div_rate[which.max(temp_mod_df$div_rate)]

      #make data frame
      stu_sta_rat <-
        data.frame(
          temp_mod_df$study[which.max(temp_mod_df$div_rate)],
          temp_mod_df$trait_level_1[which.max(temp_mod_df$div_rate)],
          temp_mod_df$trait_level_2[which.max(temp_mod_df$div_rate)],
          as.numeric(rat_val)
        )

    } else if (temp_mod_df$div_rate[which.max(temp_mod_df$div_rate)] < 0 &&
          temp_mod_df$div_rate[which.min(temp_mod_df$div_rate)] < 0) {

        #IF BOTH -VE
        cat(temp_mod_df$study[1], "is both -VE")
        writeLines("")

        #remove
        rat_val <-  999

        #make data frame
        stu_sta_rat <-
          data.frame(
            temp_mod_df$study[which.max(temp_mod_df$div_rate)],
            temp_mod_df$trait_level_1[which.max(temp_mod_df$div_rate)],
            temp_mod_df$trait_level_2[which.max(temp_mod_df$div_rate)],
            as.numeric(rat_val)
          )

      } else {

        #IF ONE -VE
        #abs(x-y)/max(x,y)
        cat(temp_mod_df$study[1], "is at least one -VE")
        writeLines("")

        #remove
        rat_val <-  999

        #make data frame
        stu_sta_rat <-
          data.frame(
            temp_mod_df$study[which.max(temp_mod_df$div_rate)],
            temp_mod_df$trait_level_1[which.max(temp_mod_df$div_rate)],
            temp_mod_df$trait_level_2[which.max(temp_mod_df$div_rate)],
            as.numeric(rat_val)
          )

      }

    #bind data frame to template
    rats <- rbind(rats, stu_sta_rat)

    }

}

#replace placeholder values with NA
rats$as.numeric.rat_val.[grepl(999,rats$as.numeric.rat_val.)]<-NA

#rename for ease
df2<-rats
colnames(df2)<-c("study","trait_level_1","trait_level_2","div_ratio")
#view(df2)

#change to factor for sorting
df2$trait_level_1<-as.factor(df2$trait_level_1)
df2$trait_level_2<-as.factor(df2$trait_level_2)

#number of models
df_no_na<-na.omit(df2)
length(df_no_na$div_ratio)
length(unique(df_no_na$study))


#####
# Stats trait level 1
#####

# Compute the analysis of variance
res.aov <- aov(div_ratio ~ trait_level_1, data = na.omit(df2))

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
kruskal.test(div_ratio ~ trait_level_1, data = df2)

#pairwise non-parametric
pairwise.wilcox.test(df2$div_ratio, df2$trait_level_1,
                     p.adjust.method = "BH")

#####
# Boxplots trait level 1
#####

#order factors for plot
library(forcats)
df3 <-df2 %>% mutate(trait_level_1 = fct_relevel(trait_level_1,
                                                 "Intrinsic",
                                                 "Extrinsic",
                                                 "Interactions"))

p1 <- ggplot(df3, aes(
  x = trait_level_1,
  y = div_ratio,
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
  scale_y_continuous(name="") +
  scale_x_discrete(name = "Trait level 1") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.3) +
  scale_color_viridis(discrete = TRUE, alpha = 0.8)  +
  annotate(geom="text", x=0.075, y=1, label="(a)",size=5) +
  coord_cartesian(ylim = c(0.001,1),
                  xlim = c(1, 3),
                  clip = 'off')

#####
# Stats trait level 2
#####

# Compute the analysis of variance
res.aov <- aov(div_ratio ~ trait_level_2, data = na.omit(df2))

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
kruskal.test(div_ratio ~ trait_level_2, data = df2)

#pairwise non-parametric
pairwise.wilcox.test(df2$div_ratio, df2$trait_level_2,
                     p.adjust.method = "BH")

#####
# Boxplots trait level 2
#####

df4 <-df2 %>% mutate(trait_level_2 = fct_relevel(trait_level_2,
                                        "Biogeography",
                                        "Reproduction",
                                        "Vegetative",
                                        "Pollination",
                                        "Symbiosis",
                                        "Dispersal",
                                        "Habitat",
                                        "Genome"
))

p2 <- ggplot(df4, aes(
  x = factor(trait_level_2),
  y = div_ratio,
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
        axis.title.x = element_text(vjust = 0.2),
        axis.text.x = element_text(angle=90)) +
        scale_y_continuous() +
  scale_x_discrete(name = "Trait level 2") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.3) +
  scale_color_viridis(discrete = TRUE, alpha = 0.8) +
  annotate(geom="text", x=-0.5, y=1, label="(b)",size=5) +
  coord_cartesian(ylim = c(0.001,1),
                  xlim = c(1, 9),
                  clip = 'off') +
  ylab(bquote(Relative~difference~of~r[max]~and~r[min]))

###
#arrange plots together
###

p1 / p2

ggsave("figures/violins_reldiffs_both_pos.png",
       width = 30,
       height = 20,
       units = 'cm')

#how many models used?
table(is.na(rats$as.numeric.rat_val.))[1]
table(is.na(df3$div_ratio))[1]
table(is.na(df4$div_ratio))[1]

#how many studies?
str(na.omit(df3))
df5<-na.omit(df3)
length(unique(df5$study))
