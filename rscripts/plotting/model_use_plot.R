###
# Model use
###

# Library
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(ggplot2)

#read in full data frame
df <- read.csv("~/Dropbox/projects/AJH_DiveRS/sse_review/sse_review_table - main_table.csv")

#unique combinations of study/order
df_mod<- unique(df[c("study", "sse_model","year")])


#get counts of model use per year
ym<-df_mod %>%
  dplyr::count(year, sse_model, .drop = F)
ym

#fill in non-existing combinations with 0
years <- unique(ym$year)
sse_models <- unique(ym$sse_model)
combinations <- expand.grid(year = years, sse_model = sse_models)
ym2 <- full_join(ym, combinations, by = c("year" = "year", "sse_model" = "sse_model")) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  arrange(year, sse_model)

# Plot stacked area chart
ggplot(ym2, aes(x=year, y=n, fill=sse_model)) +
  geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  ggtitle("-SSE model use over time")

#
ggplot(ym2, aes(x=year, y=n, fill=sse_model)) +
  geom_area()

###
#  streamgraph
###
library(streamgraph)
# Basic stream graph: just give the 3 arguments
pp <- streamgraph(ym2, key="sse_model", value="n", date="year",interactive = F,offset="zero", interpolate="cardinal")

pp %>%
  sg_axis_x(1, "year", "%Y") %>%
  sg_fill_tableau("cyclic")

###
# stacked barplot
###

#get counts of studies per year
ys<-df_mod %>%
  dplyr::count(study, year, .drop = F)
df_ys<-data.frame(table(ys$year))
df_ys$Var1<-as.numeric(as.character(df_ys$Var1))
str(df_ys)

ggplot(ym, aes(y=n, x=year)) +
  geom_bar(aes(fill = sse_model),position="stack", stat="identity") +
  scale_y_continuous(breaks = seq(0, 70, by = 5), expand = expansion(mult = c(0, .1))) +
  scale_x_continuous(breaks = seq(2007, 2020, by = 1)) +
  xlab("Publication year") +
  ylab("Frequency of model use") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.2, color="black"),
    panel.grid.minor.y = element_line( size=.1, color="grey"),
    panel.background = element_rect(fill = "white", colour = "black",
                                    size = 1, linetype = "solid")) +
  geom_line(data=df_ys, aes(x = Var1, y = Freq, group=1), size = 1, color="black") +
  geom_point(data=df_ys, aes(x = Var1, y = Freq, group=1), size = 2, color="black")

# variable widths not possible in ggplot2 without geom_rect() craziness