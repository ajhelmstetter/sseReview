###
# Taxonomy
###
#
# Phylo with nodes as pies for effect on div rate, size is number of studies
#
# Sankey diagram
#
# Treemap
#


pdf("~/Dropbox/projects/AJH_DiveRS/sse_review/plots.pdf")

#rows where diversification rates increase
r_inc <- df[df$div_inc == 1, ]

#need to split ";" for orders

#unique combinations of study/order
df_ord <- unique(df[c("study", "order")])

#pie chart of orders in all studies
pie(sort(table(df_ord$order)), col = brewer.pal(12, "Set3"),main = "Orders in studies")

#unique combinations study/tip number
df_tips <- unique(df[c("study", "tips")])

#change from factor to number and omit NAs
tips <- as.numeric(df_tips$tips)
tips <- na.omit(tips)

#buckets of tip numbers
buckets = c(0, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 100000)

par(mar=c(8,3,3,3))

#plot barplot of all studies tip numbers
plot(cut(tips, buckets), las = 2, col = 4)

#unique combinations study/tip number when div is increasing
df_tips <- unique(r_inc[c("study", "tips")])
tips <- as.numeric(levels(df_tips$tips))[df_tips$tips]
tips <- na.omit(tips)

#stacked barplot with number of studies where div is increasing overlain
plot(cut(tips, buckets),
     las = 2,
     add = T,
     col = 1)

legend(
  "topright",
  legend = c("total no. studies", "studies w/ div rate increase"),
  col = c(4, 1),
  pch = 15
)

#trait types per study with div rate increase
r_inc_trait <- unique(r_inc[c("study", "trait_type")])

par(mar=c(3,3,3,3))

#pie chart showing trait types with div rate increase
pie(sort(table(r_inc_trait$trait_type)), col = brewer.pal(12, "Set3"),main="Trait types with diversification rate increase")

dev.off()
