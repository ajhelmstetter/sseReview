#read in data
df<-read.csv("data/sse_review_table - main_table.csv")

# Subset to only hidden state models
hidd<-c("HiSSE","MuHiSSE","GeoHiSSE","SecSSE","MiSSE")
df2<-df[df$sse_model %in% hidd, ]

#look at unique studies
sort(unique(df2$study))

#number of unique studies
length(unique(df2$study))