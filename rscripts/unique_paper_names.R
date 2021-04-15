df<-read.table("~/Dropbox/projects/AJH_DiveRS/sse_review/temp.txt")
uni_df<-unique(df$V1)
write.table(uni_df,"temp2.txt")
