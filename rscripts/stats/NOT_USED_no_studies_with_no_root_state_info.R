#read in full data frame
df <- read.csv("~/Dropbox/projects/AJH_DiveRS/sseReview/data/sse_review_table - main_table.csv")

#reduce to a single binary result per model per study
df_root <- df %>%
  group_by(study, model_no) %>%
  dplyr::slice(which.max(div_inc))

#total number of models with no putative root state information
no_root<-length(df_root[is.na(df_root$putative_ancestral_state),]$study)
no_root

#Total number of models analysed
tot_mods<-length(unique(df[c("study","sse_model","model_no")])$study)
tot_mods

#proportion with no putative root state info
no_root/tot_mods

