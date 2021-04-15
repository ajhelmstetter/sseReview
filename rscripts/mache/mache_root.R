###
# notes
###
# pdftotext doesn't like () in filenames

library(papieRmache)

#ERR
#1: In list_years[i] <- regmatches(list_txt[[i]]$doc_id, m)[[1]] :
#number of items to replace is not a multiple of replacement length

kw<-c(kw,"assume","ancestral")

in_dir <- "~/Dropbox/projects/AJH_DiveRS/pdfs_for_ssereview/contains_plants/"

ct <- clean.text(in_dir = in_dir, all_keywords = kw)

#term dataset
td <-
  generate.term.dataset(cleaned_text = ct,
                        in_dir = in_dir,
                        keywords = kw)

#highlight numbers?
no<-c("1","2","3","4","5","6","7","8","9")

results<-semi.auto.paired(
  in_dir = "~/Dropbox/projects/AJH_DiveRS/pdfs_for_ssereview/contains_plants/",
  keywords1 =  c("root"),
  keywords2 = c("state","assume","ancestral"),
  highlight = "sse",
  cleaned_text = ct,
  sorted_words = td
)



results<-semi.auto.value( in_dir = "~/Dropbox/projects/AJH_DiveRS/pdfs_for_ssereview/contains_plants/",
                           keywords =  c("root","assume","ancestral"),
                           highlight = c("state,","root","assume","ancestral"),
                           cleaned_text = ct,
                           sorted_words = td)


#write output file
#write.csv(df,"../Dropbox/projects/AJH_DiveRS/sse_review/")
