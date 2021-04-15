###
# notes
###
# pdftotext doesn't like () in filenames

library(papieRmache)

#ERR
#1: In list_years[i] <- regmatches(list_txt[[i]]$doc_id, m)[[1]] :
#number of items to replace is not a multiple of replacement length

in_dir <- "~/Dropbox/projects/AJH_DiveRS/pdfs_for_ssereview/contains_plants/subset/"

##add marker related stuff
kw<-c(kw,"gene","marker","genes","loci","fragments","regions",
      "plastid","chloroplast","nuclear","mitochondrial","ribosomal",
      "nDNA","DNA","mtDNA","cpDNA","pDNA","rDNA","cDNA","nuDNA")

ct <- clean.text(in_dir = in_dir, all_keywords = kw)

#term dataset
td <-
  generate.term.dataset(cleaned_text = ct,
                        in_dir = in_dir,
                        keywords = kw)


results<-semi.auto.value(in_dir = "~/Dropbox/projects/AJH_DiveRS/pdfs_for_ssereview/contains_plants/subset/",
                keywords = c("chloroplast","plastid","nuclear"),
                highlight = c('increase','decrease'),
                cleaned_text = ct,
                sorted_words = td)



#write output file
#write.csv(df,"../Dropbox/projects/AJH_DiveRS/sse_review/")
