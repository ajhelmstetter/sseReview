###
# notes
###
# pdftotext doesn't like () in filenames

library(papieRmache)

in_dir <- "~/Dropbox/projects/AJH_DiveRS/pdfs_for_ssereview/contains_plants/"

ct <- clean.text(in_dir = in_dir, all_keywords = kw)

#term dataset
td <-
  generate.term.dataset(cleaned_text = ct,
                        in_dir = in_dir,
                        keywords = kw)

results<-semi.auto.paired(
  in_dir = "~/Dropbox/projects/AJH_DiveRS/pdfs_for_ssereview/contains_plants/",
  keywords1 =  c("sampling"),
  keywords2 = c("fraction","incomplete"),
  highlight = "state",
  cleaned_text = ct,
  sorted_words = td
)

write.csv(results,"../mache_sampling_fraction.csv
          ")
