###
# notes
###
# pdftotext doesn't like () in filenames

library(papieRmache)

#could print PDFs as it goes through them (will find errors too)

# POTENTIAL PHD theses
#file 9 long
#echiverria file too

#issue with file 211 hunt and slater
#early use of references

#ERR
#1: In list_years[i] <- regmatches(list_txt[[i]]$doc_id, m)[[1]] :
#number of items to replace is not a multiple of replacement length

in_dir <- "~/Dropbox/projects/AJH_DiveRS/pdfs_for_ssereview/contains_plants/"
ct <- clean.text(in_dir = in_dir, all_keywords = kw)

#keywords
all_kw <-
  read.table("~/Dropbox/projects/AJH_DiveRS/sse_review/keywords.txt")
all_kw$V1[84:96]
keywords <- all_kw$V1[84:96]

#ERR
#46: In tm_map.SimpleCorpus(corpus_txt, removePunctuation,  ... :
#transformation drops documents
#47: In cbind(term_freq, names(sorted_words[[k]])) :
#  number of rows of result is not a multiple of vector length (arg 2)
td <-
  generate.term.dataset(cleaned_text = ct,
                        in_dir = in_dir,
                        keywords = keywords)

library(plyr)
#create dataframe with freqs of all keywords
for (i in 1:length(td)) {
  if (i == 1) {
    td_df <- data.frame(t(data.frame(td[[i]])))
  } else {
    td_df <- rbind.fill(td_df,
                        data.frame(t(data.frame(td[[i]]))))
  }
}

#generate and apply row names based on input files
file_names <- list.files(in_dir, pattern = "*.txt")
file_names <- gsub(".txt", "", file_names)
rownames(td_df) <- file_names
td_df[is.na(td_df)] <- 0
td_df <- td_df[, order(colnames(td_df))]

#write table
write.csv(td_df,
          "~/Dropbox/projects/AJH_DiveRS/sse_review/all_kw_table.csv")

df <-
  semi.auto(
    in_dir = "~/Dropbox/projects/AJH_DiveRS/pdfs_for_ssereview/contains_plants/",
    keywords = keywords,
    n = 5,
    out_file = "~/Dropbox/projects/AJH_DiveRS/sse_review/plant_mods",
    cleaned_text = ct,
    sorted_words = td
  )
