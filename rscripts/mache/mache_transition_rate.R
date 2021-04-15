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
keywords <- c("transition")

#term dataset
td <-
  generate.term.dataset(cleaned_text = ct,
                        in_dir = in_dir,
                        keywords = keywords)


#did up to Areces_Berazain_and_Ackerman_2017_Diversification_and_fruit_evolution_in_eumalvoids

df <-
  semi.auto.value(
    in_dir = "~/Dropbox/projects/AJH_DiveRS/pdfs_for_ssereview/contains_plants/",
    keywords = keywords,
    out_file = "~/Dropbox/projects/AJH_DiveRS/sse_review/transition",
    cleaned_text = ct,
    sorted_words = td
  )
