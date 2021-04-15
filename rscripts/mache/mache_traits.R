###
# notes
###
# pdftotext doesn't like () in filenames

library(papieRmache)

#ERR
#1: In list_years[i] <- regmatches(list_txt[[i]]$doc_id, m)[[1]] :
#number of items to replace is not a multiple of replacement length

in_dir <- "~/Dropbox/projects/AJH_DiveRS/pdfs_for_ssereview/contains_plants/"

ct <- clean.text(in_dir = in_dir, all_keywords = kw)

#traits
keywords <- read.table("~/Dropbox/projects/AJH_DiveRS/sse_review/traits.txt")
traits <- keywords$V1
traits

#models
models <- kw[84:96]
models

#term dataset
td <-
  generate.term.dataset(cleaned_text = ct,
                        in_dir = in_dir,
                        keywords = kw)


df <- semi.auto.paired(
  in_dir = "~/Dropbox/projects/AJH_DiveRS/pdfs_for_ssereview/contains_plants/",

  #keywords that the section is based around
  keywords1 = c("habit"),

  #keywords that must ALSO be in the section
  keywords2 = c("trait","state"),

  #words to highlight
  highlight = models,
  cleaned_text = ct,
  sorted_words = td
)

#write output file
#write.csv(df,"../Dropbox/projects/AJH_DiveRS/sse_review/speciation_A_D.csv")

###
#NOT PLANT/EMPIRICAL/DUPLICATES
##

#check format strange - everywhere, sentences broken
#Chazot_et_al_2018_Contrasting_patterns_of_Andean_diversification_amo.txt
#Kong_et_al_2017_Both_temperature_fluctuations_and_East_Asian_monso.txt
#Bacon_et_al_2018_Iriarteeae_palms_tracked_the_uplift_of_Andean_Cord.txt

#check if also tropical habitiat affects div rates
#Kraichak_et_al_2015_A_unique_trait_associated_with_increased_diversif

#combined output
a1<-read.csv("../Dropbox/projects/AJH_DiveRS/sse_review/trait_increase.csv")
a2<-read.csv("../Dropbox/projects/AJH_DiveRS/sse_review/trait_bisse_musse.csv")
a3<-read.csv("../Dropbox/projects/AJH_DiveRS/sse_review/trait_hisse.csv")
a4<-read.csv("../Dropbox/projects/AJH_DiveRS/sse_review/trait_quasse.csv")
a5<-read.csv("../Dropbox/projects/AJH_DiveRS/sse_review/trait_bisseness_chromosse_fisse_geohisse_secsse.csv")
a6<-read.csv("../Dropbox/projects/AJH_DiveRS/sse_review/trait_cid.csv")
a7<-read.csv("../Dropbox/projects/AJH_DiveRS/sse_review/trait_geosse.csv")

df<-cbind(a1,a2,a3,a4,a5,a6,a7)
write.csv(df,"../Dropbox/projects/AJH_DiveRS/sse_review/combined_trait.csv")
