# Installer les pacakages
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

library(tm)
library(SnowballC)
library("wordcloud")
library("RColorBrewer")
library(readxl)
library(tidyr)

Sys.setenv(LANG="FR")
setwd("C:\\Users\\feife\\Documents\\Python Scripts\\Transplant")

# read the entire table
base<- read_excel("base LgTx IA 24oct2018.xlsx", sheet="ensemble")

#create different base

base_immediate=base[base$immediate_extubation==1,]
base_non_immediate=base[base$immediate_extubation==0,]

base_second=base[base$secondary_intubation==1,]
base_non_second=base[base$secondary_intubation==0,]

base_vivant=base[is.na(base$date_de_deces),]
base_morte=base[!is.na(base$date_de_deces),]


#write the function of nuage_mot


nuage_mot_direct <-function(text_to_mining){
  
  #commentaires <- file(text_to_mining,encoding='UTF-8')
  #text <- readLines(commentaires)
  
  # Load the data as a corpus
  #docs <- Corpus(VectorSource(text))
  docs <- Corpus(VectorSource(text_to_mining))
  
  
  inspect(docs)
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove french common stopwords
  docs <- tm_map(docs, removeWords, stopwords("french"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("lesquels", "après")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 3,
            max.words=300, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
  # barplot(d[1:15,]$freq, las = 2, names.arg = d[1:15,]$word,
  #         col ="lightblue", main ="Mots les plus frequents",
  #         ylab = "Fréquences de mots")
  # 
}


# Examen_Echographique_initial

nuage_mot_direct(base_immediate$Examen_Echographique_initial)
nuage_mot_direct(base_non_immediate$Examen_Echographique_initial)

nuage_mot_direct(base_second$Examen_Echographique_initial)
nuage_mot_direct(base_non_second$Examen_Echographique_initial)

nuage_mot_direct(base_vivant$Examen_Echographique_initial)
nuage_mot_direct(base_morte$Examen_Echographique_initial)



# Ventilation_Unipulmonaire_Pb
nuage_mot_direct(base_immediate$Ventilation_Unipulmonaire_Pb)
nuage_mot_direct(base_non_immediate$Ventilation_Unipulmonaire_Pb)

nuage_mot_direct(base_second$Ventilation_Unipulmonaire_Pb)
nuage_mot_direct(base_non_second$Ventilation_Unipulmonaire_Pb)

nuage_mot_direct(base_vivant$Ventilation_Unipulmonaire_Pb)
nuage_mot_direct(base_morte$Ventilation_Unipulmonaire_Pb)

# Antibioprophylaxie

nuage_mot_direct(base_immediate$Antibioprophylaxie)
nuage_mot_direct(base_non_immediate$Antibioprophylaxie)

nuage_mot_direct(base_second$Antibioprophylaxie)
nuage_mot_direct(base_non_second$Antibioprophylaxie)


nuage_mot_direct(base_vivant$Antibioprophylaxie)
nuage_mot_direct(base_morte$Antibioprophylaxie)

# Pb_induction_detail
nuage_mot_direct(base_immediate$Pb_induction_detail)
nuage_mot_direct(base_non_immediate$Pb_induction_detail)

nuage_mot_direct(base_second$Pb_induction_detail)
nuage_mot_direct(base_non_second$Pb_induction_detail)


nuage_mot_direct(base_vivant$Pb_induction_detail)
nuage_mot_direct(base_morte$Pb_induction_detail)


# Immunosuppresseurs

#it seem that there are some patterns. check with docteurs
nuage_mot_direct(base_immediate$Immunosuppresseurs)
nuage_mot_direct(base_non_immediate$Immunosuppresseurs)
nuage_mot_direct(base_vivant$Immunosuppresseurs)
nuage_mot_direct(base_morte$Immunosuppresseurs)



# ATCD_chirugicaux

nuage_mot_direct(base_immediate$ATCD_chirugicaux)
nuage_mot_direct(base_non_immediate$ATCD_chirugicaux)
nuage_mot_direct(base_vivant$ATCD_chirugicaux)
nuage_mot_direct(base_morte$ATCD_chirugicaux)


# ATCD_medicaux

nuage_mot_direct(base_immediate$ATCD_medicaux)
nuage_mot_direct(base_non_immediate$ATCD_medicaux)
nuage_mot_direct(base_vivant$ATCD_medicaux)
nuage_mot_direct(base_morte$ATCD_medicaux)



#clampage

#cote_1

# Evolution_PAP_clampage_cote_1

nuage_mot_direct(base_immediate$Evolution_PAP_clampage_cote_1)
nuage_mot_direct(base_non_immediate$Evolution_PAP_clampage_cote_1)

nuage_mot_direct(base_second$Evolution_PAP_clampage_cote_1)
nuage_mot_direct(base_non_second$Evolution_PAP_clampage_cote_1)


nuage_mot_direct(base_vivant$Evolution_PAP_clampage_cote_1)
nuage_mot_direct(base_morte$Evolution_PAP_clampage_cote_1)

#evenements clampage cote 1 commentaires
nuage_mot_direct(base_immediate$evenements_clampage_cote_1_commentaires)
nuage_mot_direct(base_non_immediate$evenements_clampage_cote_1_commentaires)

nuage_mot_direct(base_second$evenements_clampage_cote_1_commentaires)
nuage_mot_direct(base_non_second$evenements_clampage_cote_1_commentaires)


nuage_mot_direct(base_vivant$evenements_clampage_cote_1_commentaires)
nuage_mot_direct(base_morte$evenements_clampage_cote_1_commentaires)

#Examen_Echographique_declampage_cote_1

nuage_mot_direct(base_immediate$Examen_Echographique_declampage_cote_1)
nuage_mot_direct(base_non_immediate$Examen_Echographique_declampage_cote_1)
nuage_mot_direct(base_vivant$Examen_Echographique_declampage_cote_1)
nuage_mot_direct(base_morte$Examen_Echographique_declampage_cote_1)

#cote2

nuage_mot_direct(base_immediate$evenements_clampage_cote_2_commentaires)
nuage_mot_direct(base_non_immediate$evenements_clampage_cote_2_commentaires)
nuage_mot_direct(base_vivant$evenements_clampage_cote_2_commentaires)
nuage_mot_direct(base_morte$evenements_clampage_cote_2_commentaires)

#Examen_Echographique_declampage_cote_2
nuage_mot_direct(base_immediate$Examen_Echographique_declampage_cote_2)
nuage_mot_direct(base_non_immediate$Examen_Echographique_declampage_cote_2)
nuage_mot_direct(base_vivant$Examen_Echographique_declampage_cote_2)
nuage_mot_direct(base_morte$Examen_Echographique_declampage_cote_2)

#Ventilation_Unipulmonaire_Pb
nuage_mot_direct(base_immediate$Ventilation_Unipulmonaire_Pb)
nuage_mot_direct(base_non_immediate$Ventilation_Unipulmonaire_Pb)
nuage_mot_direct(base_vivant$Ventilation_Unipulmonaire_Pb)
nuage_mot_direct(base_morte$Ventilation_Unipulmonaire_Pb)


#extubation

nuage_mot_direct(base_immediate$Extubation_commentaire)
nuage_mot_direct(base_non_immediate$Extubation_commentaire)
nuage_mot_direct(base_vivant$Extubation_commentaire)
nuage_mot_direct(base_morte$Extubation_commentaire)

#Fibroscopie_fermeture

nuage_mot_direct(base_immediate$Fibroscopie_fermeture)
nuage_mot_direct(base_non_immediate$Fibroscopie_fermeture)
nuage_mot_direct(base_vivant$Fibroscopie_fermeture)
nuage_mot_direct(base_morte$Fibroscopie_fermeture)

# Problemes_survenus_VBP_2_commentaires

nuage_mot_direct(base_immediate$Problemes_survenus_VBP_2_commentaires)
nuage_mot_direct(base_non_immediate$Problemes_survenus_VBP_2_commentaires)
nuage_mot_direct(base_vivant$Problemes_survenus_VBP_2_commentaires)
nuage_mot_direct(base_morte$Problemes_survenus_VBP_2_commentaires)

