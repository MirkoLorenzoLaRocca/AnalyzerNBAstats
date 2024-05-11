#Avvio librerie----
  library(rio)
  library(dplyr)
  #install.packages("quanteda")
  #install.packages("newsmap")
  library(quanteda)
  library(newsmap)
  library(quanteda.textstats)
  library(readtext)

#Importazione DataSet----
  pasticcerie <- import("GRUPPO 6-7. Spain_Bakery.xlsx")
  pasticcerie <- filter(pasticcerie, lang_value=="es") #teniamo solo le recensioni segnate come spagnole
  
#Esaminazione DataSet----
  str(pasticcerie) #ispezioniamo il dataset e notiamo che il tipo della variabile likes è chr quando ci aspettavamo un int
  table(pasticcerie$likes) #notiamo che tutta la colonna likes è NA decidiamo quindi di eliminarla perchè non implica perdita di informazione
  pasticcerie <- select(pasticcerie, -likes) #eliminiamo la colonna likes
  str(pasticcerie) #verifichiamo che effettvamente sia stata eliminata
  
  #continuiamo l'ispezione
  
  table(pasticcerie$score_rating) #vediamo che la colonna score_rating è corretta
  table(pasticcerie$Players) #vediamo a chi si riferiscono le recensioni
  sum(is.na(pasticcerie$Players)) #vediamo che non ci sono NA nella colonna Players quindi continuiamo
  sum(is.na(pasticcerie$score_rating)) #gli score rating non contengono NA
  sum(is.na(pasticcerie$text)) #i text non contengono NA
  
  #estraiamo 200 recensioni a caso per classificarle a mano per poi allenare l'algoritmo
  # campioni <- sample_n(pasticcerie, 200)
  library(writexl) #avviamo la libreria per esportare i campioni su excel e analizzarli più comodamente
  # write_xlsx(campioni, "campioni.xlsx")

Driver <- dictionary(list(Personale = c("amabl*", "cordial*", "empatic*", "dispo*", "groser*", "maleduca*", "descort*",
                                        "rud*", "personal*", "bonit*", "cuidad*", "atten*", "desagadrad*", "educad*", "simpati*"),
                          Qualità = c( "val*", "bon*", "cup*", "calid*", "excel*", "mal*", "buen*", "saboros*",
                                      "estupend*", "complet*","peqe", "tant*", "grand*"),
                          
                          Prezzo = c( "prec*", "car*", "paga*", "bass*", "peqe*", "poc*", "derec*"),
                          
                          Location = c("limp*", "suci*", "gran*", "bonit*", "peqe", "local*", "locatio*", "posici*", "centr*",
                                       "espacio*", "lind*", "encant*", "cuidad* ")
                          ))

campioni_R <- import("C:/Users/FilippoConsole/OneDrive - ITS Angelo Rizzoli/Desktop.old/RStudio/Esame-R/campioni_R.xlsx")
campioni_R_2 <- select(campioni_R, !sentiment_score)

PasticcierieSenzaCampioni <- as.data.frame(anti_join(pasticcerie, campioni_R_2))
print(typeof(Driver))




#Training set
install.packages("readtext")
install.packages("quanteda.textstats")

Corpus_pasticcerie <- corpus(pasticcerie)
Analisi_testo <- textstat_summary(Corpus_pasticcerie)


Dfm_pasticcierie <- Corpus_pasticcerie %>%
  tokens(remove_punct = T, remove_numbers = T) %>%
  tokens_tolower() %>%
  tokens_wordstem() %>%
  tokens_remove(c(stopwords("spanish"), "y", "el", "muy","ha","la","las","en","vi","un","sin","me")) %>%
  dfm()

topfeatures(Dfm_pasticcierie)

#Applicazione Dizionario alla DFM

Driver_pasticcierie <- dfm_lookup(PasticcierieSenzaCampioni, Driver)
