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
                                        "rud*", "personal*", "bonit*", "cuidad*", "atten*", "desagadrad*", "educad*", "simpati*","friend*","incred*","genial*","antip*","atenc*","perfec*","lent*","pesim*"),
                          
                          Qualità = c( "val*", "bon*", "cup*", "calid*", "excel*", "mal*", "buen*", "saboros*",
                                      "estupend*", "complet*","peqe", "tant*", "grand*", "simpl*","delici*","content*","estup*","grea*","maj*","peor*","gust*","bien*","perfec*","ecxelent*","ric*","fri*","fatal*","pesim*","recomend*","espetac*"),
                          
                          Prezzo = c( "prec*", "car*", "paga*", "bass*", "peqe*", "poc*", "derec*"),
                          
                          
                          Location = c("limp*", "suci*", "gran*", "bonit*", "peqe", "local*", "locatio*", "posici*", "centr*",
                                       "espacio*", "lind*", "encant*", "cuidad* ","preci*","perfec*","agradab*","fri*","espetac*")
                          ))

campioni_R <- import("C:/Users/FilippoConsole/OneDrive - ITS Angelo Rizzoli/Desktop.old/RStudio/Esame-R/campioni_R.xlsx")
campioni_R_2 <- select(campioni_R, !sentiment_score)

PasticcierieSenzaCampioni <- as.data.frame(anti_join(pasticcerie, campioni_R_2))
print(typeof(Driver))




#Training set
install.packages("readtext")
install.packages("quanteda.textstats")

#Corpus
Corpus_campioni_R <- corpus(campioni_R)
Analisi_testo <- textstat_summary(Corpus_pasticcerie)

#DFM
Dfm_Training <- Corpus_campioni_R %>%
  tokens(remove_punct = T, remove_numbers = T) %>%
  tokens_tolower() %>%
  tokens_wordstem() %>%
  tokens_remove(c(stopwords("spanish"), "y", "el", "muy","ha","la","las","en","vi","un","sin","me")) %>%
  dfm()

topfeatures(Dfm_Training)

#Applicazione Dizionario alla DFM

Driver_Training <- dfm_lookup(Dfm_Training, Driver)
Driver_Training


#Test set
#Corpus
Corpus_PasticcierieSenzaCampioni <- corpus(PasticcierieSenzaCampioni)
Analisi_testo <- textstat_summary(Corpus_PasticcierieSenzaCampioni)

#DFM
Dfm_Test <- Corpus_PasticcierieSenzaCampioni %>%
  tokens(remove_punct = T, remove_numbers = T) %>%
  tokens_tolower() %>%
  tokens_wordstem() %>%
  tokens_remove(c(stopwords("spanish"), "y", "el", "muy","ha","la","las","en","vi","un","sin","me")) %>%
  dfm()

topfeatures(Dfm_Test)

#Applicazione Dizionario alla DFM

Driver_Test <- dfm_lookup(Dfm_Test, Driver)
Driver_Test

#Controllo se matchano
setequal(featnames(Dfm_Training), 
         featnames(Dfm_Test)) 

#Controllimo la lunghezza e la sistemiamo rendendola uguale
length(Dfm_Training@Dimnames$features)

length(Dfm_Test@Dimnames$features)

Dfm_Test2 <- dfm_match(Dfm_Test, features = featnames(Dfm_Training))

setequal(featnames(Dfm_Training), 
         featnames(Dfm_Test2)) 

#Creo le matrci basandomi su i due DFM test e training
Matrice_Training <- as.matrix(Dfm_Training)
Matrice_Test <- as.matrix(Dfm_Test2)

#Trasformazione della variabile su cui vogliamo svolgere il training

str(Dfm_Training@docvars$sentiment_score)

#Trasformiamo in factor
Dfm_Training@docvars$sentiment_score <- as.factor(Dfm_Training@docvars$sentiment_score)
str(Dfm_Training@docvars$sentiment_score)
