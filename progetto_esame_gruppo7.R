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
  pasticcerie <- import("C:/Users/MirkoLorenzoLaRocca/UFS/UFS07/Esercizi/GRUPPO 6-7. Spain_Bakery.xlsx")
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
                                        "rud*", "personal*", "bonit*", "cuidad*", "atten*", "desagadrad*", "educad*", "simpati*","friend*",
                                        "incred*","genial*","antip*","atenc*","perfec*","lent*","pesim*"),
                          
                          Qualità = c( "val*", "bon*", "cup*", "calid*", "excel*", "mal*", "buen*", "saboros*",
                                      "estupend*", "complet*","peqe", "tant*", "grand*", "simpl*","delici*","content*","estup*","grea*","maj*",
                                      "peor*","gust*","bien*","perfec*","ecxelent*","ric*","fri*","fatal*","pesim*","recomend*","espetac*"),
                          
                          Prezzo = c( "prec*", "car*", "paga*", "bass*", "peqe*", "poc*", "derec*"),
                          
                          
                          Location = c("limp*", "suci*", "gran*", "bonit*", "peqe", "local*", "locatio*", "posici*", "centr*",
                                       "espacio*", "lind*", "encant*", "cuidad* ","preci*","perfec*","agradab*","fri*","espetac*")
                          ))

campioni_R <- import("C:/Users/MirkoLorenzoLaRocca/UFS/UFS07/Esercizi/campioni_R.xlsx")
campioni_R_2 <- select(campioni_R, !sentiment_score)

PasticcierieSenzaCampioni <- as.data.frame(anti_join(pasticcerie, campioni_R_2))



#Algoritmo Supervisionato
#Training set
#install.packages("readtext")
#install.packages("quanteda.textstats")

#Corpus
Corpus_campioni_R <- corpus(campioni_R)
Analisi_testo <- textstat_summary(Corpus_campioni_R)

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

#Algoritmo SemiSupervisionato
library(newsmap)

#Creo la Dfm delle pasticcierie e il corpus
Corpus_pasticcierie <- corpus(pasticcerie)
Analisi_testo <- textstat_summary(Corpus_pasticcierie)

Dfm_Pasticcierie <- Corpus_pasticcierie %>%
  tokens(remove_punct = T, remove_numbers = T) %>%
  tokens_tolower() %>%
  tokens_wordstem() %>%
  tokens_remove(c(stopwords("spanish"), "y", "el", "muy","ha","la","las","en","vi","un","sin","me")) %>%
  dfm()

Dfm_SSVPasticcierie <- dfm_lookup(Dfm_Test, Driver)
Dfm_SSVPasticcierie

#Utilizziamo il dizionario creato in precedenza per classicare le categorie da analizzare
#creaiamo la dfm del algoritmo semisupervisionato
Dfm_SSVPasticcierie <- Corpus_pasticcierie %>%
  tokens(remove_punct = T, remove_numbers = T) %>%
  tokens_tolower() %>%
  tokens_wordstem() %>%
  tokens_remove(c(stopwords("spanish"), "y", "el", "muy","ha","la","las","en","vi","un","sin","me")) %>%
  dfm()
Dfm_SSVPasticcierie <- dfm_lookup(Dfm_SSVPasticcierie, Driver)
Dfm_SSVPasticcierie


#Training Stage 
Text_Model <- textmodel_newsmap(Dfm_Pasticcierie, Dfm_SSVPasticcierie)
Text_Model$model[, 1:30]

#Predizione
predict(Text_Model)[1:15]

Dfm_SSVPasticcierie$Semisupervisionato <- predict(Text_Model)
str(Dfm_SSVPasticcierie)

#Calcoliamo la percentuale di menzione dei vari driver all'interno delle recensioni
round(prop.table(table(predict(Text_Model))), 2)*100

#Classificazione
#Naive Bayes Model
install.packages("naivebayes")
library(naivebayes)
#settiamo il seed
set.seed(123)

#Lanciamo il modello
system.time(NaiveBayesModel <- multinomial_naive_bayes(x = Matrice_Training,
                                                       y = Dfm_Training@docvars$sentiment_score,
                                                       laplace = 1))
summary(NaiveBayesModel)

#Salviamo la predizione in un oggetto
Test_predictNB <- predict(NaiveBayesModel, Matrice_Test)
table(Test_predictNB)
round(prop.table(table(Test_predictNB)), 2)

#random Forest
install.packages("randomForest")
library(randomForest)
set.seed(123)
system.time(RF <- randomForest( y = Dfm_Training@docvars$sentiment_score, 
                                x = Matrice_Training,
                                importance = TRUE,
                                do.trace = FALSE,
                                ntree = 500))
RF
#Quante features sono state usate per la creazione dei subset
sqrt(length(Dfm_Training@Dimnames$features))

#Calcolo degli errori
30+21+2

#Numero dei testi calssificati
nrow(campioni_R)
53/200

#Grafico Errori
plot(RF, type = "l", col = c("black", "steelblue4","violetred4", "springgreen4"),main = "Random Forest Model Errors: sentiment variable")
legend("topright", horiz = F, cex = 0.7, fill = c("springgreen4", "black", "steelblue4", "violetred4"), c("Positive error", "Average error", "Negative error", "Neutral error"))

Errori <- as.data.frame(RF$err.rate)
#Estraiamo il numero di tree associati con l'errore più basso
which.min(Errori$OOB)
#Corregiamo 
set.seed(123)
system.time(RF2 <- randomForest( y = Dfm_Training@docvars$sentiment_score, 
                                x = Matrice_Training,
                                importance = FALSE,
                                do.trace = FALSE,
                                ntree = 1))
RF2
#Controlliamo con il grafico
plot(RF2, type = "l", col = c("black", "steelblue4","violetred4", "springgreen4"),main = "Random Forest Model Errors: sentiment variable")
legend("topright", horiz = F, cex = 0.7, fill = c("springgreen4", "black", "steelblue4", "violetred4"), c("Positive error", "Average error", "Negative error", "Neutral error"))

#Predizione
system.time(Test_PredictRF <- predict(RF2, Matrice_Test, type="class"))
table(Test_PredictRF)
round(prop.table(table(Test_PredictRF)), 2)

#Support Vector machine
#install.packages("iml")
#install.packages("future")
#install.packages("future.callr")
#install.packages("e1071")

library(iml)
library(future)
library(future.callr)
library(e1071)

#Impostazione del seed
set.seed(123)

#Eseguiamo il modello
system.time(SupportVectorMachine <- svm(y = Dfm_Training@docvars$sentiment_score, x = Matrice_Training, kernel = "linear", cost = 1))

#support vectors
length(SupportVectorMachine$index)

#predizione dati
system.time(test_predictedSV <- predict(SupportVectorMachine, Matrice_Test))

#visualizzazione sentiment
table(test_predictedSV)
round(prop.table(table(test_predictedSV)))

#Aggiungiamo la variabile al test set
campioni_R_3 <- as.data.frame(PasticcierieSenzaCampioni)
campioni_R_3$predictionSV <- test_predictedSV

#Confrontiamo i risultati
distribuzione <- as.data.frame(rbind(prop.table(table(Test_predictNB)), prop.table(table(Test_PredictRF)), prop.table(table(test_predictedSV))))
str(distribuzione)

#aggiungo algoritmo di riferimento
distribuzione$algoritmo <- c("neviBayes", "randomForest", "supportVectorMachine")

install.packages("reshape2")
library(reshape2)

df.long <- melt(distribuzione, id.vars = c("algoritmo"))
str(df.long)

library(ggplot2)
#Creiamo un plot
ggplot(df.long,aes(algoritmo,value,fill=variable))+
  geom_bar(position="dodge",stat="identity") + scale_fill_manual(values = c("violetred3", "yellow3", "green4")) +
  labs(title = "Comparazione delle predizioni") +
  theme(axis.text.x = element_text(color="#993333", angle=90)) + coord_flip() +
  ylab(label="Proporzione delle categorie nel test set") + xlab("Algoritmi") +
  guides(fill=guide_legend(title="Categorie di \nsentiment")) +
  theme(plot.title = element_text(color = "black", size = 12, face = "plain"),
        axis.title=element_text(size=11,face="plain"),
        axis.text= element_text(size =10, face = "italic")
  )
