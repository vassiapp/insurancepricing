#NUMERO DI SINISTRI

#la logica dietro ogni simulazione
#GRUPPO NORD 18-30 1C <1000 <1
sinistri1 <- rpois(8, 4*1*1*2*2/10)

#GRUPPO NORD 31-40 1C <1000 <1
sinistri2<- rpois(10, 3*1*1*2*2/10)

#Inizio simulazione. Riprendo la matrice iniziale del file.
nVector <- c(60,80,100,160)
cVector <- c(45,60,75,120)
sVector <- c(30,40,50,80)
iVector <- c(15,20,25,40)

#La salvo come un array unico per utilizzarlo nel numero di simulazioni da effettuare
ageCardinalMatrix <- array(c(nVector, cVector, sVector, iVector), dim = c(4, 4))

#numero che mi dirà quante volte ripete la simulazione
index = 1

#numero che deve coincidere alla fine del ciclo for con il numero totale della popolazione assicurata
numPeopleTotal = 0

fullDataVector=NULL
fullCostoDataVector=NULL

for(cardinalPoint in 1:4)
{
  # here 4 times
  for(ageSplit in 1:4)
  {
    # here 16 times
    for(classes in 1:2)
    {
      # here 32 times
      for(motorStrength in 1:2)
      {
        # here 64 times
        for(carAge in 1:2){
          
          #comando per selezionare il numero di persone per la Poisson
          numberPeople = round(ageCardinalMatrix[cardinalPoint,ageSplit]/8,0)
          numPeopleTotal = numPeopleTotal + numberPeople
          
          #i valori dei gamma
          ageFactor = 5 - ageSplit
          
          cardinalFactor = 1
          
          motorFactor = 2
          
          carAgeFactor = 2
          
          if(classes == 1)
          {
            classFactor = 1
          }
          else
          {
            classFactor = 3
          }
          
          #salviamo le realizzazioni della poisson in una variabile
          rpoisData = rpois(numberPeople, ageFactor*cardinalFactor*motorFactor*carAgeFactor*classFactor/10)
          
          
          #ciclo for per creare un vettore per il dataframe dei sinistri e del costo del risarcimento
          for(poisData in rpoisData)
          {
            if(poisData != 0) #quando il valore simulato della poisson é diverso da zero
            {
              # temporaneamente mettiamo dei valori fissi
              nu = 50
              if(motorStrength==1 && carAge==2) {
                mu=500
              } else if(motorStrength==1 && carAge==1) {
                mu=1500
              } else if(motorStrength==2 && carAge==1) {
                mu=2000
              } else {
                mu=1000
              }
              
              costi = rgamma(poisData, nu, nu/mu)
              
              for(costo in costi)
              {
                tempCostoData = c(costo, cardinalPoint, ageSplit, classes, motorStrength, carAge)
                fullCostoDataVector = append(fullCostoDataVector, tempCostoData)
              }
            }
            
            tempData = c(poisData, cardinalPoint, ageSplit, classes, motorStrength, carAge)
            fullDataVector = append(fullDataVector, tempData)
          }
          
          index = index+1
          # here 128 times
        }
      }
    }
  }
}
#converto il vettore in matrice
fullDataMatrix = matrix(fullDataVector, nrow=6, ncol = 1000)
fullCostoDataMatrix = matrix(fullCostoDataVector, nrow=6)

#ruoto la matrice di 90° per ottenere i valori della poisson e della gamma nella prima colonna
fullDataMatrix = t(fullDataMatrix[nrow(fullDataMatrix):1,])
fullDataMatrix = fullDataMatrix[,c(ncol(fullDataMatrix):1)]

fullCostoDataMatrix = t(fullCostoDataMatrix[nrow(fullCostoDataMatrix):1,])
fullCostoDataMatrix = fullCostoDataMatrix[,c(ncol(fullCostoDataMatrix):1)]

#converto la matrice in dataframe
fullDataFrame = as.data.frame(fullDataMatrix)
fullDataFrameCosto = as.data.frame(fullCostoDataMatrix)

#do diverse denominazioni alle colonne del dataframe
colnames(fullDataFrame)[1] <- "Sinistri"
colnames(fullDataFrame)[2] <- "Zona"
colnames(fullDataFrame)[3] <- "Eta"
colnames(fullDataFrame)[4] <- "Classe"
colnames(fullDataFrame)[5] <- "Cilindrata"
colnames(fullDataFrame)[6] <- "Veicolo"

colnames(fullDataFrameCosto)[1] <- "Risarcimenti"
colnames(fullDataFrameCosto)[2] <- "Zona"
colnames(fullDataFrameCosto)[3] <- "Eta"
colnames(fullDataFrameCosto)[4] <- "Classe"
colnames(fullDataFrameCosto)[5] <- "Cilindrata"
colnames(fullDataFrameCosto)[6] <- "Veicolo"

head(fullDataFrame)
fullDataFrame$`Zona`<-as.factor(fullDataFrame$`Zona`)
fullDataFrame$`Eta`<-as.factor(fullDataFrame$`Eta`)
fullDataFrame$`Classe`<-as.factor(fullDataFrame$`Classe`)
fullDataFrame$`Cilindrata`<-as.factor(fullDataFrame$`Cilindrata`)
fullDataFrame$`Veicolo`<-as.factor(fullDataFrame$`Veicolo`)
str(fullDataFrame)

head(fullDataFrameCosto)
fullDataFrameCosto$`Zona`<-as.factor(fullDataFrameCosto$`Zona`)
fullDataFrameCosto$`Eta`<-as.factor(fullDataFrameCosto$`Eta`)
fullDataFrameCosto$`Classe`<-as.factor(fullDataFrameCosto$`Classe`)
fullDataFrameCosto$`Cilindrata`<-as.factor(fullDataFrameCosto$`Cilindrata`)
fullDataFrameCosto$`Veicolo`<-as.factor(fullDataFrameCosto$`Veicolo`)
str(fullDataFrameCosto)

#USIAMO BRMS
require(brms)
require(ggplot2)
require(gdata)
require(dplyr)
require(parallel)
require(cowplot)
library("RColorBrewer")

#grafici per visualizzare i dati per i sinistri
sinistrieta <- ggplot(data=fullDataFrame, aes(x=Eta, y=Sinistri))
sinistrieta + geom_col(fill="#6e0a19") + theme_minimal() + labs(x = "x: Età assicurato", y = "y: Sinistri")


sinistrizona <- ggplot(data=fullDataFrame, aes(x=Zona, y=Sinistri))
sinistrizona + geom_col(fill="#A81026") + theme_minimal() + labs(x = "x: Zona geografica", y = "y: Sinistri")

sinistriclasse <- ggplot(data=fullDataFrame, aes(x=Classe, y=Sinistri))
sinistriclasse + geom_col(fill="#E01533") + theme_minimal() + labs(x = "x: Classe di merito", y = "y: Sinistri")

sinistricilindrata <- ggplot(data=fullDataFrame, aes(x=Cilindrata, y=Sinistri))
sinistricilindrata + geom_col(fill="#EE445E") + theme_minimal() + labs(x = "x: Cilindrata", y = "y: Sinistri")

sinistrianz <- ggplot(data=fullDataFrame, aes(x=Veicolo, y=Sinistri))
sinistrianz + geom_col(fill="#F58F9E") + theme_minimal() + labs(x = "x: Anzianità veicolo", y = "y: Sinistri")

#grafici per visualizzare i dati per i risarcimenti
claimseta <- ggplot(data=fullDataFrameCosto, aes(x=Eta, y=Risarcimenti))
claimseta + geom_col(fill="#2F6E74") + theme_minimal() + labs(x = "x: Età assicurato", y = "y: Costo per sinistro")

claimszona <- ggplot(data=fullDataFrameCosto, aes(x=Zona, y=Risarcimenti))
claimszona + geom_col(fill="#3c8c93") + theme_minimal() + labs(x = "x: Zona geografica", y = "y: Costo per sinistro")

claimsclasse <- ggplot(data=fullDataFrameCosto, aes(x=Classe, y=Risarcimenti))
claimsclasse + geom_col(fill="#47A5AE") + theme_minimal() + labs(x = "x: Classe di merito", y = "y: Costo per sinistro")

claimscilindrata <- ggplot(data=fullDataFrameCosto, aes(x=Cilindrata, y=Risarcimenti))
claimscilindrata + geom_col(fill="#7DC3CA") + theme_minimal() + labs(x = "x: Cilindrata", y = "y: Costo per sinistro")

claimsanz <- ggplot(data=fullDataFrameCosto, aes(x=Veicolo, y=Risarcimenti))
claimsanz + geom_col(fill="#A8D7DC") + theme_minimal() + labs(x = "x: Anzianità veicolo", y = "y: Costo per sinistro")

#otteniamo numero di core su cui far girare il programma
options(mc.cores = parallel::detectCores())

#fitting del modello per la poisson
fit.sinistri <- brm(Sinistri ~ Eta + Zona + Classe + Cilindrata + Veicolo, data = fullDataFrame, family = ("poisson"), cores = getOption("mc.cores", 1))
summary(fit.sinistri) #vedo i risultati
plot(fit.sinistri, ask=FALSE)

#vediamo solo con variabili significative
sinistri.eta <- brm(Sinistri ~ Eta, family="poisson", data = fullDataFrame, cores = getOption("mc.cores", 1))
sinistri.classe <- brm(Sinistri ~ Classe, family="poisson", data = fullDataFrame, cores = getOption("mc.cores", 1))
summary(sinistri.eta)
summary(sinistri.classe)
plot(sinistri.eta, ask=FALSE)
plot(sinistri.classe, ask=FALSE)

#proviamo con la binomiale negativa
fit.sinistri2 <- brm(Sinistri ~ Eta + Zona + Classe + Cilindrata + Veicolo, data = fullDataFrame, family = ("negbinomial"), cores = getOption("mc.cores", 1))
summary(fit.sinistri2) #vedo i risultati
plot(fit.sinistri2, ask=FALSE)

#calcolo WAIC per verificare la bontá del modello
brms::WAIC(fit.sinistri, fit.sinistri2)

#fitting del modello per la gamma
fit.costo <- brm(Risarcimenti ~ Eta + Zona + Classe + Cilindrata + Veicolo, data = fullDataFrameCosto, family = ("gamma"), cores = getOption("mc.cores", 1))
summary (fit.costo)
plot(fit.costo, ask=FALSE)

fit.costo2 <- brm(Risarcimenti ~ Eta + Zona + Classe + Cilindrata + Veicolo, data = fullDataFrameCosto, family = ("lognormal"), cores = getOption("mc.cores", 1))
summary (fit.costo2)
plot(fit.costo2, ask=FALSE)

#vediamo solo con variabili significative
claims.cilindrata <- brm(Risarcimenti ~ Cilindrata, family="gamma", data = fullDataFrameCosto, cores = getOption("mc.cores", 1))
claims.anzianita <- brm(Risarcimenti ~ Veicolo, family="gamma", data = fullDataFrameCosto, cores = getOption("mc.cores", 1))
summary(claims.cilindrata)
summary(claims.anzianita)
plot(claims.cilindrata, ask=FALSE)
plot(claims.anzianita, ask=FALSE)

#calcolo WAIC per verificare bontá del modello
brms::WAIC(fit.costo, fit.costo2)


