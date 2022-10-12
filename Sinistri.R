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
          for(rData in rpoisData)
          {
            if(rData != 0) #quando il valore simulato della poisson é diverso da zero
            {
              # temporaneamente mettiamo dei valori fissi
              nu = 8
              lambda = 8
              costi = rgamma(rData, nu, lambda)
              for(costo in costi)
              {
                tempCostoData = c(rData, cardinalPoint, ageSplit, classes, motorStrength, carAge)
                fullCostoDataVector = append(fullCostoDataVector, tempCostoData)
              }
            }
            
            tempData = c(rData, cardinalPoint, ageSplit, classes, motorStrength, carAge)
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
colnames(fullDataFrame)[2] <- "Zona geografica"
colnames(fullDataFrame)[3] <- "Età"
colnames(fullDataFrame)[4] <- "Classe di merito"
colnames(fullDataFrame)[5] <- "Cilindrata"
colnames(fullDataFrame)[6] <- "Anzianità veicolo"

colnames(fullDataFrameCosto)[1] <- "Risarcimenti"
colnames(fullDataFrameCosto)[2] <- "Zona geografica"
colnames(fullDataFrameCosto)[3] <- "Età"
colnames(fullDataFrameCosto)[4] <- "Classe di merito"
colnames(fullDataFrameCosto)[5] <- "Cilindrata"
colnames(fullDataFrameCosto)[6] <- "Anzianità veicolo"

#USIAMO BRMS
require(brms)
require(ggplot2)
require(gdata)
require(dplyr)
require(parallel)
require(cowplot)

#otteniamo numero di core su cui far girare il programma
options(mc.cores = parallel::detectCores())

#fitting del modello per la poisson
fit.sinistri <- brm(Sinistri ~ exp(Età), data = fullDataFrame, family = ("poisson"), cores = getOption("mc.cores", 1))

#fitting del modello per la gamma
fit.costo <- brm(Risarcimenti ~ exp(Età), data = fullDataFrameCosto, family = ("gamma"), cores = getOption("mc.cores", 1))
