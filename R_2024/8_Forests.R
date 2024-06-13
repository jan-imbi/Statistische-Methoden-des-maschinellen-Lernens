library(dplyr)

A <- read.csv2("dat/heart.csv",sep=",") 
A$Geschlecht <- factor(A$Geschlecht)
A$Brustschmerz  <- factor(A$Brustschmerz)
A$Blutzucker   <- factor(A$Blutzucker)
A$EKG   <- factor(A$EKG)
A$Angina   <- factor(A$Angina)
A$Gefaesse   <- factor(A$Gefaesse)
A$Herzkrank <- factor(A$Herzkrank)



library(randomForest)

set.seed(123)
modl1 <- randomForest(Herzkrank ~ .,  data = A)
varImpPlot(modl1)

set.seed(123)
modl2 <- randomForest(Herzkrank ~ .,  data = A, mtry=5, nodesize=6)
varImpPlot(modl2)

set.seed(123)
modl3 <- randomForest(Herzkrank ~ .,  data = A, nodesize=6)
varImpPlot(modl3)


