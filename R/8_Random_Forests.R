library(dplyr)
# read data
read.csv2("dat/heart.csv",sep=",") %>%
  mutate(Herzkrank    = factor(ifelse(Herzkrank == 0, "gesund", "krank")),
         Geschlecht   = factor(ifelse(Geschlecht == 0, "female", "male")),
         Brustschmerz = factor(Brustschmerz),
         Blutzucker   = factor(ifelse(Blutzucker == 1, "erhoeht", "normal")),
         EKG          = factor(EKG),
         Angina       = factor(ifelse(Angina == 1, "ja", "nein")),
         Depression1  = as.numeric(Depression1),
         Depression2  = factor(Depression2),
         Thal         = factor(Thal)) %>%
  select(-Thal) -> A

library(randomForest)
rf1 <- randomForest(Herzkrank ~ ., data = A)
rf1
importance(rf1)
varImpPlot(rf1, main = NULL)



