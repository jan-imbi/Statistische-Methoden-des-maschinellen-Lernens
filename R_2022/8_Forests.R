library(dplyr)
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
  dplyr::select(-Thal) -> dat


library(randomForest)
rf1 <- randomForest(Herzkrank ~ ., data = dat)
rf1
importance(rf1)
varImpPlot(rf1, main = NULL)


rf2 <- randomForest(Herzkrank ~ ., data = dat, mtry = 5)
rf2
importance(rf2)
varImpPlot(rf2, main = NULL)




