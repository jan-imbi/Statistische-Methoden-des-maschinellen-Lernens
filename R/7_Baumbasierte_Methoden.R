library(dplyr)
library(rpart)
library(rpart.plot)
# Datensatz einlesen
read.csv2("dat/weight-height.csv", sep =",", dec=".") %>%
  mutate(
    Weight = Weight / 2.2046, # weight is given in pounds
    Height = Height * 2.54 # height is given in inches
  ) -> A

# Klassifikationsbaum fitten
tree <- rpart::rpart(Gender~., data=A, method = "class",
                     control = rpart.control(minsplit = 100L))

# Baum graphisch darstellen
rpart.plot::prp(tree, type = 5)

# Baum zur Vorhersage benutzen
# Es werden Wahrscheinlichkeiten vorhergesagt, und wir benutzen diese danach zur Klassifikation
pred_vs <- predict(tree, A)
pred_values <- ifelse(pred_vs[,"Male"] > pred_vs[,"Female"],
                      "Male",
                      "Female")

# Tabelle mit Ergebnissen
table(pred_values, A$Gender)
