library(dplyr)
library(rpart)
library(rpart.plot)
# Aufgabe 1
read.csv2("dat/weight-height.csv", sep =",", dec=".") %>%
  mutate(
    Weight = Weight / 2.2046, # weight is given in pounds
    Height = Height * 2.54 # height is given in inches
  ) -> dat
tree <- rpart::rpart(Gender~., data=dat, method = "class",
                     control = rpart.control(minsplit = 100L))
rpart.plot::prp(tree, type = 5)

# Aufgabe 2
pred_vs <- predict(tree, dat)
pred_values <- ifelse(pred_vs[,"Male"] > pred_vs[,"Female"],
                      "Male",
                      "Female")
table(pred_values, dat$Gender)
