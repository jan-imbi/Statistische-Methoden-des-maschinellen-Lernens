library(dplyr)
library(rpart)
library(rpart.plot)
read.csv2("dat/weight-height.csv", sep =",", dec=".") %>%
  mutate(
    Weight = Weight / 2.2046, # weight is given in pounds
    Height = Height * 2.54 # height is given in inches
  ) -> A

