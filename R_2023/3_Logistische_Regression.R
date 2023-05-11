library(dplyr)
read.csv2("dat/weight-height.csv", sep =",", dec=".") %>%
  mutate(
    Weight = Weight / 2.2046, # weight is given in pounds
    Height = Height * 2.54 # height is given in inches
  ) -> A

A$Gender <- as.factor(A$Gender)
?glm


log_modl <- glm(Gender ~ Height, family = binomial(), data = A)
summary(log_modl)

?predict

predict(log_modl)
predict(log_modl, type = "response")

inv_log <- function(eta) 1/(1+exp(-eta))
inv_log(predict(log_modl))

A$Gender

table(A$Gender, ifelse(predict(log_modl, type = "response")  > 0.5, "Male", "Female")) 

log_modl2 <- glm(Gender ~ Height + Weight , family = binomial(), data = A)
summary(log_modl2)


table(A$Gender,
  ifelse(predict(log_modl2, type = "response") > 0.5, "Male", "Female"),
  deparse.level = 2)

table(A$Gender, ifelse(predict(log_modl, type = "response")  > 0.5, "Male", "Female")) 
      
summary(log_modl)
summary(log_modl2)


