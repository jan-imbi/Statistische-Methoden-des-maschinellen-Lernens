library(dplyr)
read.csv2("dat/weight-height.csv", sep =",", dec=".") %>%
  mutate(
    Weight = Weight / 2.2046, # weight is given in pounds
    Height = Height * 2.54 # height is given in inches
  ) -> A

A$Gender <- as.factor(A$Gender)
# A$Gender2 <- A$Gender == "Male" 

log_mod <- glm(Gender ~ Height, data = A, family = binomial(link="logit"))
predict(log_mod, A, type = "response")
inv_log <- function(eta) 1/(1+exp(-eta))
inv_log(predict(log_mod, A))


probs <- predict(log_mod, A, type = "response")
prediction <- if_else(probs > 0.5, "Male", "Female")
table(A$Gender, prediction)


log_mod2 <- glm(relevel(Gender, ref = "Male") ~ Height, data = A, family = binomial(link="logit"))
predict(log_mod2, A, type = "response")

log_mod3 <- glm(as.factor(Gender) ~ ., data = A, family = binomial())
pred3 <- predict(log_mod3, A, type = "response")
table(A$Gender, if_else(pred3 > .5, "Male", "Female"))


summary(log_mod3)




