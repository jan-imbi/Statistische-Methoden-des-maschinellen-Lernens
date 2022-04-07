set.seed(123)
X_train = rnorm(1000, mean = 0, sd = 3)
y_train = 2 * X_train - 1 + rcauchy(1000)
X_test = rnorm(1000, mean = 0, sd = 3)
y_test = 2 * X_test - 1 + rnorm(1000)

plot(X_train, y_train)

plot(X_train, y_train, ylim = c(-25, 25))

lin_mod <- lm(y_train ~ X_train)
library(MASS)
rob_mod <- rlm(y_train ~ X_train)



#                                       vvvvvvv Hier war der Fehler.
pred_lin <- predict(lin_mod, data.frame(X_train = X_test))
# Vorher stand dort X_test = X_test. Unser Modell ist aber Y = (Intercept) + X_train * beta_1
# R findet also keinen Prädiktor mit dem Namen "X_train" im Datensatz, sucht dann eine Ebene darüber danach
# und findet eine Variable mit dem namen X_train in der globalen Umgebung.
# Dadurch haben wir effektiv das Modell wieder auf dem Trainingsdatensatz ausgewertet, und der übergebene
# Datensatz wird komplett ignoriert... natürlich komplett ohne Warnung.


pred_rob <- predict(rob_mod, data.frame(X_train = X_test))
pred2_lin <- lin_mod$coefficients[1] + lin_mod$coefficients[2] * X_test
pred2_rob <- rob_mod$coefficients[1] + rob_mod$coefficients[2] * X_test

mean((pred2_lin - y_test)^2)
mean((pred2_rob - y_test)^2)



library(dplyr)
read.csv2("dat/weight-height.csv", sep =",", dec=".") %>%
  mutate(
    Weight = Weight / 2.2046, # weight is given in pounds
    Height = Height * 2.54 # height is given in inches
  ) -> A

set.seed(123)
A <- A[sample(nrow(A)), ]
A_train = A[1 : (nrow(A) / 2), ]
A_test = A[(nrow(A) / 2 + 1) : nrow(A), ]

lin_mod2 <- lm(Height ~ Weight, data = A_train)
lin_mod2

hub_mod <- MASS::rlm(Height ~ Weight, data = A_train)
hub_mod

mean((A_test$Height - predict(lin_mod2, A_test))^2)
mean((A_test$Height - predict(hub_mod, A_test))^2)



