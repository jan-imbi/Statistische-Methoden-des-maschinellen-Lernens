library(MASS)
library(dplyr)
library(leaps)
library(caret)

# Aufgabe 1
dat <- read.csv2("dat/heart.csv", sep=",") %>%
  mutate(Herzkrank = factor(ifelse(Herzkrank == 0, "gesund", "krank")),
         Geschlecht = factor(ifelse(Geschlecht == 0, "female", "male")),
         Brustschmerz = factor(Brustschmerz),
         Blutzucker = factor(ifelse(Blutzucker == 1, "erhoeht", "normal")),
         EKG = factor(EKG),
         Angina = factor(ifelse(Angina == 1, "ja", "nein")),
         Depression1 = as.numeric(Depression1),
         Depression2 = factor(Depression2)) %>%
  dplyr::select(-Thal)

best_sub <- regsubsets(Herzkrank ~ ., data = dat, nvmax = 16)
summary(best_sub)

plot(best_sub, scale="bic")
plot(best_sub, scale="adjr2")

sum_best_sub <- summary(best_sub)

good_fit <- glm(Herzkrank ~ Geschlecht + Brustschmerz + Blutdruck + Cholesterin + Herzschlag+
      Angina + Depression1 + Depression2 + Gefaesse, data = dat, family = binomial())
summary(good_fit)



nullmodl <- glm(Herzkrank ~ 1, data = dat, family = binomial())

stepAIC(nullmodl, direction = "forward",
        scope = list(lower=Herzkrank ~ 1,
                     upper=Herzkrank ~ Alter + Geschlecht + Brustschmerz + Blutdruck + Cholesterin + Blutzucker + EKG + Herzschlag + Angina + Depression1 + Depression2 + Gefaesse + Herzkrank))


fullmodl <- glm(Herzkrank ~ Alter + Geschlecht + Brustschmerz + Blutdruck + Cholesterin + Blutzucker + EKG + Herzschlag + Angina + Depression1 + Depression2 + Gefaesse, data = dat, family = binomial())

stepAIC(fullmodl, direction = "backward",
        scope = list(lower=Herzkrank ~ 1,
                     upper=Herzkrank ~ Alter + Geschlecht + Brustschmerz + Blutdruck + Cholesterin + Blutzucker + EKG + Herzschlag + Angina + Depression1 + Depression2 + Gefaesse + Herzkrank))



startmodl <- glm(Herzkrank ~ Alter + Geschlecht + Brustschmerz + Blutdruck + Cholesterin, data = dat, family = binomial())

stepAIC(startmodl, direction = "both",
        scope = list(lower=Herzkrank ~ 1,
                     upper=Herzkrank ~ Alter + Geschlecht + Brustschmerz + Blutdruck + Cholesterin + Blutzucker + EKG + Herzschlag + Angina + Depression1 + Depression2 + Gefaesse + Herzkrank))

