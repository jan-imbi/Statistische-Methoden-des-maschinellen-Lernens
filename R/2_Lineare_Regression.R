# Als erstes wollen wir den Datensatz einlesen. Zu beachten ist, dass die
# einzelnen Werte mit einem "," getrennt sind, und "." das Dezimalzeichen ist
A <- read.csv("dat/weight-height.csv", dec = ".", sep = ",")

# Größe und Gewicht in Zentimeter und kg umrechnen
A$Height <- A$Height * 2.54
A$Weight <- A$Weight /2.2046


# Ein lineares Modell ohne intercept ("y-Achenabschnitt") fitten
model_0 <- lm(Height ~ 0 + Weight, data = A)
# Informationen zu unserem Modell
summary(model_0)

# Bibliothek zur schöneren Visualisierung
library(ggplot2)
# Zeichne eine Punktwolke der Gewicht vs Größe Werte und zeichne unsere
# Regressionsgerade ein
ggplot(A, aes(x = Weight, y=Height)) +
  geom_point() +
  geom_abline(slope = model_0$coefficients[["Weight"]],
              intercept = 0,
              col = "blue", size = 1)
# Wie man sieht, ist ein Modell ohne Intercept nicht besonders zielführend bei
# unserer Datenlage

# Zeichne Residuen (= X * beta - Y) vs. die Gefitteten werte (X * beta)
ggplot(A, aes(x = predict(model_0, A), y=residuals(model_0))) +
  geom_point() +
  scale_x_continuous("Fitted values") +
  scale_y_continuous("Residuals")
# Man sieht das die Residuen von links nach rechts zuerst Größer als 0 sind
# und dann negativ werden. Das ist konsistent mit der ersten Graphik,
# bei der wir gesehen haben, dass die Punktewolke zuerst über unserer
# geschätzten Geraden liegt, und dann darunter

# Die summe der quadrierten Residuen beträgt:
sum(residuals(model_1)^2)
# Man kann diese auch von der "deviance" funktion ausgeben lassen:
deviance(model_1)


# Nun wollen wir alle vorangegangenen Schritte wiederholen, aber mit intercept
model_1 <- lm(Height ~ Weight, data = A)
summary(model_1)

library(ggplot2)
ggplot(A, aes(x = Weight, y=Height)) +
  geom_point() +
  geom_abline(slope = model_1$coefficients[["Weight"]],
              intercept = model_1$coefficients[["(Intercept)"]],
              col = "blue", size = 1)


ggplot(A, aes(x = predict(model_1, A), y=residuals(model_1))) +
  geom_point() +
  scale_x_continuous("Fitted values") +
  scale_y_continuous("Residuals")

deviance(model_1)


# Können wir den fit des Modells verbessern indem wir zusätzlich für die
# "Gender" variable adjustieren?

model_2 <- lm(Height ~ Weight + Gender, data = A)
summary(model_2)

# Eine lineare funktion mit einem Outcome und zwei abhängigen Variablen
# würde man im allgemeinen im 3-d Raum darstellen. Dadurch das "Gender"
# in unserem Datensatz aber nur 2 Ausprägungen hat, lässt sich die Funktion
# ohne Informationsverlust in den 2-d Raum projezieren. Wir sehen so zwei
# Geraden, eine für Gender="Male" und eine für Gender="Female"
# Man sieht auch, dass Männer im Mittel deutlich größer sind als Frauen. Männer
# sind in diesem Datensatz allerdings im Mittel auch deutlich schwerer als
# Frauen. Es stellt sich heraus, dass es in diesem linearen Modell der Modellfit
# optimiert wird, wenn man der Gender=Male Variable einen negativen Koeffizienten
# zuschreibt.

ggplot(A, aes(x = Weight, y=Height, col=Gender)) +
  geom_point() +
  geom_abline(slope = model_2$coefficients[["Weight"]],
              intercept = model_2$coefficients[["(Intercept)"]] + model_2$coefficients[["GenderMale"]],
              col = "blue", size = 1) + 
  geom_abline(slope = model_2$coefficients[["Weight"]],
              intercept = model_2$coefficients[["(Intercept)"]],
              col = "red", size = 1)

ggplot(A, aes(x = predict(model_2, A), y=residuals(model_2), col = Gender)) +
  geom_point() +
  scale_x_continuous("Fitted values") +
  scale_y_continuous("Residuals")

deviance(model_2)

# Nun wollen wir die BMI variable ableiten
A$BMI <- A$Weight / (A$Height/100)^2

# Und nun ein lineares Modell das Größe über BMI + intercept modelliert
model_3 <- lm(Height ~ BMI, data = A)

ggplot(A, aes(x = BMI, y=Height)) +
  geom_point() +
  geom_abline(slope = model_3$coefficients[["BMI"]],
              intercept = model_3$coefficients[["(Intercept)"]],
              col = "blue", size = 1) 

ggplot(A, aes(x = predict(model_3, A), y=residuals(model_3))) +
  geom_point() +
  scale_x_continuous("Fitted values") +
  scale_y_continuous("Residuals")

deviance(model_3)
# Die Residuenquadratsumme ist etwa 3.81 mal wie bei model_1. Das Modell fittet
# insgesamt viel schlechter. "BMI" ist eine Meßgröße des Gewichts, die
# schon für die Größe des Probanden adjustiert ist! Natürlich würde man dann
# erwarten, dass man dann anhand des BMI's keine Rückschlüsse mehr auf die
# Größe des Probanden treffen lassen.

# Als letztes wollen wir ein Modell rechnen das sowhl für BMI als auch die
# Gender Variable als Prädiktoren benutzt
model_4 <- lm(Height ~ BMI + Gender, data = A)
ggplot(A, aes(x = BMI, y=Height, col=Gender)) +
  geom_point() +
  geom_abline(slope = model_4$coefficients[["BMI"]],
              intercept = model_4$coefficients[["(Intercept)"]] + model_4$coefficients[["GenderMale"]],
              col = "blue", size = 1) + 
  geom_abline(slope = model_4$coefficients[["BMI"]],
              intercept = model_4$coefficients[["(Intercept)"]],
              col = "red", size = 1)

ggplot(A, aes(x = predict(model_4, A), y=residuals(model_4), col = Gender)) +
  geom_point() +
  scale_x_continuous("Fitted values") +
  scale_y_continuous("Residuals")

# Man sieht dass die Hinzunahme der Gender-Variablen dazu führt, dass sich die
# Fit vs. Residuen Wertepaare in zwei räumlich voneinander getrennten clustern
# konzentrieren. Man Vergleiche dieses Bild mit dem Residuenplot für model_3.

deviance(model_4)
# Die Residuenquadratsumme hat sich im Vergleich zum Modell ohne die "Gender"
# Variable verbessert.


# Eine kleine Anmerkungen zu Residuenplots im allgemeinen:
# Es ist gängig diese Plots mit den predicted values (X * beta) auf der x-Achse
# darzustellen, statt den tatsächlich gemessenen Werten Y (in unserem Fall Height)
# Dies ist auch die Darstellung in den default plots von R. Man betrachte z.B.
# (Sie müssen danach enter in der console drücken um die Plots angezeigt zu bekommen)
plot(model_1)

# Plots mit den gemessenen "Y-Werten" auf der X-Achse und den Residuen auf der
# Y-Achse haben jedoch auch eine interessante Interpretation.
# Ich verweise auf:
# https://stats.stackexchange.com/questions/5235/what-is-the-expected-correlation-between-residual-and-the-dependent-variable



