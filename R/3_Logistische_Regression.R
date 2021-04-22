# Als erstes wollen wir den Datensatz einlesen. Zu beachten ist, dass die
# einzelnen Werte mit einem "," getrennt sind, und "." das Dezimalzeichen ist
A <- read.csv("dat/weight-height.csv", dec = ".", sep = ",")

# Größe und Gewicht in Zentimeter und kg umrechnen
A$Height <- A$Height * 2.54
A$Weight <- A$Weight /2.2046
A$Gender <- as.factor(A$Gender)

# Wir speichern die "Gender" Variable als Faktor ab
A$Gender <- as.factor(A$Gender)
# Die Referenzkategorie dieser Variable ist in diesem Fall "Female"
# Die Wahrscheinlichkeit für die "Male" Kategorie soll geschätzt werden
# "Male" bekommt den Contrast 1, Female den Contrast 0
contrasts(A$Gender)

# Wir spezifizieren die Binomialverteilungsfamilie mit der "logit" link funktion
log_mod1 <- glm(Gender ~ Height, A, family = binomial(link="logit"))
summary(log_mod1)
# Ein Vektor mit den geschätzten Wahrscheinlichkeiten für die "Male" Kategorie
pred1 <- predict(log_mod1, A, type="response")
# type = "response" führt dazu, dass der lineare prediktor direkt über
# die link funktion zu einer Wahrscheinlichkeit transformiert wird
# ohne type = "response" bekommen wir den Wert des linearen Prädiktors
predict(log_mod1, A)
# Das enspricht folgendem:
log_mod1$coefficients["(Intercept)"] +
  A$Height * log_mod1$coefficients["Height"]
# Wir könnten diese transformation auch manuell durchführen:
lin_pred <- log_mod1$coefficients["(Intercept)"] +
  A$Height * log_mod1$coefficients["Height"]

1 / (1 + exp(-lin_pred))


# Tabelle mit den wahren Prädiktionen in den Zeilen, den Prädiktionen in den Spalten
table(A$Gender, ifelse(pred1>0.5, "Male", "Female"))

log_mod2 <- glm(Gender ~ Height + Weight, A, family = binomial(link="logit"))
summary(log_mod2)
pred2 <- predict(log_mod2, A, type="response")
# Man vergleiche diese Kreuztabelle mit der Tabelle des Modells mit nur
# einem Prädiktor (log_mod1)
table(A$Gender, ifelse(pred2>0.5, "Male", "Female"))

