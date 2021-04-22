# Als erstes wollen wir den Datensatz einlesen. Zu beachten ist, dass die
# einzelnen Werte mit einem "," getrennt sind, und "." das Dezimalzeichen ist
A <- read.csv("dat/weight-height.csv", dec = ".", sep = ",")

# Größe und Gewicht in Zentimeter und kg umrechnen
A$Height <- A$Height * 2.54
A$Weight <- A$Weight /2.2046
A$Gender <- as.factor(A$Gender)

# Wir wollen den Datensatz A zufällig "mischen" und dann in einen Test-
# un Trainingsdatensatz aufteilen
set.seed(123)
A <- A[sample(nrow(A)), ]
A_train = A[1 : (nrow(A) / 2), ]
A_test = A[(nrow(A) / 2 + 1) : nrow(A), ]

# Nun schätzen wir ein linears Modell (quadratische Verlustfunktion)
lin_mod <- lm(Height ~ Weight, A)
# Wir verwenden dieses Modell um die Werte für den Testdatensatz vorauszusagen
# und betrachten die mittleren Residuenquadrate
mean((predict(lin_mod, A_test) - A_test$Height)^2)

# Vermutlich müssen Sie das "MASS" paket installieren mit dem Befehl
# install.packages("MASS")
library(MASS)

hub_mod <- rlm(Height ~ Weight, A)
# Die mittleren Residuenquadrate für die robuste Regressionsmethode sind
# in diesem Datensatz nicht viel schlechter, obwohl wir uns in einem
# Fall mit eher "gutartigen" Fehlern befinden, in dem ein lineares Modell
# auch gut funktioniert
mean((predict(hub_mod, A_test) - A_test$Height)^2)




