# Als erstes wollen wir den Datensatz einlesen. Zu beachten ist, dass die
# einzelnen Werte mit einem "," getrennt sind, und "." das Dezimalzeichen ist
A <- read.csv("dat/weight-height.csv", dec = ".", sep = ",")

# Größe und Gewicht in Zentimeter und kg umrechnen
A$Height <- A$Height * 2.54
A$Weight <- A$Weight /2.2046
A$Gender <- as.factor(A$Gender)


log_mod1 <- glm(Gender ~ Height, A, family = binomial(link="logit"))
pred1 <- predict(log_mod1, A, type="response")

table(ifelse(pred1>0.5, "Male", "Female"), A$Gender)
