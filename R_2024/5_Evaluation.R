# Aufgabe 1
library(evidence)
data(fev)
lmfev1 <- lm(FEV ~ Age + Sex, data = fev)
r2 <- summary(lmfev1)["r.squared"]
print(r2)
adjr2 <- summary(lmfev1)["adj.r.squared"]
print(adjr2)

lmfev2 <- lm(FEV ~ Age + Sex + Smoke, data = fev)
r2 <- summary(lmfev2)["r.squared"]
print(r2)
adjr2 <- summary(lmfev2)["adj.r.squared"]
print(adjr2)

lmfev3 <- lm(FEV ~ Age + Sex + Smoke + Hgt, data = fev)
r2 <- summary(lmfev3)["r.squared"]
print(r2)
adjr2 <- summary(lmfev3)["adj.r.squared"]
print(adjr2)

# Aufgabe 2
library(DescTools)
library(survival)
library(tidyverse)
data(pbc)
pbc$death <- factor(pbc$status==2)
pbc$hepato <-  factor(pbc$hepato)
pbc$stage <- factor(pbc$stage)

glmpbc1 <- glm(death ~ age + sex, data = pbc, family = binomial(link=logit))

PseudoR2(glmpbc1, which="McFadden")
PseudoR2(glmpbc1, which="McFaddenAdj")
PseudoR2(glmpbc1, which="Nagelkerke")

glmpbc2 <- glm(death ~ age + sex + chol, data = pbc,
               family = binomial(link=logit))
PseudoR2(glmpbc2, which="McFadden")
PseudoR2(glmpbc2, which="McFaddenAdj")
PseudoR2(glmpbc2, which="Nagelkerke")

glmpbc3 <- glm(death ~ age + sex + chol + stage, data = pbc,
               family = binomial(link=logit))
PseudoR2(glmpbc3, which="McFadden")
PseudoR2(glmpbc3, which="McFaddenAdj")
PseudoR2(glmpbc3, which="Nagelkerke")


glmpbc4 <- glm(death ~ age + sex + chol +hepato + stage, data = pbc,
               family = binomial(link=logit))
PseudoR2(glmpbc4, which="McFadden")
PseudoR2(glmpbc4, which="McFaddenAdj")
PseudoR2(glmpbc4, which="Nagelkerke")

# Aufgabe 3
glmpbc <- list(glmpbc1 = glmpbc1, glmpbc2 = glmpbc2,
               glmpbc3 = glmpbc3, glmpbc4 = glmpbc4)
lapply(glmpbc, BrierScore, scaled=F)

# Aufgabe 4
lapply(glmpbc, AIC)
lapply(glmpbc, BIC)

lmfev <- list(lmfev1,
              lmfev2,
              lmfev3)
lapply(lmfev, AIC)
lapply(lmfev, BIC)

# Aufgabe 5
library(TH.data)
data("GBSG2")
actual.values <- GBSG2$cens
Model <- glm(cens ~ horTh + age + menostat + tsize,
             family=binomial(link=logit), data=GBSG2)
Modelpredictions <- predict(Model, type="response")
library(pROC)
auc(actual.values, Modelpredictions)
roc.plot2 <- roc(actual.values, Modelpredictions)
coords(roc.plot2, x="best", input="threshold", best.method="youden")
plot(roc.plot2)










