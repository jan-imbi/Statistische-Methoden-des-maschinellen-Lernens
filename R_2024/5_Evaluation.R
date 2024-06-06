library(DescTools)
library(evidence)
data(fev)

modl1 <- lm(FEV ~ Age + Sex, data = fev)
summary(modl1)

modl2 <- lm(FEV ~ Age + Sex + Smoke, data = fev)
summary(modl2)

modl3 <- lm(FEV ~ Age + Sex + Smoke + Hgt, data = fev)
summary(modl3)

library(survival)
data(pbc)
pbc$death <- pbc$status==2

modl1 <- glm(death ~ age + chol + sex, data = pbc, family = binomial())
modl2 <- glm(death ~ age + chol + sex + stage, data = pbc, family = binomial())
modl3 <- glm(death ~ age + chol + sex + stage + hepato, data = pbc, family = binomial())
PseudoR2(modl1, which="Nagelkerke")
PseudoR2(modl2, which="Nagelkerke")
PseudoR2(modl3, which="Nagelkerke")

PseudoR2(modl1, which="McFaddenAdj")
PseudoR2(modl2, which="McFaddenAdj")
PseudoR2(modl3, which="McFaddenAdj")

library(TH.data)
data(GBSG2)

modl <- glm(cens ~ horTh + age + menostat + tsize, data = GBSG2, family = binomial())

library(pROC)


roc(GBSG2$cens, predict(modl, type = "response"))
plot(roc(GBSG2$cens, predict(modl, type = "response")))

coords(roc(GBSG2$cens, predict(modl, type = "response")), x = "best")

