library(DescTools)
library(evidence)
data(fev)

index <- sample(nrow(fev), nrow(fev)*.6)
modl1 <- lm(FEV ~ Age + Hgt, data = fev, subset = index)
mean((predict(modl1, fev) - fev$FEV)[-index]^2)


library(boot)
modl1 <- glm(FEV ~ Age + Hgt, data = fev)

cv.glm(fev, modl1, K=10)

