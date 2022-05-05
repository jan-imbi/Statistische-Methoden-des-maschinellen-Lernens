# Aufgabe 1
set.seed(12345)
library(evidence)
data(fev)
n <- nrow(fev)
train <- sample(n, .6*n) # 60% of the data is used for training
lm.fit <- lm(FEV ~ Age + Hgt,
             data = fev,
             subset = train)
mean((fev$FEV-predict(lm.fit, fev))[-train]^2)

set.seed(123456)
data(fev)
n <- nrow(fev)
train <- sample(n, .6*n) # 60% of the data is used for training
lm.fit <- lm(FEV ~ Age + Hgt,
             data = fev,
             subset = train)
mean((fev$FEV-predict(lm.fit, fev))[-train]^2)

set.seed(1234567)
data(fev)
n <- nrow(fev)
train <- sample(n, .6*n) # 60% of the data is used for training
lm.fit <- lm(FEV ~ Age + Hgt,
             data = fev,
             subset = train)
mean((fev$FEV-predict(lm.fit, fev))[-train]^2)

set.seed(12345)
data(fev)
n <- nrow(fev)
train <- sample(n, .6*n) # 60% of the data is used for training
lm.fit <- lm(FEV ~ Age + Hgt + Sex + Smoke,
             data = fev,
             subset = train)
mean((fev$FEV-predict(lm.fit, fev))[-train]^2)


set.seed(123456)
data(fev)
n <- nrow(fev)
train <- sample(n, .6*n) # 60% of the data is used for training
lm.fit <- lm(FEV ~ Age + Hgt + Sex + Smoke,
             data = fev,
             subset = train)
mean((fev$FEV-predict(lm.fit, fev))[-train]^2)

set.seed(1234567)
data(fev)
n <- nrow(fev)
train <- sample(n, .6*n) # 60% of the data is used for training
lm.fit <- lm(FEV ~ Age + Hgt + Sex + Smoke,
             data = fev,
             subset = train)
mean((fev$FEV-predict(lm.fit, fev))[-train]^2)

# Aufgabe 2
set.seed(12345)
library(boot)
library(TH.data)
data(GBSG2)
glm.fit <- glm(cens ~ horTh + age + menostat + tsize + tgrade + pnodes,
               data=GBSG2, family = binomial(link = "logit"))
cv.error <- cv.glm(GBSG2, glm.fit)
cv.error$delta[1]
cv.error5 <- cv.glm(GBSG2, glm.fit, K=5)
cv.error5$delta[1]

set.seed(12345)
cv.error10 <- cv.glm(GBSG2, glm.fit, K=10)
cv.error10$delta[1]
cv.error50 <- cv.glm(GBSG2, glm.fit, K=50)
cv.error50$delta[1]

# Aufgabe 3
set.seed(12345)
data(weightgain)
theta.fct <- function(data, index){
  return(coef(lm(weightgain ~ ., data=data, subset=index)))
}
theta_boot <- boot(weightgain, theta.fct, R=200)
print(theta_boot)

boot.ci(theta_boot)
summary(lm(weightgain ~ ., data=weightgain))
