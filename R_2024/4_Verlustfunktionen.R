set.seed(123)
X_train = rnorm(1000, mean = 0, sd = 3)
y_train = 2 * X_train - 1 + rcauchy(1000)

X_test = rnorm(1000, mean = 0, sd = 3)
y_test = 2 * X_test - 1 + rnorm(1000)



plot(X_train, y_train)
plot(X_train, y_train, ylim = c(-5, 5))


fitlm <- lm(y ~ X, data = data.frame(y = y_train, X = X_train))
library(MASS)
fitrlm <- rlm(y ~ X, data = data.frame(y = y_train, X = X_train))

mean((y_test - predict(fitlm, data.frame(X = X_test)))^2)
mean((y_test - predict(fitrlm, data.frame(X = X_test)))^2)

library(readr)
A <- read_csv("dat/weight-height.csv")
A$Weight <- A$Weight / 2.2046
A$Height <- A$Height * 2.54
A$Gender <- factor(A$Gender)

set.seed(1)
ind <- sample.int(nrow(A), size=round(nrow(A)*0.2))
A_test <- A[ind,]
A_train <- A[-ind,]
fitlm <- lm(Height ~ Weight, data = A_train)
fitrlm <- rlm(Height ~ Weight, data = A_train)

mean((A_test$Height - predict(fitlm, newdata = A_test))^2)
mean((A_test$Height - predict(fitrlm, newdata = A_test))^2)












