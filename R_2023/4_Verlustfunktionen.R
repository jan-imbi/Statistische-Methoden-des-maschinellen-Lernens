set.seed(123)
X_train = rnorm(1000, mean = 0, sd = 3)
y_train = 2 * X_train - 1 + rcauchy(1000)
X_test = rnorm(1000, mean = 0, sd = 3)
y_test = 2 * X_test - 1 + rnorm(1000)

plot(X_train, y_train)
plot(X_train, y_train, ylim = c(-25, 25))

lin_mod <- lm(y_train ~ X_train)

library(MASS)

?rlm

robust_mod <- rlm(y_train ~ X_train)


mean( (lin_mod$coefficients[1] + lin_mod$coefficients[2] * X_test - y_test)^2)

mean ( (robust_mod$coefficients[1] + robust_mod$coefficients[2] * X_test - y_test)^2)


set.seed(123)
idx <- sample(1:10000, 5000)
A_train <- A[idx,]
A_test <- A[-idx,]

lin_height_mod <- lm(Height ~ Weight, data = A_train)
robust_height_mod <- rlm(Height ~ Weight, data = A_train)

mean( (lin_height_mod$coefficients[1] + lin_height_mod$coefficients[2] * A_test$Weight - A_test$Height)^2)
mean ( (robust_height_mod$coefficients[1] + robust_height_mod$coefficients[2] * A_test$Weight - A_test$Height)^2)


