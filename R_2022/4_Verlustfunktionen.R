set.seed(123)
X_train = rnorm(1000, mean = 0, sd = 3)
y_train = 2 * X_train - 1 + rcauchy(1000)
X_test = rnorm(1000, mean = 0, sd = 3)
y_test = 2 * X_test - 1 + rnorm(1000)

plot(X_train, y_train)

plot(X_train, y_train, ylim = c(-25, 25))

lin_mod <- lm(y_train ~ X_train)
library(MASS)
rob_mod <- rlm(y_train ~ X_train)
pred_lin <- predict(lin_mod, data.frame("(Intercept)" = 1, X_test = X_test))
pred_rob <- predict(rob_mod, data.frame(X_test = X_test))
pred2_lin <- lin_mod$coefficients[1] + lin_mod$coefficients[2] * X_test
pred2_rob <- rob_mod$coefficients[1] + rob_mod$coefficients[2] * X_test

mean((pred2_lin - y_test)^2)
mean((pred2_rob - y_test)^2)



xres_lin <- mean((y_test - pred_lin)^2)
res_rob <- mean((y_test - pred_rob)^2)
res_lin
res_rob








