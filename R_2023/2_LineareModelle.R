library(readr)
weight_height <- read_csv("dat/weight-height.csv")
print(weight_height)

weight_height$Weight <- weight_height$Weight / 2.2046
weight_height$Height <- weight_height$Height * 2.54
View(weight_height)

?lm


model_1 <- lm(Height ~ Weight, data = weight_height)
summary(model_1)

res <- residuals(model_1)
pred <- predict(model_1)

sum(residuals(model_1)^2)


plot(pred, res)


model_2 <- lm(Height ~ Weight + Gender, data = weight_height)
model_2

sum(residuals(model_2)^2)


weight_height$BMI <- weight_height$Weight / (weight_height$Height / 100)^2
weight_height$BMI

model_3 <- lm(Height ~ BMI, data = weight_height)
summary(model_3)

model_4 <- lm(Height ~ BMI + Gender, data = weight_height)
summary(model_4)

pred <- predict(model_4)
res <- residuals(model_4)
plot(pred, res)

library(ggplot2)
ggplot(mapping = aes(y = res, x = pred, col=weight_height$Gender)) +
  geom_point()






