library(readr)
weight_height <- read_csv("dat/weight-height.csv")
weight_height$Weight <- weight_height$Weight / 2.2046
weight_height$Height <- weight_height$Height * 2.54
model_1 <-  lm(Height ~ Weight, weight_height)
summary(model_1)

res1 <- residuals(model_1)
pred1 <- predict(model_1, weight_height)

model_2 <-  lm(Height ~ Weight + Gender, weight_height)
summary(model_2)

weight_height$BMI <- weight_height$Height / (weight_height$Weight/100)^2

model_3 <- lm(Height ~ BMI, weight_height)
summary(model_3)

model_3 <- lm(Height ~ ., weight_height)

ggplot(mapping = aes(y = Height, x = BMI, col=Gender), data = weight_height) +
  geom_point()











