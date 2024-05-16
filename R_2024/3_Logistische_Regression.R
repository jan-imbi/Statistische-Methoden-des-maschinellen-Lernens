github.com/jan-imbi/statistische-methoden-des-maschinellen-lernens
library(readr)
A <- read_csv("dat/weight-height.csv")
A$Weight <- A$Weight / 2.2046
A$Height <- A$Height * 2.54
A$Gender <- factor(A$Gender)

?glm

fit <- glm(Gender ~ Height, data = A,  family = binomial())

?predict

predict(fit)

inv_log <- function(eta) 1/(1+exp(-eta))
inv_log(predict(fit))

predict(fit, type="response")


ifelse(predict(fit, type="response") > 0.5, "Male", "Female")
A$Gender

table(ifelse(predict(fit, type="response") > 0.5, "Male", "Female"), A$Gender, deparse.level = 2)


fit2 <- glm(Gender ~ Height + Weight, data = A,  family = binomial())
fit2
table(ifelse(predict(fit2, type="response") > 0.5, "Male", "Female"), A$Gender, deparse.level = 2)

anova(fit, fit2, test = "Chisq")
