library(dplyr)
library(rpart)
library(rpart.plot)
library(readr)
A <- read_csv("dat/weight-height.csv")
A$Weight <- A$Weight / 2.2046
A$Height <- A$Height * 2.54
A$Gender <- factor(A$Gender)

?rpart

modl <- rpart(Gender ~ Weight + Height, data = A)
plot(modl)
rpart.plot(modl)


table(A$Gender, predict(modl, type = "class"), deparse.level = 2)

# Wir ihr seht, wird mit den Basiseinstellungen "Weight" garnicht benutzt.
# Durch umstellen der rpart.control parameter, können mehr splits erzwungen
# werden.
rpart.plot(rpart(Gender ~ Weight + Height, data = A,
                 control = rpart.control(cp=0, minsplit = 500)))
rpart.plot(rpart(Gender ~ Weight + Height, data = A,
                 control = rpart.control(cp=0, maxdepth = 4)))

