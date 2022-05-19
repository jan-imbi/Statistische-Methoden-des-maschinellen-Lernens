library(MASS)
library(dplyr)

# Aufgabe 1
dat <- read.csv2("dat/heart.csv", sep=",") %>%
  mutate(Herzkrank = factor(ifelse(Herzkrank == 0, "gesund", "krank")),
         Geschlecht = factor(ifelse(Geschlecht == 0, "female", "male")),
         Brustschmerz = factor(Brustschmerz),
         Blutzucker = factor(ifelse(Blutzucker == 1, "erhoeht", "normal")),
         EKG = factor(EKG),
         Angina = factor(ifelse(Angina == 1, "ja", "nein")),
         Depression1 = as.numeric(Depression1),
         Depression2 = factor(Depression2)) %>%
  dplyr::select(-Thal)
str(dat)
regfit_full <- leaps::regsubsets(Herzkrank ~ ., 
                                 data = dat, 
                                 nvmax = 16)
results_bestsub <- summary(regfit_full)
results_bestsub$outmat
plot(regfit_full, scale = "adjr")
plot(regfit_full, scale = "adjr")
which.max(results_bestsub$adjr2)
plot(results_bestsub$adjr2,
     main = "Adjusted R^2 over number of variables",
     xlab = "Number of Variables",
     ylab = "Adjusted RSq",
     type = "b")
points(11, results_bestsub$adjr2[11], col="red", cex=2, pch=20)
plot(results_bestsub$adjr2,
     main = "Adjusted R^2 over number of variables",
     xlab = "Number of Variables",
     ylab = "Adjusted RSq",
     type = "b")
points(11, results_bestsub$adjr2[11], col="red", cex=2, pch=20)
results_bestsub$which[11,]
model_best_subset <- glm(Herzkrank ~ Geschlecht + Brustschmerz + 
                           Blutdruck + Cholesterin + Herzschlag +
                           Angina + Depression1 + Depression2 +
                           Gefaesse,
                         data = dat,
                         family = "binomial")
summary(model_best_subset)


# Aufgabe 2

# define null model 
model_null <- glm(Herzkrank ~ 1, data = dat, family = "binomial")

# perform forward selection
results_FWS <- stepAIC(
  model_null, 
  direction = "forward",
  scope = list(lower = ~ 1,
               upper = Herzkrank ~ Alter + Geschlecht + 
                 Brustschmerz + Blutdruck + Cholesterin + 
                 Blutzucker + EKG + Herzschlag + Angina + 
                 Depression1 + Depression2 + Gefaesse))

# view results
summary(results_FWS)


# define full model
model_full <- glm(Herzkrank ~ ., data = dat, family = "binomial")

# perform backward selection
results_BWS <- stepAIC(model_full, direction = "backward")

# view results
summary(results_BWS)


# perform stepwise selection
results_both <- stepAIC(model_full, direction = "both", trace = TRUE)

# view results
summary(results_both)

# generate final model including selected variables
model_final <- glm(Herzkrank ~ Geschlecht + Brustschmerz + 
                     Blutdruck + Cholesterin + Herzschlag +
                     Angina + Depression1 + Depression2 + Gefaesse,
                   data = dat,
                   family = "binomial")

# 5-fache Kreuzvalidierung des finalen Modells
set.seed(20042021)
glm_best <- caret::train(Herzkrank ~ Geschlecht + Brustschmerz + Blutdruck + 
                           Cholesterin + Herzschlag + Angina + Depression1 + 
                           Depression2 + Gefaesse, 
                         data = dat,
                         method = "glm",
                         trControl = ctrl,
                         metric = "ROC")

roc_best <- pROC::roc(response = glm_best$pred$obs, # observations
                      predictor = glm_best$pred$krank, # predictions
                      ci = TRUE,        # compute confidence interval
                      plot = TRUE,      # create ROC plot
                      print.auc = TRUE,
                      main = "ROC curve of the final logistic regression model")


# 5-fold cross-validation of the full model
glm_full <- caret::train(Herzkrank ~ ., data = dat,
                         method = "glm",
                         trControl = ctrl,
                         metric = "ROC")

roc_full <- pROC::roc(response = glm_full$pred$obs, 
                      predictor = glm_full$pred$krank, 
                      ci = TRUE,
                      plot = TRUE,
                      add = TRUE,       # add ROC curve to existing plot
                      col = "darkgreen",
                      print.auc.adj=c(0,5), # adjust position of AUC
                      print.auc = TRUE,
                      main = "ROC curve of the final(black) and full (green) model")








