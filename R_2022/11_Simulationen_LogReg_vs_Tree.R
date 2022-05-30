################################################################################
#           Simulation: logistic Regression vs Tree                            #
################################################################################

#-------------------------- Gebrauchte Packages --------------------------------
library(tidyverse)
library(ggplot2)
library(tree)
library(rpart)
library(parsnip)
library(parttree)
library(rpart.plot)

#------------------- Funktionen für Daten Generation ----------------------------
linear_data <- function(mu = 20, sigma = 15, n = 1000, cx = 1, cy = 0.1, 
                        intercept = 0, xmin = 0, xmax = 300){
  #######################################################
  #    Two classes separated by a straight line.
  #######################################################
  # normal vector
  nx = -cy
  ny = cx
  normal = c(nx,ny)
  
  data <- data.frame(intercept = rep(intercept, 2*n),
                     slope = rep(cy/cx, 2*n),
                     group = rep(NA, 2*n),
                     X = rep(NA, 2*n),
                     Y = rep(NA, 2*n))
  
  for (i in 1:n){
    # Yes point
    data[i, "group"] <- "yes"
    # normally distributed displacments in normal direction
    yes_dists <- rnorm(n = 1, mean = mu, sd = sigma)
    
    # sample points along boundary and add displacements
    yes_point <- runif(n = 1, min = xmin, max = xmax)*c(cx,cy) + yes_dists*normal
    data[i, "X"] <- yes_point[1]
    data[i, "Y"] <- yes_point[2]
    
    # No point
    data[(n + i), "group"] <- "no"
    # normally distributed displacments in normal direction
    no_dists <- rnorm(n = 1, mean = -mu, sd = sigma)
    
    # sample points along boundary and add displacements
    no_point <- runif(n = 1, min = xmin, max = xmax)*c(cx,cy) + no_dists*normal
    data[(n + i), "X"] <- no_point[1]
    data[(n + i), "Y"] <- no_point[2]
  }
  
  data$group <- factor(data$group)
  
  return(data)
}

radial_data <- function(mu0 = 10, sigma0 = 5, mu1 = 40, sigma1 = 5, n = 1000){
  ###################################################################
  # Two classes arranged in concentric semi-circular segments
  ###################################################################
  data <- data.frame(group = rep(NA, 2*n),
                     X = rep(NA, 2*n),
                     Y = rep(NA, 2*n))
  
  for (i in 1:n){
    #Yes group
    data[i, "group"] <- "yes"
    # Sample radius and angle
    r0 <- rnorm(n = 1, mean = mu0, sd = sigma0)
    theta0 <- runif(n = 1, min = 0, max = pi)
    p0 <- r0 * c(cos(theta0),sin(theta0))
    
    data[i, "X"] <- p0[1]
    data[i, "Y"] <- p0[2]
    
    #No group
    data[(n + i), "group"] <- "no"
    # Sample radius and angle
    r1 <- rnorm(n = 1, mean = mu1, sd = sigma1)
    theta1 <- runif(n = 1, min = 0, max = pi)
    p1 <- r1 * c(cos(theta1),sin(theta1))
    
    data[(n + i), "X"] <- p1[1]
    data[(n + i), "Y"] <- p1[2]
  }
  
  data$group <- factor(data$group)
  
  return(data)
}

#---------------------------- Einzelauswertung -----------------------------------
#### Linear data
set.seed(01072021)
df.lin <- linear_data()

#Calculate logistic regression on the data with X and Y as predictors
lin.log <- glm(group ~ X + Y, family = binomial, data = df.lin)
# Derive decision boundary from this model, i.e. line where predicted probability
# is 0.5 or equivalently where logit(p)=0.
bound.intercept <- - coef(lin.log)["(Intercept)"]/coef(lin.log)["Y"]
bound.slope <- - coef(lin.log)["X"]/coef(lin.log)["Y"]

#Calculate tree model
## Build our tree using parsnip (but with rpart as the model engine)
lin_tree <- rpart(
  group ~ X + Y, 
  data = df.lin,
  method = "class", 
  parms=list(split="gini"))# use "information" for deviance
# The default splitting method for classification is “gini”.
rpart.plot(lin_tree)

## Plot the data and model partitions
df.lin %>%
  ggplot(aes(x=Y, y=X, col = group)) +
  geom_point() +
  geom_parttree(data = lin_tree, aes(fill=group), alpha = 0.1) +
  theme_minimal() +
  geom_abline(intercept = -bound.intercept/bound.slope, slope = 1/bound.slope)


#### Radial data
df.rad <- radial_data()

#Calculate logistic regression on the data with X and Y as predictors
rad.log <- glm(group ~ X + Y, family = binomial, data = df.rad)
# Derive decision boundary from this model, i.e. line where predicted probability
# is 0.5 or equvalently where logit(p)=0.
bound.intercept <- - coef(rad.log)["(Intercept)"]/coef(rad.log)["Y"]
bound.slope <- - coef(rad.log)["X"]/coef(rad.log)["Y"]

#Calculate tree model
## Build our tree using parsnip (but with rpart as the model engine)
rad_tree <- rpart(
  group ~ X + Y,
  data = df.rad,
  method = "class", 
  parms=list(split="gini"))# use "information" for deviance
# The default splitting method for classification is “gini”.
rpart.plot(rad_tree)

## Plot the data and model partitions
df.rad %>%
  ggplot(aes(x=Y, y=X, col = group)) +
  geom_point() +
  geom_parttree(data = rad_tree, aes(fill=group), alpha = 0.1) +
  theme_minimal() +
  geom_abline(intercept = -bound.intercept/bound.slope, slope = 1/bound.slope)


#-------------------------------------------------------------------------------
#----------------------------- Simulations -------------------------------------
#-------------------------------------------------------------------------------

#---------------------------- Test data set ------------------------------------
set.seed(123456)
df_lin_test <- linear_data(n = 150)
df_rad_test <- radial_data(n = 150)
#------------------------- Parameter definitions -------------------------------
n_sim <- 100
n_obs <- c(100, 150, 200, 300, 500)

para <- data.frame(n_sim        = rep(n_sim, length(n_obs)), 
                   n_obs        = n_obs)

#------------------------- Starting simulations --------------------------------
start.time.1 <- Sys.time()

# Setting outcome variables
Avg_PredError_logReg_LinData_Train <- c()
Avg_PredError_logReg_LinData_Test  <- c()
Avg_PredError_Tree_LinData_Train   <- c()
Avg_PredError_Tree_LinData_Test    <- c()

Avg_PredError_logReg_RadData_Train <- c()
Avg_PredError_logReg_RadData_Test  <- c()
Avg_PredError_Tree_RadData_Train   <- c()
Avg_PredError_Tree_RadData_Test    <- c()

# Setting seed
set.seed(01072021)

# Starting loop
for (row in 1:dim(para)[1]) {
  PredError_logReg_LinData_Train <- rep(NA, para$n_sim[row])
  PredError_logReg_LinData_Test  <- rep(NA, para$n_sim[row])
  PredError_Tree_LinData_Train   <- rep(NA, para$n_sim[row])
  PredError_Tree_LinData_Test    <- rep(NA, para$n_sim[row])
  
  PredError_logReg_RadData_Train <- rep(NA, para$n_sim[row])
  PredError_logReg_RadData_Test  <- rep(NA, para$n_sim[row])
  PredError_Tree_RadData_Train   <- rep(NA, para$n_sim[row])
  PredError_Tree_RadData_Test    <- rep(NA, para$n_sim[row])
  
  for (i in 1:para$n_sim[row]) {
    ### Data generation
    df.lin <- linear_data(n = para$n_obs[row]/2)
    df.rad <- radial_data(n = para$n_obs[row]/2)
    
    ### Analysis of radial data
    ## logistic regression
    rad.glm <- glm(group ~ Y + X, data=df.rad, family="binomial")
    # Prediction errors
    PredError_logReg_RadData_Train[[i]] <- 1 - sum(ifelse(predict(rad.glm, type = "response")>0.5, "yes", "no")==as.character(df.rad$group))/para$n_obs[row]
    PredError_logReg_RadData_Test[[i]]  <- 1 - sum(ifelse(predict(rad.glm, newdata = df_rad_test, type = "response")>0.5, "yes", "no")==as.character(df_rad_test$group))/dim(df_rad_test)[1]
    
    ## tree 
    rad.tree <- rpart(
      group ~ Y + X,
      data = df.rad,
      method = "class", 
      parms=list(split="gini"))
    # Prediction errors
    PredError_Tree_RadData_Train[[i]] <- 1 - sum(as.character(predict(rad.tree, data = df.rad, type = "class"))==as.character(df.rad$group))/para$n_obs[row]
    PredError_Tree_RadData_Test[[i]]  <- 1 - sum(as.character(predict(rad.tree, newdata = df_rad_test, type = "class"))==as.character(df_rad_test$group))/dim(df_rad_test)[1]
    
    ### Analysis of linear data
    ## logistic regression
    lin.glm <- glm(group ~ Y + X, data=df.lin, family="binomial")
    # Prediction errors
    PredError_logReg_LinData_Train[[i]] <- 1 - sum(ifelse(predict(lin.glm, type = "response")>0.5, "yes", "no")==as.character(df.lin$group))/para$n_obs[row]
    PredError_logReg_LinData_Test[[i]]  <- 1 - sum(ifelse(predict(lin.glm, newdata = df_lin_test, type = "response")>0.5, "yes", "no")==as.character(df_lin_test$group))/dim(df_lin_test)[1]
      
    ## tree 
    lin.tree <- rpart(
      group ~ Y + X,
      data = df.lin,
      method = "class", 
      parms=list(split="gini"))
    # Prediction errors
    PredError_Tree_LinData_Train[[i]] <- 1 - sum(as.character(predict(lin.tree, data = df.lin, type = "class"))==as.character(df.lin$group))/para$n_obs[row]
    PredError_Tree_LinData_Test[[i]]  <- 1 - sum(as.character(predict(lin.tree, newdata = df_lin_test, type = "class"))==as.character(df_lin_test$group))/dim(df_lin_test)[1]
  }  
  
  Avg_PredError_logReg_LinData_Train <- c(Avg_PredError_logReg_LinData_Train, 
                                          mean(PredError_logReg_LinData_Train))
  Avg_PredError_logReg_LinData_Test  <- c(Avg_PredError_logReg_LinData_Test,
                                          mean(PredError_logReg_LinData_Test))
  Avg_PredError_Tree_LinData_Train   <- c(Avg_PredError_Tree_LinData_Train,
                                          mean(PredError_Tree_LinData_Train))
  Avg_PredError_Tree_LinData_Test    <- c(Avg_PredError_Tree_LinData_Test, 
                                          mean(PredError_Tree_LinData_Test))
  
  Avg_PredError_logReg_RadData_Train <- c(Avg_PredError_logReg_RadData_Train,
                                          mean(PredError_logReg_RadData_Train))
  Avg_PredError_logReg_RadData_Test  <- c(Avg_PredError_logReg_RadData_Test, 
                                          mean(PredError_logReg_RadData_Test))
  Avg_PredError_Tree_RadData_Train   <- c(Avg_PredError_Tree_RadData_Train, 
                                          mean(PredError_Tree_RadData_Train))
  Avg_PredError_Tree_RadData_Test    <- c(Avg_PredError_Tree_RadData_Test, 
                                          mean(PredError_Tree_RadData_Test))
}

# Time measurement
(time.1 <- Sys.time() - start.time.1)

# Plotting results (TODO)
df.plot <- data.frame(
  N_obs     = n_obs,
  Datatype  = rep(c("Rad", "Lin"), each = 4*length(n_obs)),
  Model     = rep(rep(c("logReg", "Tree"), each = 2*length(n_obs)),2),
  TrainTest = rep(rep(c("Train", "Test"), each = length(n_obs)),2*4),
  PredError = c(Avg_PredError_logReg_RadData_Train, 
                Avg_PredError_logReg_RadData_Test, 
                Avg_PredError_Tree_RadData_Train, 
                Avg_PredError_Tree_RadData_Test,
                Avg_PredError_logReg_LinData_Train, 
                Avg_PredError_logReg_LinData_Test, 
                Avg_PredError_Tree_LinData_Train, 
                Avg_PredError_Tree_LinData_Test)
  )

df.plot %>%
  ggplot(aes(x=N_obs, y=PredError, color=Model)) +
  geom_point() +
  geom_line() +
  facet_grid(Datatype~TrainTest) +
  theme_bw() +
  theme(legend.position = "bottom")
