####################################################################
############## Simulation to assess noncollapsibility in models ####
# This simulation study intends to show that in linear models the 
# marginal and conditional effect of an exposure are the same, whereas
# those differ in a logistic regression model even if the additional
# covariate is independent from the exposure
# https://rstudio-pubs-static.s3.amazonaws.com/53883_67479cdb70e34402a357df0ff70f785e.html
####################################################################

library(magrittr)
library(reshape2)
library(ggplot2)
library(tidyverse)

n_sim <- 1600
n_obs <- c(150,300,1000)

#------------------------- Functions -------------------------------
biasEstimate <- function(theta, theta.Estimate, n = n_sim){
  return( 1/n * sum(theta.Estimate-theta) )
}
biasSE <- function(theta, theta.Estimate, n = n_sim){
  return( sqrt(1/(n*(n-1)) * sum( (theta.Estimate-theta)^2 )) )
}

#------------------ Simulation functions ----------------------------
Simulate.logit <- function(n_obs, OR.e = 0.5, OR.p = 10) {
  ## Outcome predictor
  p <- rbinom(n = n_obs, size = 1, prob = 0.5)
  ## Randomly assign exposure
  e <- rbinom(n = n_obs, size = 1, prob = 0.5)
  
  ################### Logistic regression model #########################
  # Assume an OR of OR.e for exposure e and OR of OR.p for predictor p, i.e.
  # Y = log(OR.e) * e + log(OR.p) * p
  #######################################################################
  ## Bernoulli response variable Y
  Y <- sapply(1/(1+exp(-(log(OR.e)*e + log(OR.p)*p))), rbinom, n = 1, size = 1 )
  
  ## Logistic regression: Conditional effect of exposure
  logit_conditional_coef_e <-
    glm(Y ~ e + p, family = binomial(link = "logit")) %>%
    coef %>% magrittr::extract("e")
  ## Logistic regression: Marginal effect of exposure (valid estimate as the exposure is randomized)
  logit_marginal_coef_e <-
    glm(Y ~ e, family = binomial(link = "logit")) %>%
    coef %>% magrittr::extract("e")
  
  ##
  return(
    c(logit_marginal       = logit_marginal_coef_e,
    logit_conditional    = logit_conditional_coef_e)
  )
}

Simulate.lin <- function(n_obs, beta.e = 0.5, beta.p = 10) {
  ## Outcome predictor
  p <- rbinom(n = n_obs, size = 1, prob = 0.5)
  ## Randomly assign exposure
  e <- rbinom(n = n_obs, size = 1, prob = 0.5)
  
  ################### Linear regression model #########################
  # Assume an effect beta.e for exposure e and an effect of beta.p for predictor p, i.e.
  # Y = beta.e * e + beta.p * p
  #######################################################################
  ## response variable Y
  Y <- beta.e * e + beta.p * p + rnorm(n = n_obs, mean = 0, sd = 1)
  
  ## Linear regression: Conditional effect of exposure
  lin_conditional_coef_e <-
    glm(Y ~ e + p, family = gaussian(link = "identity")) %>%
    coef %>% magrittr::extract("e")
  ## Linear regression: Marginal effect of exposure (valid estimate as the exposure is randomized)
  lin_marginal_coef_e <-
    glm(Y ~ e, family = gaussian(link = "identity")) %>%
    coef %>% magrittr::extract("e")
  
  ##
  c(lin_marginal         = lin_marginal_coef_e,
    lin_conditional      = lin_conditional_coef_e)
}

#------------------- Simulation --------------------
# Logistic regression
start.logistic <- Sys.time()
set.seed(72789)# 20150117
resSim <- data.frame(n.obs = rep(n_obs, each = n_sim),
                     OR.true = rep(0.5, n_sim*length(n_obs)),
                     marginal = rep(NA, n_sim*length(n_obs)),
                     conditional = rep(NA, n_sim*length(n_obs)))

for (row in 1:nrow(resSim)){
  resSim[row, c("marginal", "conditional")] <- Simulate.logit(
    n_obs = resSim[row, "n.obs"], 
    OR.e = resSim[row, "OR.true"]
    )
}

#----------------- Calculate bias ----------------------------
resSim %>%
  mutate(logOR.true = log(OR.true)) %>%
  group_by(n.obs, logOR.true) %>%
  summarise(Mean_marginal = mean(marginal),
            Mean_conditional = mean(conditional),
            Bias.marginal = biasEstimate(theta = marginal, 
                                         theta.Estimate = unique(logOR.true),
                                         n = unique(n.obs)),
            BiasSE.marginal = biasSE(theta = marginal, 
                                     theta.Estimate = unique(logOR.true), 
                                     n = unique(n.obs)),
            Bias.conditional = biasEstimate(theta = conditional, 
                                            theta.Estimate = unique(logOR.true),
                                            n = unique(n.obs)),
            BiasSE.conditional = biasSE(theta = conditional, 
                                        theta.Estimate = unique(logOR.true), 
                                        n = unique(n.obs)),
            .groups = "drop") ->
results

stop.logistic <- Sys.time()

resSim %>% 
  pivot_longer(marginal:conditional, names_to = "variable", values_to = "value") %>%
  ggplot(., aes(x = variable, y = value)) +
  geom_boxplot() +
  geom_hline(yintercept = log(0.5)) +
  theme_bw() + 
  ylab("Estimated log(OR)") +
  xlab("Type of effect") +
  facet_wrap(~ n.obs)

# Linear regression
start.linear <- Sys.time()
set.seed(72789) # 20150117
resSim.lin <- data.frame(n.obs = rep(n_obs, each = n_sim),
                     beta.true = rep(0.5, n_sim*length(n_obs)),
                     marginal = rep(NA, n_sim*length(n_obs)),
                     conditional = rep(NA, n_sim*length(n_obs)))

for (row in 1:nrow(resSim.lin)){
  resSim.lin[row, c("marginal", "conditional")] <- Simulate.lin(
    n_obs = resSim.lin[row, "n.obs"], 
    beta.e = resSim.lin[row, "beta.true"]
  )
}

#----------------- Calculate bias ----------------------------
resSim.lin %>%
  group_by(n.obs, beta.true) %>%
  summarise(Mean_marginal = mean(marginal),
            Mean_conditional = mean(conditional),
            Bias.marginal = biasEstimate(theta = marginal, 
                                         theta.Estimate = unique(beta.true),
                                         n = unique(n.obs)),
            BiasSE.marginal = biasSE(theta = marginal, 
                                     theta.Estimate = unique(beta.true), 
                                     n = unique(n.obs)),
            Bias.conditional = biasEstimate(theta = conditional, 
                                            theta.Estimate = unique(beta.true),
                                            n = unique(n.obs)),
            BiasSE.conditional = biasSE(theta = conditional, 
                                        theta.Estimate = unique(beta.true), 
                                        n = unique(n.obs)),
            .groups = "drop") ->
results.lin

stop.linear <- Sys.time()

resSim.lin %>% 
  pivot_longer(marginal:conditional, names_to = "variable", values_to = "value") %>%
  ggplot(., aes(x = variable, y = value)) +
  geom_boxplot() +
  geom_hline(yintercept = 0.5) +
  theme_bw() + 
  ylab("Estimated effect") +
  xlab("Type of effect") +
  facet_wrap(~ n.obs)
