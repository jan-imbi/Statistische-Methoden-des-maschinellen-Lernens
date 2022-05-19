library(tidyverse)
## Parameter
n_obs = c(10,50,100,200,300,400,500,600,700,800,900,1000,10000)
n_sim = 1900
set.seed(24062021)
seed <- sample(1:10000000, replace = FALSE, size = n_sim)
sig_result <- matrix(NA, nrow = n_sim, ncol = length(n_obs),
                     dimnames = list(NULL, paste0("n_obs_",n_obs)))
rejection <- data.frame(n_obs            = factor(n_obs),
                        Rejection_Est    = NA,
                        Rejection_Est_SE = NA)
## Simulation
for (i in 1:length(n_obs)) {
  for (j in 1:n_sim) {
    set.seed(seed[j])
    ## Data Generation
    A <- rnorm(n = n_obs[i]/2, mean = 5, sd = 2)
    P <- rnorm(n = n_obs[i]/2, mean = 5, sd = 2)
    ## Testen:
    sig_result[j,i] <- t.test(x=A, y=P, alternative = "two.sided")$p.value <= 0.05
  }
  ## Ergebnisaufbereitung
  rejection$Rejection_Est[i]    <- sum(sig_result[,i])/n_sim
  rejection$Rejection_Est_SE[i] <- sqrt( (rejection$Rejection_Est[i] * (1-rejection$Rejection_Est[i]) ) / n_sim)
}
rejection

rejection %>%
  ggplot(aes(x=n_obs, y=Rejection_Est, group=1)) +
  geom_point() + geom_line() + geom_hline(yintercept = 0.05) + lims(y=c(0,0.1))

rejection %>%
  ggplot(aes(x=n_obs, y=Rejection_Est_SE, group=1)) +
  geom_point() + geom_line() + lims(y=c(0,0.01))

