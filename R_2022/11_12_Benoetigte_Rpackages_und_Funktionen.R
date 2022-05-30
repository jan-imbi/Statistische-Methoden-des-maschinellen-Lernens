## Installation der benoetigten Pakete (nur einmal ausfuehren!!) 
install.packages("tidyverse")
install.packages("tree")
install.packages("parsnip")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("remotes")
remotes::install_github("grantmcdermott/parttree") 
# wenn Sie nach Updates gefragt werden, waehlen Sie "1" und druecken Sie dann Enter, um alles zu aktualisieren
install.packages("magrittr")
install.packages("reshape2")

## Pakete in aktuelle Session einbinden (immer am Anfang des Programmes ausfuehren)
library(tidyverse)
library(tree)
library(parsnip)
library(parttree)
library(rpart)
library(rpart.plot)
library(magrittr)
library(reshape2)

## Funktionen zur Datengenerierung:
linear_data <- function(mu=20,sigma=15,n=1000,cx=1,cy=0.1,intercept=0,xmin=0,xmax=300){
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
