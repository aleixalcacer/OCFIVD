
require(tidyverse)
library(ordinalForest)

kiof <- function(train, labels, distance = "hausdorff") {
  #----------------
  Mdist <- interval_distance(train, train, distance)
  #------------- ---
  gamma <- 1
  K <- exp(-Mdist^2/gamma)
  #----------------
  et  <- labels
  db  <- data.frame(et=et, kk=K)
  ordforres <- ordfor(depvar="et", data=db)
  new_model <- c("model"=list(ordforres), "distance"=list(distance), "data"=list(train))
  class(new_model) <- "KIOF"
  return(new_model)
}


predict.KIOF <- function(model, test) {
  
  test_dist <- interval_distance(test, model$data, distance = model$distance)
  gamma <- 1
  test_ker <- exp(-test_dist^2/gamma)
  db <- data.frame( kk=test_ker)
  a  <- predict(model$model,db)
  return(a$ypred)
}


kiof_functional <- function(distances, labels) {
    #----------------
    Mdist <- distances # train vs train
    #------------- ---
    K <- 1 - Mdist
    #----------------
    et  <- labels
    db  <- data.frame(et=et, kk=K)
    ordforres <- ordfor(depvar="et", data=db)
    new_model <- c("model"=list(ordforres))
    class(new_model) <- "KIOF_F"
    return(new_model)
}


predict.KIOF_F <- function(model, distances) {
    
    test_dist <- distances  # test vs train
    test_ker <- 1 - test_dist
    db <- data.frame( kk=test_ker)
    a  <- predict(model$model,db)
    return(a$ypred)
}