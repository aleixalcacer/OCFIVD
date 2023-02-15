
require(tidyverse)
library(stats)
library(ordinalForest)

of <- function(train, labels) {
  # Ordinal forest ------
  et  <- labels
  db  <- data.frame(et=et, kk=train@MidP)
  ordforres <- ordfor(depvar="et", data=db)
  # New model class -----
  new_model <- c("model"=list(ordforres))
  class(new_model) <- "OF"
  return(new_model)
}

predict.OF <- function(model, test) {
  db <- data.frame(kk=test@MidP)
  a  <- predict(model$model, db)
  return(a$ypred)
}


of_functional <- function(data, labels) {
    # Ordinal forest ------
    et  <- labels
    
    dd <- NULL
    
    for (day in data) {
        if (is.null(dd)) {
            dd <- day
        } else {
            dd <- cbind(dd, day)
        }
    }
    data <- dd
    data <- data@MidP
    
    db  <- data.frame(et=et, kk=data)
    ordforres <- ordfor(depvar="et", data=db)
    # New model class -----
    new_model <- c("model"=list(ordforres))
    class(new_model) <- "OF_F"
    return(new_model)
}

predict.OF_F <- function(model, data) {
    dd <- NULL
    for (day in data) {
        if (is.null(dd)) {
            dd <- day
        } else {
            dd <- cbind(dd, day)
        }
    }
    data <- dd
    data <- data@MidP
    db <- data.frame(kk=data)
    a  <- predict(model$model, db)
    return(a$ypred)
}

