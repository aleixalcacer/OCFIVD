
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


