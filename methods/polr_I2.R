#A) ficar al polr els punts extrems d'intervals, en lloc del punt mig
#Tots junts
#B) fer 2 polr, un amb els extrems inferiors i altre amb els extrems superiors
# dels intervals, i combinar les 2 probabilitats x obtindre la pertinença a cada classe
#The posterior probabilities of the classes for both models are195
#averaged to obtain the final posterior probabilities of the classes.

library(nnet)

polr_I2 <- function(data, labels) {
  df_1 <- data@MidP
  PM_I1 <- df_1$I1.MidP
  PM_I2 <- df_1$I2.MidP
  df_2 <- data@LogR
  D_I1 <- df_2$I1.LogR
  D_I2 <- df_2$I2.LogR
  # interval inferior
  inf_1 <- PM_I1 - exp(D_I1)/2
  inf_2 <- PM_I2 - exp(D_I2)/2
  
  sup_1 <- PM_I1 + exp(D_I1)/2
  sup_2 <- PM_I2 + exp(D_I2)/2
  
  data2 <- data.frame(inf_1=inf_1,inf_2=inf_2, sup_1=sup_1,sup_2=sup_2 )
  
  df <- as_tibble(data2) %>% mutate(y = labels)
  
  model_1 <- MASS::polr(y ~ inf_1 + inf_2, df)
  model_2 <- MASS::polr(y ~ sup_1 + sup_2, df)
  
  new_model <- c("model_1" = list(model_1), "model_2" = list(model_2), levels = list(levels(labels)))
  class(new_model) <- "polr_I2"
  return(new_model)
}
# aci no se com es fa? igual?
predict.polr_I2 <- function(model, data) {
  
  df_1 <- data@MidP
  PM_I1 <- df_1$I1.MidP
  PM_I2 <- df_1$I2.MidP
  df_2 <- data@LogR
  D_I1 <- df_2$I1.LogR
  D_I2 <- df_2$I2.LogR
  
  # interval inferior
  inf_1 <- PM_I1 - exp(D_I1)/2
  inf_2 <- PM_I2 - exp(D_I2)/2
  
  sup_1 <- PM_I1 + exp(D_I1)/2
  sup_2 <- PM_I2 + exp(D_I2)/2
  
  data2 <- data.frame(inf_1=inf_1,inf_2=inf_2, sup_1=sup_1,sup_2=sup_2 )
 
  df <- as_tibble(data2)
  
  # mean of both models
  M1 <- predict(model$model_1, data2[c("inf_1","inf_2")],type="probs")
  M2 <- predict(model$model_2, data2[c("sup_1","sup_2")],type="probs")
  M3=(M2+M1)/2

  class <- factor(apply(M3, 1, which.max), ordered = T, levels = model$levels)
  return(class)
}
