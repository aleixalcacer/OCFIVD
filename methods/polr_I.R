#A) ficar al polr els punts extrems d'intervals, en lloc del punt mig
#Tots junts
#B) fer 2 polr, un amb els extrems inferiors i altre amb els extrems superiors
# dels intervals, i combinar les 2 probabilitats x obtindre la pertinença a cada classe
#The posterior probabilities of the classes for both models are195
#averaged to obtain the final posterior probabilities of the classes.


polr_I <- function(data, labels) {
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
  
  model <- MASS::polr(y ~ inf_1 + inf_2, df)
  new_model <- c("model" = list(model))
  class(new_model) <- "polr_I"
  return(new_model)
}
# aci no se com es fa? igual?
predict.polr_I <- function(model, data) {
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
  
  return(predict(model$model, data2[c("inf_1","inf_2")]))
}
