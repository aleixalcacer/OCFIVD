
polr <- function(data, labels) {
    data <- data@MidP
    
    df <- as_tibble(data) %>% mutate(y = labels)
    
    model <- MASS::polr(y ~ I1.MidP + I2.MidP, df)
    new_model <- c("model" = list(model))
    class(new_model) <- "polr_"
    return(new_model)
}

predict.polr_ <- function(model, data) {
    return(predict(model$model, data@MidP))
}
