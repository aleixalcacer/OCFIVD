
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

polr_functional <- function(data, labels) {
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
    
    df <- as.data.frame(data) %>% mutate(y = labels)
    fma <- as.formula(paste("y ~ ", paste(head(colnames(df), -1), collapse= "+")))

    model <- MASS::polr(fma, df)
    new_model <- c("model" = list(model))
    class(new_model) <- "POLR_F"
    return(new_model)
}

predict.POLR_F <- function(model, data) {
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
    
    return(predict(model$model, data))
}
