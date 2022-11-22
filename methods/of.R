require(ordinalForest)

of <- function(data, labels) {
    data <- data@MidP
    
    df <- as_tibble(data) %>% mutate(y = labels)
    
    model <- ordinalForest::ordfor("y", df)
    new_model <- c("model" = list(model))
    class(new_model) <- "of_"
    return(new_model)
}

predict.of_ <- function(model, data) {
    return(predict(model$model, data@MidP))
}
