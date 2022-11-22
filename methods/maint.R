require("MAINT.Data")

maint <- function(data, labels) {
    new_model <- c("model" = list(MAINT.Data::lda(data, labels)))
    class(new_model) <- "maint_"
    return(new_model)
}

predict.maint_ <- function(model, data) {
    r <- predict(model$model, data)
    return(r$class)
}
