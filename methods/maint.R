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


maint_functional <- function(data, labels) {
    dd <- NULL
    for (day in data) {
        if (is.null(dd)) {
            dd <- day
        } else {
            dd <- cbind(dd, day)
        }
    }
    data <- dd

    new_model <- c("model" = list(MAINT.Data::lda(data, labels)))
    class(new_model) <- "MAINT_F"
    return(new_model)
}

predict.MAINT_F <- function(model, data) {
    dd <- NULL
    for (day in data) {
        if (is.null(dd)) {
            dd <- day
        } else {
            dd <- cbind(dd, day)
        }
    }
    data <- dd
    r <- predict(model$model, data)
    return(r$class)
}
