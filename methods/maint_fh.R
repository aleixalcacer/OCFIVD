require("MAINT.Data")

maint_fh <- function(data, labels) {

    transformed_labels <- c()

        for (i in head(levels(labels), - 1)) {
        new_labels <- rep(0, length(labels))
        new_labels[labels > i] = 1
        new_labels <- factor(new_labels, levels = c(0, 1), ordered = T)
        transformed_labels <- c(transformed_labels, list(new_labels))
    }

    transformed_models <- c()
    for (lab in transformed_labels) {
        new_model <- MAINT.Data::lda(data, lab)
        transformed_models <- c(transformed_models, list(new_model))
    }

    new_model <- c("models" = list(transformed_models), "levels" = list(levels(labels)))
    class(new_model) <- "maint_fh"
    return(new_model)
}


predict.maint_fh <- function(model, data) {
    models <- model$models
    nlevels <- length(model$levels)
    
    projected <- array(0, dim = c(nrow(data), nlevels - 1))
    
    for (i in 2:nlevels) {
        projected[, i - 1] = predict(models[[i - 1]], data)$posterior[, 2]
    }
    
    preds = array(0, dim = c(nrow(data), nlevels))
    
    preds[, 1]  <- 1 - projected[, 1]
    
    if (nlevels > 2) {
    
    for (i in 2:(nlevels - 1)) {
        preds[, i] <- projected[, i - 1] * (1 - projected[, i])
    }
    }
    preds[, nlevels] <- projected[, nlevels - 1]
    
    d <- data.frame(preds)
    colnames(d) <- model$levels
    
    res <- factor(model$levels[max.col(d)], levels = model$levels)
    return(res)
}
