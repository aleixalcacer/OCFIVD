require(tidyverse)
require(kernlab)

kpcao <- function(train, labels, distance = "ichino_yaguchi") {
    
    dist <- interval_distance(train, train, distance)
    
    ker <- kernlab::as.kernelMatrix(1 - dist)
    
    pca <- kernlab::kpca(ker)
    
    pca_rotated <- pca@rotated
    polr_df <- as_tibble(pca_rotated) %>% mutate(y = labels)
    
    model <- MASS::polr(y ~ V1 + V2 + V3 + V4 + V5 + V6, data=polr_df)
    new_model <- c("model"=list(model), "pca"=list(pca), "distance"=list(distance), "data"=list(train))
    
    class(new_model) <- "kpcao"
    return(new_model)
}


predict.kpcao <- function(model, test) {
    test_dist <- interval_distance(test, model$data, distance = model$distance)
    test_ker <- kernlab::as.kernelMatrix(1 - test_dist)
    test_pca_rotated <- predict(model$pca, test_ker)
    test_polr_df <- as.tibble(test_pca_rotated)
    return(predict(model$model, test_polr_df))
}
