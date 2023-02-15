require(tidyverse)
source("methods/interval-distance.R")

kknn <- function(data, labels) {
    new_model <- c("train" = list(data), "labels" = list(labels))
    class(new_model) <- "kknn"
    return(new_model)
}


predict.kknn <-  function (model, data, k = 7, kernel = "gaussian", distance = "euclidean_hausdorff") {
    train <- model$train
    train_labels <- model$labels
    test <- data
    
    weight.y <- function(l=1){
        k <- 1
        result <- matrix(0, l, l)
        diag(result) <- k
        for(i in 1:(k - 1)){
            for(j in 1:(l - i)){
                result[j, j+i] <- k - i
                result[j+i ,j] <- k - i
            }
        }
        result  
    }
    
    m <- dim(train)[1]
    p <- dim(test)[1]
    q <- dim(train)[2]
    
    kernel <- match.arg(kernel, c("rectangular", "triangular", "epanechnikov",
                                  "biweight", "triweight", "cos", "inv", 
                                  "gaussian"), FALSE)
    
    cl <- train_labels
    lev <- levels(cl)
    
    dist <- interval_distance(train, test, distance = distance)
    
    kdist <- as_tibble(dist) %>%
        rowid_to_column("a") %>%
        pivot_longer(-1, names_to = "b",
                     names_transform = list(b = as.integer),
                     values_to = "distance") %>%
        group_by(b) %>%
        slice_min(distance, n = k + 1) %>%
        inner_join(as_tibble(cl) %>% rowid_to_column("a"))
    
    D <- kdist %>%
        select(b, distance) %>%
        mutate(a = 1:(k+1)) %>%
        pivot_wider(names_from = a, values_from = distance) %>%
        column_to_rownames(var = "b")
    D <- as.matrix(D)
    
    C <- kdist %>%
        select(b, value) %>%
        mutate(a = 1:(k+1)) %>%
        pivot_wider(names_from = a, values_from = value) %>%
        column_to_rownames(var = "b")
    C <- as.matrix(C)
    
    maxdist <- D[, k + 1]
    maxdist[maxdist < 1.0e-6] <- 1.0e-6
    
    D <- D[, 1:k, drop=FALSE]
    CL <- C[, 1:k, drop=FALSE]
    
    l <- length(lev)
    weightClass <- matrix(0, p, l)
    
    W <- D / maxdist
    W <- pmin(W, 1 - 1e-6)
    W <- pmax(W ,1e-6)
    
    if (kernel=="inv") W <- 1/W
    if (kernel=="rectangular") W <- matrix(1,nrow = p, ncol = k)
    if (kernel=="triangular") W <- 1-W	 	
    if (kernel=="epanechnikov") W <- 0.75 * (1 - W^2)
    if (kernel=="biweight") W <- dbeta((W + 1) / 2, 3, 3)	 	
    if (kernel=="triweight") W <- dbeta((W + 1) / 2, 4, 4)	 	
    if (kernel=="cos") W <- cos(W * pi/2)
    if (kernel=="triweights") W <- 1
    if (kernel=="gaussian") {
        alpha <- 1 / (2 * (k + 1))
        qua <- abs(qnorm(alpha))
        W <- W * qua
        W <- dnorm(W, sd = 1)
    }
    
    W <- matrix(W, p, k)
    
    for (i in 1:l) {
        weightClass[, i] <- rowSums(W * (CL == lev[i]))	
    }
    weightClass <- weightClass/rowSums(weightClass)	
    colnames(weightClass) <- lev
    
    weightClass <- weightClass %*% weight.y(length(lev))
    weightClass <- weightClass / rowSums(weightClass)	
    weightClass <- t(apply(weightClass, 1, cumsum))
    colnames(weightClass) <- lev
    
    fit <- numeric(p)
    for (i in 1:p) fit[i] <- min((1:l)[weightClass[i, ] >= 0.5])
    fit <- ordered(fit, levels = 1:l, labels = lev)
    
    return(fit)
}



kknn_functional <- function(labels) {
    new_model <- c("labels" = list(labels))
    class(new_model) <- "KKNN_F"
    return(new_model)
}


predict.KKNN_F <-  function (model, distances, k = 7, kernel = "triangular") {
    train_labels <- model$labels
    test <- data
    
    weight.y <- function(l=1){
        k <- 1
        result <- matrix(0, l, l)
        diag(result) <- k
        for(i in 1:(k - 1)){
            for(j in 1:(l - i)){
                result[j, j+i] <- k - i
                result[j+i ,j] <- k - i
            }
        }
        result  
    }
    
    m <- dim(distances)[1]
    p <- dim(distances)[2]

    kernel <- match.arg(kernel, c("rectangular", "triangular", "epanechnikov",
                                  "biweight", "triweight", "cos", "inv", 
                                  "gaussian"), FALSE)
    
    cl <- train_labels
    lev <- levels(cl)
    
    dist <- distances  # train vs test
    
    kdist <- as_tibble(dist) %>%
        rowid_to_column("a") %>%
        pivot_longer(-1, names_to = "b",
                     names_transform = list(b = as.integer),
                     values_to = "distance") %>%
        group_by(b) %>%
        slice_min(distance, n = k + 1) %>%
        inner_join(as_tibble(cl) %>% rowid_to_column("a"))
    
    D <- kdist %>%
        select(b, distance) %>%
        mutate(a = 1:(k+1)) %>%
        pivot_wider(names_from = a, values_from = distance) %>%
        column_to_rownames(var = "b")
    D <- as.matrix(D)
    
    C <- kdist %>%
        select(b, value) %>%
        mutate(a = 1:(k+1)) %>%
        pivot_wider(names_from = a, values_from = value) %>%
        column_to_rownames(var = "b")
    C <- as.matrix(C)
    
    maxdist <- D[, k + 1]
    maxdist[maxdist < 1.0e-6] <- 1.0e-6
    
    D <- D[, 1:k, drop=FALSE]
    CL <- C[, 1:k, drop=FALSE]
    
    l <- length(lev)
    weightClass <- matrix(0, p, l)
    
    W <- D / maxdist
    W <- pmin(W, 1 - 1e-6)
    W <- pmax(W ,1e-6)
    
    if (kernel=="inv") W <- 1/W
    if (kernel=="rectangular") W <- matrix(1,nrow = p, ncol = k)
    if (kernel=="triangular") W <- 1-W	 	
    if (kernel=="epanechnikov") W <- 0.75 * (1 - W^2)
    if (kernel=="biweight") W <- dbeta((W + 1) / 2, 3, 3)	 	
    if (kernel=="triweight") W <- dbeta((W + 1) / 2, 4, 4)	 	
    if (kernel=="cos") W <- cos(W * pi/2)
    if (kernel=="triweights") W <- 1
    if (kernel=="gaussian") {
        alpha <- 1 / (2 * (k + 1))
        qua <- abs(qnorm(alpha))
        W <- W * qua
        W <- dnorm(W, sd = 1)
    }
    
    W <- matrix(W, p, k)
    
    for (i in 1:l) {
        weightClass[, i] <- rowSums(W * (CL == lev[i]))	
    }
    weightClass <- weightClass/rowSums(weightClass)	
    colnames(weightClass) <- lev
    
    weightClass <- weightClass %*% weight.y(length(lev))
    weightClass <- weightClass / rowSums(weightClass)	
    weightClass <- t(apply(weightClass, 1, cumsum))
    colnames(weightClass) <- lev
    
    fit <- numeric(p)
    for (i in 1:p) fit[i] <- min((1:l)[weightClass[i, ] >= 0.5])
    fit <- ordered(fit, levels = 1:l, labels = lev)
    
    return(fit)
}