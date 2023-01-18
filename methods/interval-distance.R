# Reference: https://link.springer.com/content/pdf/10.1007/978-3-642-22194-1_76.pdf

interval_distance <- function(A, B, distance="hausdorff") {
    hausdorff <- function(i, j, p, AL, AU, BL, BU) {
        d <- 0
        for (k in 1:p) {
            d <- d + max(abs(AL[i, k] - BL[j, k]), abs(AU[i, k] - BU[j, k]))
        }
        d
    }
    
    euclidean_hausdorff <- function(i, j, p, AL, AU, BL, BU) {
        d <- 0
        for (k in 1:p) {
            d <- d + max(abs(AL[i, k] - BL[j, k]), abs(AU[i, k] - BU[j, k]))^2
        }
        d <- sqrt(d)
        d
    }
    
    # normalized_euclidean_hausdorff
    
    gowda_diday <- function(i, j, p, AL, AU, BL, BU) {
        d <- 0
        for (k in 1:p) {
            yk <- max(AU[,k]) - min(AL[, k])
            Uijk <- abs(max(AU[i, k], BU[j, k]) - min(AL[i, k], BL[j, k]))
            Iijk <- max(min(AU[i, k], BU[j, k]) - max(AL[i, k], BL[j, k]), 0)
            
            d1 <- abs(AL[i, k] - BL[j, k]) / yk
            d2 <- abs(abs(AU[i, k] - AL[i, k]) - abs(BU[j, k] - BL[j, k])) / Uijk
            d3 <- abs(abs(AU[i, k] - AL[i, k]) + abs(BU[j, k] - BL[j, k]) - 2 * Iijk) / Uijk
            d <- d + d1 + d2 + d3
        }
        d
    }
    
    ichino_yaguchi <- function(i, j, p, AL, AU, BL, BU, g=0.25) {
        d <- 0
        for (k in 1:p) {
            Uijk <- abs(max(AU[i, k], BU[j, k]) - min(AL[i, k], BL[j, k]))
            Iijk <- max(min(AU[i, k], BU[j, k]) - max(AL[i, k], BL[j, k]), 0)
            
            d <- d + (Uijk - Iijk) - g * (abs(AU[i, k] - AL[i, k]) + abs(BU[j, k] - BL[j, k]) - 2 * Iijk)
        }
        d
    }
    
    N_A = nrow(A)
    p_A = ncol(A)
    N_B = nrow(B)
    p_B = ncol(B)
    
    if (p_A != p_B) {
        error("p_A != p_B")
    }
    
    # Obtain [max, min] intervals
    AL <- A@MidP - exp(A@LogR)/2
    AU <- A@MidP + exp(A@LogR)/2
    
    BL <- B@MidP - exp(B@LogR)/2
    BU <- B@MidP + exp(B@LogR)/2
    
    distances <- matrix(0, nrow = N_A, ncol = N_B)
    rownames(distances) <- rownames(A)
    colnames(distances) <- rownames(B)
    
    for (i in 1:N_A) {
        for (j in 1:N_B) {
            d <- switch (distance,
                         "hausdorff" = hausdorff(i, j, p_A, AL, AU, BL, BU),
                         "euclidean_hausdorff" = euclidean_hausdorff(i, j, p_A, AL, AU, BL, BU),
                         "gowda_diday" = gowda_diday(i, j, p_A, AL, AU, BL, BU),
                         "ichino_yaguchi" = ichino_yaguchi(i, j, p_A, AL, AU, BL, BU, g=0.25),
            )
            
            distances[i, j] <- d
        }
    }
    distances
}



interval_distance_functional <- function(A, B, distance="hausdorff") {
    d <- NULL
    if (length(A) != length(B)) {
        error("length(A) != length(B)")
    }
    n_days <- length(A)
    
    for (i in 1:n_days) {
        AA <- A[[i]]
        BB <- B[[i]]
        if (is.null(d)) {
            d <- interval_distance(AA, BB, distance)
        } else {
            d <- d + interval_distance(AA, BB, distance)
        }
    }
    
    return(d)
}
