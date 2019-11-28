# Based on: https://www.kaggle.com/olivermadsen/fast-and-stable-quadratic-kappa-r

library(compiler)
library(Matrix)

quadratic_kappa <- cmpfun(function(observed, predicted, levels = 0L:3L){
    n <- length(levels)
    if(length(observed) != length(predicted)) {
        stop('observed and predicted length does not match!')
    }
    if(!is.factor(observed)) {
        observed  <- factor(observed, levels = levels)
    }
    if(!is.factor(predicted)) {
        predicted <- factor(predicted, levels = levels)
    }
    
    # Generate weight matrix
    i   <- i2 <- rep(1L:n, each = n)
    j   <- j2 <- rep(1L:n, n)
    ind <- j > i
    
    # Remove unnecessary elements
    i <- i[ind]
    j <- j[ind]
    
    # Calculate weight matrix
    w <- sparseMatrix(i, j, , (i - j)^2/(n-1)^2, c(n, n), symmetric = TRUE)
    O <- table(observed, predicted)
    
    # Generate histogram values
    observed  <- table(observed)
    predicted <- table(predicted)
    
    # Calculate expected values
    Omean <- mean(observed[i2])
    Pmean <- mean(predicted[j2])
    Esum  <- Omean * Pmean
    
    # Calculate expeccted probability matrix
    E <- sparseMatrix(i2, j2, , as.numeric((predicted[j2] / Esum) * (observed[i2] * as.numeric(w)) / n^2), c(n, n))
    1 - (sum(O * w) / (sum(observed) * sum(E)))
})
