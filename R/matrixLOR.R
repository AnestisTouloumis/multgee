matrixLOR <- function(x) {
    if (!is.matrix(x)) 
        stop("'x' must be a matrix")
    if (nrow(x) != ncol(x)) 
        stop("'x' must be a square matrix")
    if (any(x < 0)) 
        stop("all elements of 'x' must be > 0")
    y <- matrix(1, nrow(x) + 1, ncol(x) + 1)
    for (i in seq_len(ncol(x))) {
        for (j in seq_len(ncol(x))) y[i + 1, j + 1] <- prod(x[1:i, 1:j])
    }
    prop.table(y)
}

