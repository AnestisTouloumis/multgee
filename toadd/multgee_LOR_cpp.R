matrixLOR_cpp <- function(x) {
  if (!is.matrix(x)) {
    stop("'x' must be a matrix")
    } else {
      if (nrow(x) != ncol(x)) {
        stop("'x' must be a square matrix")
        }
      x_cols <- ncol(x)
      if (any(x <= 0)) {
        stop("all elements of 'x' must be > 0")
      }
    }
  matrix_LOR_cpp(x, x_cols)
}
