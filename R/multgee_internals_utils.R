combns <- function(x) {
  ans <- t(utils::combn(seq(x), 2))
  ans <- cbind(ans, seq.int(nrow(ans)))
  ans
}



diagmod <- function(x) {
  x_dims <- length(x)
  ans <- matrix(0, x_dims, x_dims)
  ans[1 + 0:(x_dims - 1) * (x_dims + 1)] <- x
  ans
}



format_perc <- function(probs, digits) {
  paste(
    format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
    "%"
    )
}



inversemat <- function(x, inverse_method) { # nolint
  switch(
    inverse_method,
    cholesky = chol2inv(chol(x)),
    solve = solve(x, diagmod(rep.int(1, nrow(x)))),
    qr.solve = qr.solve(x, diagmod(rep.int(1, nrow(x))))
    )
}




ipfp <- function(initial, rowmars, colmars, dimension,
                  maxit = ipfp.control()$maxit, tol = ipfp.control()$tol) {
  ans <- initial
  rowsums <- .rowSums(ans, dimension, dimension, FALSE)
  for (i in 1:maxit) {
    ans <- ans * rep.int(rowmars / rowsums, dimension)
    colsums <- .colSums(ans, dimension, dimension, FALSE)
    if (all(abs(colsums - colmars) <= tol)) {
      break
    }
    ans <- ans * rep(colmars / colsums, each = dimension)
    rowsums <- .rowSums(ans, dimension, dimension, FALSE)
    if (all(abs(rowsums - rowmars) <= tol)) {
      break
    }
  }
  matrix(ans, dimension, dimension)
}




muprob <- function(cumprob, nobs, ncategoriesm1) {
  ans <- matrix(cumprob, nobs, ncategoriesm1, TRUE)
  ans <- rbind(ans[, 1], diff(t(ans)), 1 - ans[, ncategoriesm1])
  ans <- c(ans)
  ans
}



normscores <- function(x) {
  (x - mean(x)) / sqrt(sum((x - mean(x))^2))
}



odds_ratio <- function(x) { # nolint
  x_dims <- nrow(x)
  x[-1, -1] * x[-x_dims, -x_dims] / x[-x_dims, -1] / x[-1, -x_dims]
}
