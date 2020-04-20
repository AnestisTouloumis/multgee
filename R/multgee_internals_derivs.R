derivacl <- function(fitprob, ncategoriesm1, X_mat) { # nolint
  n1 <- nrow(X_mat)
  n2 <- ncol(X_mat)
  z1 <- n1 / ncategoriesm1
  fitprob1 <- matrix(fitprob, z1, ncategoriesm1, TRUE)
  cumprob1 <- t(apply(fitprob1, 1, cumsum))
  mat1 <- matrix(fitprob, n1, n2)
  mat4 <- matrix(0, n1, n2, TRUE)
  for (i in seq_len(nrow(fitprob1))) {
    mat2 <- matrix(
      1 - cumprob1[i, ], ncategoriesm1, ncategoriesm1,
      TRUE
    )
    mat3 <- 1 - mat2
    mat2[lower.tri(mat2)] <- -mat3[lower.tri(mat3)]
    mat4[
      ((i - 1) * ncategoriesm1 + 1):(i * ncategoriesm1),
      1:ncategoriesm1
    ] <- mat2
  }
  if (n2 > ncategoriesm1) {
    dummy2 <- (ncategoriesm1 + 1):n2
    if (length(dummy2) == 1) {
      mat2 <- X_mat[, dummy2] * mat1[, dummy2]
      for (i in seq_len(nrow(fitprob1))) {
        dummy <- ((i - 1) * ncategoriesm1 + 1):(i * ncategoriesm1)
        mat4[dummy, dummy2] <- X_mat[dummy, dummy2] -
          rep(sum(mat2[dummy]), ncategoriesm1)
      }
    } else {
      dummy3 <- n2 - ncategoriesm1
      mat2 <- X_mat[, dummy2] * mat1[, dummy2]
      for (i in seq_len(nrow(fitprob1))) {
        dummy <- ((i - 1) * ncategoriesm1 + 1):(i * ncategoriesm1)
        mat3 <- .colSums(
          mat2[dummy, ], ncategoriesm1, dummy3,
          FALSE
        )
        mat3 <- X_mat[dummy, dummy2] - matrix(
          mat3, ncategoriesm1,
          dummy3, TRUE
        )
        mat4[dummy, (ncategoriesm1 + 1):n2] <- mat3
      }
    }
  }
  mat4 * mat1
}




derivbcl <- function(fitprob, ncategoriesm1, X_mat) { # nolint
  n1 <- nrow(X_mat)
  n2 <- ncol(X_mat)
  fitprob1 <- matrix(fitprob, n1 / ncategoriesm1, ncategoriesm1, TRUE)
  mat1 <- matrix(0, n1, ncategoriesm1)
  for (i in seq_len(nrow(fitprob1))) {
    mat1[((i - 1) * ncategoriesm1 + 1):(i * ncategoriesm1), ] <-
      diagmod(fitprob1[i, ]) - tcrossprod(fitprob1[i, ])
  }
  if (n2 == ncategoriesm1) {
    ans <- mat1
  } else {
    mat1 <- t(apply(mat1, 1, function(x) rep(x, each = n2 / ncategoriesm1)))
    mat2 <- X_mat[seq(1, n1, by = ncategoriesm1), 1:(n2 / ncategoriesm1)]
    mat2 <- t(apply(mat2, 1, function(x) rep(x, ncategoriesm1)))
    mat2 <- apply(mat2, 2, function(x) rep(x, each = ncategoriesm1))
    ans <- mat1 * mat2
  }
  ans
}

derivmclm <- function(mueta, ncategoriesm1, X_mat) { # nolint
  nobs <- nrow(X_mat) / ncategoriesm1
  ans <- diagmod(rep.int(1, ncategoriesm1))
  ans[seq(2, ncategoriesm1^2, ncategoriesm1 + 1)] <- -1
  ans <- apply(ans, 2, function(x) rep.int(x, nobs))
  mat1 <- matrix(mueta, nobs, ncategoriesm1, TRUE)
  mat1 <- apply(mat1, 2, function(x) rep(x, each = ncategoriesm1))
  mat1 <- ans * mat1
  mat2 <- .rowSums(mat1, nrow(mat1), ncol(mat1), FALSE) *
    X_mat[, -c(1:ncategoriesm1)]
  mat2 <- cbind(mat1, mat2)
  mat2
}
