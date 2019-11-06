rjc <- function(object, digits = 3) {
  naive_covariance <- vcov(object, robust = FALSE)
  robust_covariance <- vcov(object, robust = TRUE)
  q_matrix <- solve(naive_covariance) %*% robust_covariance
  p <- length(object$coeff)
  c1 <- sum(diag(q_matrix)) / p
  c2 <- sum(q_matrix ^ 2) / p
  rjc <- sqrt(((1 - c1) ^ 2) + ((1 - c2) ^ 2))
  round(rjc, digits)
}
