qic <- function(object, digits = 3) {
  independence_model <- update(object, LORstr = "independence")
  independence_naive_covariance <- vcov(independence_model, robust = FALSE)
  robust_covariance <- vcov(object, robust = TRUE)
  fitted_props <- fitted(object)
  y_vector <- object$y
  ncategories <- max(y_vector)
  y_binary <- rep(y_vector, each = ncategories)
  intercept <- rep.int(seq(ncategories), length(object$id))
  y_binary <- as.numeric(y_binary == intercept)
  y_binary <- matrix(y_binary, nrow = nrow(fitted_props), ncol = ncategories,
                     byrow = TRUE)
  quasi_likelihood <- sum(y_binary * log(fitted_props))
  qic_u <- -2 * quasi_likelihood + 2 * length(object$coeff)
  cic <- sum(solve(independence_naive_covariance) * robust_covariance)
  qic <- -2 * quasi_likelihood + 2 * cic
  #q_matrix <- solve(vcov(object, robust = FALSE)) %*% robust_covariance 
  #c1 <-  sum(diag(q_matrix)) / length(object$coeff)
  #c2 <- sum(q_matrix^2) / length(object$coeff)
  #rjc <- sqrt(((1 - c1) ^ 2) + ((1 - c2) ^ 2))
  list(
    qic = round(qic, digits = digits),
    qic_u = round(qic_u, digits = digits),
    cic = round(cic, digits = digits),
    quasi_likelihood = round(quasi_likelihood, digits = digits)
   #rjc = round(rjc, digits = digits)
    )
}
