model_selection_criteria <- function(object, digits = 3) {
  independence_model <- update(object, LORstr = "independence")
  naive_covariance <- vcov(independence_model, robust = FALSE)
  robust_covariance <- vcov(object, robust = TRUE)
  fitted_props <- fitted(object)
  y_vector <- object$y
  ncategories <- max(y_vector)
  y_binary <- rep(y_vector, each = ncategories)
  intercept <- rep.int(seq(ncategories), length(object$id))
  y_binary <- as.numeric(y_binary == intercept)
  y_binary <- matrix(y_binary, nrow = nrow(fitted_props),
                     ncol = ncol(fitted_props), byrow = TRUE)
  qlike <- sum(y_binary * log(fitted_props))
  qicu <- -2 * qlike + 2 * length(object$coeff)
  qic <- -2 * qlike + 2 * sum(solve(naive_covariance) * robust_covariance)
  q_matrix <- solve(vcov(object, robust = FALSE)) %*% robust_covariance 
  c1 <-  sum(diag(q_matrix)) / length(object$coeff)
  c2 <- sum(q_matrix^2) / length(object$coeff)
  rjc <- sqrt(((1 - c1) ^ 2) + ((1 - c2) ^ 2))
  list(
    QLike = round(qlike, digits = digits),
    QIC = round(qic,digits = digits),
    QICu = round(qicu, digits = digits),
    rjc = round(rjc, digits = digits)
    )
}
